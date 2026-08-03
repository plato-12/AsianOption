// Utility-indifference Bellman engine for Asian options under price impact.
//
// Implements the discrete CARA dealer problem. State is (s, i, q, j, a):
//
//   s'_xi   = s * exp[(mu(t_m) + lambda_I*i - sigma^2/2)*dt + sigma*sqrt(dt)*xi]
//   i'_zeta = i - kappa_I*i*dt + eta(t_m)*sqrt(dt)*zeta
//   q'      = q + nu*dt
//   j'      = j + (-kappa_J*j + nu)*dt
//   a'      = a + h(s)*dt          (continuous monitoring)
//   a'      = a + w_m*h(s')        (discrete monitoring, w_m = 0 off a fixing)
//   g       = (s + lambda_bar_P*q + lambda_bar_T*j)*nu
//               + k_A*(nu^+)^(1+psi) + k_B*(nu^-)^(1+psi)
//
//   v^theta_N = q*s - ell_1*|q| - (Gamma_Q/2)q^2 - theta*n_opt*Phi(a)
//   v^theta_m = max_{nu in U_m} { -beta_m*delta_r*g
//                 - (1/gamma) log sum_{xi,zeta} p_{xi,zeta}
//                     exp(-gamma * Interp[v^theta_{m+1}](s', i', q', j', a')) }
//
// with the admissible set
//
//   U_m(s,i,q,j) = { nu : |q'| <= Q_bar,  s + lambda_bar_P*q + lambda_bar_T*j
//                          >= eps,  s'_xi + lambda_bar_P*q' + lambda_bar_T*j'
//                          >= eps for xi in {-1,+1} }.
//
// The second condition is forward-looking: a trade is admissible only if the
// execution price still clears eps after one step, along both price branches.
// There is no terminal charge on j; the dealer's transient impact enters only
// through the pre-expiry execution costs.
//
// Two interchangeable code paths compute the same recursion:
//   engine_mode = 1 ("reference")  straight transcription, one 5-linear
//                                  interpolation per (node, control, branch);
//   engine_mode = 0 ("cached")     precomputed bracket tables plus a plane-wise
//                                  partial collapse of the value array,
//                                  parallelised over planes with RcppParallel.
// They perform the same floating-point operations in the same order and agree
// to a few ulps; tests/testthat/test-indiff-engine.R checks this.
//
// Three discretisation choices keep the scheme accurate; each is documented at
// the point of use and each is switchable so its effect can be measured:
//   * the log-price grid moves with the deterministic drift, so the drift never
//     has to be interpolated (grid_drift);
//   * the grid cell width is set to one shock sigma*sqrt(dt), so the price
//     transition lands exactly on a node (grid alignment);
//   * the accumulator uses trapezoidal quadrature, whose variance error is
//     O(dt^2) against O(dt) for the left-endpoint rule (accum_rule).

// RcppParallel.h must precede Rcpp.h: it pulls in system headers that define
// TRUE/FALSE as macros and undefines them again on the way out, which would
// otherwise clash with R_ext/Boolean.h.
#include <RcppParallel.h>

#include "indiff_bellman.h"
#include <Rcpp.h>
#include <vector>
#include <limits>
#include <cstddef>
#include <algorithm>

// ---------------------------------------------------------------------------
// Exported scalar helpers (used by the R-level unit tests)
// ---------------------------------------------------------------------------

//' Branch probabilities of the four-point shock scheme
//'
//' @param rho Correlation between the price and impact shocks, in [-1, 1].
//' @return Numeric vector of length 4 holding p_(+,+), p_(+,-), p_(-,+), p_(-,-).
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericVector indiff_branch_probs_cpp(double rho) {
  if (!(rho >= -1.0 && rho <= 1.0)) {
    Rcpp::stop("rho must be in [-1, 1]");
  }
  double p[4];
  indiff_branch_probs(rho, p);
  return Rcpp::NumericVector::create(p[0], p[1], p[2], p[3]);
}

//' Cash-discount helpers of the self-financing recursion
//'
//' @param r_cont Continuously compounded rate.
//' @param T_mat Maturity.
//' @param N Number of time steps.
//' @return List with \code{dt}, \code{delta_r}, \code{beta} (length N+1) and
//'   \code{beta_delta} (length N).
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List indiff_discount_helpers_cpp(double r_cont, double T_mat, int N) {
  if (N <= 0) Rcpp::stop("N must be a positive integer");
  if (T_mat <= 0.0) Rcpp::stop("T (maturity) must be positive");

  double dt = T_mat / N;
  double delta_r = indiff_delta_r(r_cont, dt);

  Rcpp::NumericVector beta(N + 1), beta_delta(N);
  for (int m = 0; m <= N; m++) beta[m] = indiff_beta(r_cont, T_mat, m * dt);
  for (int m = 0; m < N; m++) beta_delta[m] = beta[m] * delta_r;

  return Rcpp::List::create(
    Rcpp::Named("dt") = dt,
    Rcpp::Named("delta_r") = delta_r,
    Rcpp::Named("beta") = beta,
    Rcpp::Named("beta_delta") = beta_delta
  );
}

//' Terminal Asian payoff used by the indifference engine
//'
//' @param a Running accumulator value.
//' @param S0 Initial price.
//' @param K Strike.
//' @param T_mat Maturity.
//' @param asian_type 0 = arithmetic, 1 = geometric.
//' @param option_type 0 = call, 1 = put.
//' @param phi_cap Payoff cap; values <= 0 mean no cap.
//' @return The (possibly capped) payoff.
//' @keywords internal
// [[Rcpp::export]]
double indiff_payoff_cpp(double a, double S0, double K, double T_mat,
                         int asian_type, int option_type, double phi_cap) {
  return indiff_payoff(a, S0, K, T_mat, asian_type, option_type, phi_cap);
}

//' Report the RcppParallel backend used by the indifference engine
//'
//' @return \code{"tbb"} when the Intel TBB backend is active, or
//'   \code{"tinythread"} when RcppParallel falls back to its portable
//'   thread pool. \code{"serial"} is reported inside a \code{fork()}ed child
//'   process, where TBB cannot be used and the engine runs single-threaded.
//' @keywords internal
// [[Rcpp::export]]
std::string indiff_parallel_backend_cpp() {
  if (RcppParallel::isProcessForkedChild()) return "serial";
  return RcppParallel::internal::backendToString(RcppParallel::internal::backend());
}

// ---------------------------------------------------------------------------
// Shared state-transition maps.  Both code paths call these, so the coordinate
// maps have a single definition.
// ---------------------------------------------------------------------------

// Log-price successor expressed in grid-offset coordinates.  The log-price grid
// at step m is centred on logS0 + shift[m], where
//   shift[m+1] = shift[m] + (mu_m - sigma^2/2) dt,
// so the deterministic part of the drift is a change of origin rather than a
// displacement to be interpolated.  Only the impact-driven part lambda_I*i*dt
// and the shock remain, and with an aligned grid the shock is an exact number
// of cells.  With lambda_I = 0 the price lattice is then exactly recombining.
static inline double step_logS_off(double off, double i_val, double lambda_I,
                                   double sigma, double dt, double sqdt,
                                   double xi) {
  return off + lambda_I * i_val * dt + sigma * sqdt * xi;
}

static inline double step_I(double i_val, double kappa_I, double dt,
                            double eta_m, double sqdt, double zeta) {
  return i_val - kappa_I * i_val * dt + eta_m * sqdt * zeta;
}

static inline double step_Q(double q, double nu, double dt) {
  return q + nu * dt;
}

static inline double step_J(double j, double nu, double kappa_J, double dt) {
  return j + (-kappa_J * j + nu) * dt;
}

static inline double accum_h(double logs, double logS0, int asian_type) {
  return (asian_type == 0) ? std::exp(logs - logS0) : (logs - logS0);
}

// Accumulator successor.
//
// Under continuous monitoring (monitor_mode 0) the accumulator discretises
// a_T = int_0^T h(S_u) du: accum_rule 0 is the left-endpoint rule, rule 1 the
// trapezoid rule, which misstates Var(a_T) by O(dt^2) instead of O(dt).  That
// matters because the payoff sees a only through its distribution.
//
// Under discrete monitoring (monitor_mode 1) the accumulator instead jumps only
// at the contractual fixing dates: a picks up w_m*h(S_{t_{m+1}}) where w_m is
// zero unless t_{m+1} is a fixing.  With M equally spaced fixings and
// w_m = T/M the payoff normalisation is unchanged -- S0*a/T is the mean of the
// M fixings and S0*exp(a/T) their geometric mean -- so indiff_payoff needs no
// discrete-monitoring branch.
//
// logs_next must already be clamped to the successor grid so that the
// accumulator bound still holds.
static inline double accum_next(double a, double logs, double logs_next,
                                double logS0, int asian_type, int accum_rule,
                                int monitor_mode, double fix_w_m, double dt) {
  if (monitor_mode == 1) {
    if (fix_w_m == 0.0) return a;
    return a + fix_w_m * accum_h(logs_next, logS0, asian_type);
  }
  double h0 = accum_h(logs, logS0, asian_type);
  if (accum_rule == 0) return a + h0 * dt;
  double h1 = accum_h(logs_next, logS0, asian_type);
  return a + 0.5 * (h0 + h1) * dt;
}

// Certainty equivalent of the four branch continuation values, in the
// stabilized log-sum-exp form of design.md Section 2.
static inline double branch_ce(const double* vb, const double* p,
                               double gamma_ra) {
  if (gamma_ra < 1e-10) {
    return p[0] * vb[0] + p[1] * vb[1] + p[2] * vb[2] + p[3] * vb[3];
  }
  double w[4];
  double M = -std::numeric_limits<double>::infinity();
  for (int b = 0; b < 4; b++) {
    if (p[b] > 0.0) {
      w[b] = -gamma_ra * vb[b];
      if (w[b] > M) M = w[b];
    }
  }
  // The branch (or branches) attaining M have w[b] - M == 0.0 exactly (same
  // bit pattern subtracted from itself), so exp(w[b] - M) == 1.0 exactly:
  // skip the call rather than compute it.
  double acc = 0.0;
  for (int b = 0; b < 4; b++) {
    if (p[b] <= 0.0) continue;
    acc += (w[b] == M) ? p[b] : p[b] * std::exp(w[b] - M);
  }
  return -(M + std::log(acc)) / gamma_ra;
}

// Running execution cost k_A*(nu^+)^(1+psi) + k_B*(nu^-)^(1+psi).
static inline double exec_cost(double nu, double k_A, double k_B, double psi) {
  double c = 0.0;
  if (nu > 0.0) c += k_A * std::pow(nu, 1.0 + psi);
  if (nu < 0.0) c += k_B * std::pow(-nu, 1.0 + psi);
  return c;
}

// ---------------------------------------------------------------------------
// Model specification and grids
// ---------------------------------------------------------------------------

struct IndiffSpec {
  double S0, K, T_mat;
  int N;
  double sigma, r_cont;
  double lambda_I, kappa_I, rho;
  double lambda_bar_T, lambda_bar_P, kappa_J;
  double k_A, k_B, psi_cost;
  double gamma_ra, Gamma_Q, ell_1;
  double Q_bar, phi_cap, n_opt;
  double eps_exec;      // execution-price floor of the admissible set
  double I0, Q0, J0;
  int asian_type, option_type, theta;
  int accum_rule;       // 0 = left endpoint, 1 = trapezoid
  int monitor_mode;     // 0 = continuous, 1 = discrete fixing dates
  double accum_sd;      // accumulator half-width in sd of the running average
  int grid_drift;       // 1 = log-price grid tracks the deterministic drift
};

struct IndiffGrids {
  std::vector<double> off;     // log-price grid offsets, [-margin, margin]
  std::vector<double> shift;   // length N+1 grid origin displacement
  std::vector<double> I, Q, J, R;
  double logS0;
  double off_lo, off_dx;
  double I_lo, I_dx;
  double Q_lo, Q_dx;
  double J_lo, J_dx;
  double R_lo, R_dx;
  int n_logS, n_I, n_Q, n_J, n_R;
  double margin, I_bound, J_bound;
  int shock_cells;             // sigma*sqrt(dt) in log-price cells
  bool aligned;
  bool collapsed_I;            // I dimension dropped because lambda_I == 0

  size_t blk() const { return static_cast<size_t>(n_Q) * n_J * n_R; }
  size_t size() const { return static_cast<size_t>(n_logS) * n_I * blk(); }
  size_t idx(int is, int ii, int iq, int ij, int ia) const {
    return ((((static_cast<size_t>(is) * n_I + ii) * n_Q + iq) * n_J + ij)
            * n_R + ia);
  }
  // Absolute log price of node is at step m.
  double logS_at(int m, int is) const {
    return logS0 + shift[static_cast<size_t>(m)] + off[static_cast<size_t>(is)];
  }
};

// 5-linear interpolation, collapsing (s, i), then a, then (q, j).  The cached
// path performs exactly these operations in exactly this order, one collapse
// stage at a time.
static double interp5(const double* V, const IndiffGrids& G,
                      const IndiffBracket& bs, const IndiffBracket& bi,
                      const IndiffBracket& bq, const IndiffBracket& bj,
                      const IndiffBracket& ba) {
  double ws = bs.w, wi = bi.w;
  double w00 = (1.0 - ws) * (1.0 - wi);
  double w01 = (1.0 - ws) * wi;
  double w10 = ws * (1.0 - wi);
  double w11 = ws * wi;

  const double* V00 = V + G.idx(bs.lo,     bi.lo,     0, 0, 0);
  const double* V01 = V + G.idx(bs.lo,     bi.lo + 1, 0, 0, 0);
  const double* V10 = V + G.idx(bs.lo + 1, bi.lo,     0, 0, 0);
  const double* V11 = V + G.idx(bs.lo + 1, bi.lo + 1, 0, 0, 0);

  double Pc[2][2][2];
  for (int dq = 0; dq < 2; dq++) {
    for (int dj = 0; dj < 2; dj++) {
      for (int da = 0; da < 2; da++) {
        size_t o = (static_cast<size_t>(bq.lo + dq) * G.n_J + (bj.lo + dj))
                     * G.n_R + (ba.lo + da);
        Pc[dq][dj][da] = w00 * V00[o] + w01 * V01[o]
                       + w10 * V10[o] + w11 * V11[o];
      }
    }
  }

  double wa = ba.w;
  double Qc[2][2];
  for (int dq = 0; dq < 2; dq++) {
    for (int dj = 0; dj < 2; dj++) {
      Qc[dq][dj] = (1.0 - wa) * Pc[dq][dj][0] + wa * Pc[dq][dj][1];
    }
  }

  double wq = bq.w, wj = bj.w;
  double u00 = (1.0 - wq) * (1.0 - wj);
  double u01 = (1.0 - wq) * wj;
  double u10 = wq * (1.0 - wj);
  double u11 = wq * wj;
  return u00 * Qc[0][0] + u01 * Qc[0][1] + u10 * Qc[1][0] + u11 * Qc[1][1];
}

static IndiffGrids build_grids(const IndiffSpec& S,
                               const std::vector<double>& mu_vec,
                               const std::vector<double>& eta_vec,
                               const std::vector<double>& fix_w,
                               const std::vector<double>& controls,
                               int n_logS, int n_I, int n_Q, int n_J, int n_R) {
  IndiffGrids G;
  G.logS0 = std::log(S.S0);

  // When lambda_I == 0 the exogenous impact state enters neither the price
  // drift, nor g, nor the payoff, so the value function is exactly constant in
  // I.  Collapsing that dimension to two nodes is therefore exact, and it cuts
  // the state space by a large factor in the common lambda_I = 0 case.
  G.collapsed_I = false;
  if (S.lambda_I == 0.0 && n_I > 2) { n_I = 2; G.collapsed_I = true; }

  G.n_I = n_I; G.n_Q = n_Q; G.n_J = n_J;

  double eta_max = 0.0, mu_absmax = 0.0;
  for (size_t m = 0; m < eta_vec.size(); m++)
    eta_max = std::max(eta_max, std::abs(eta_vec[m]));
  for (size_t m = 0; m < mu_vec.size(); m++)
    mu_absmax = std::max(mu_absmax, std::abs(mu_vec[m]));

  double nu_bar = 0.0;
  for (size_t c = 0; c < controls.size(); c++)
    nu_bar = std::max(nu_bar, std::abs(controls[c]));

  // I is exogenous here: no control term, unlike the legacy HJB engine.
  double I_bound = std::abs(S.I0);
  if (S.kappa_I > 1e-12) {
    I_bound += 4.0 * eta_max / std::sqrt(2.0 * S.kappa_I);
  } else {
    I_bound += 4.0 * eta_max * std::sqrt(S.T_mat);
  }
  if (I_bound < 1.0) I_bound = 1.0;

  const double dt = S.T_mat / S.N;
  const double shock = S.sigma * std::sqrt(dt);

  // Grid origins.  With grid_drift the deterministic drift is absorbed into the
  // origin and the grid only has to span the stochastic spread; otherwise the
  // origin is fixed and the grid must also cover the drift excursion.
  G.shift.assign(static_cast<size_t>(S.N) + 1, 0.0);
  if (S.grid_drift == 1) {
    for (int m = 0; m < S.N; m++)
      G.shift[m + 1] = G.shift[m] + (mu_vec[m] - 0.5 * S.sigma * S.sigma) * dt;
  }

  double drift_span = (S.grid_drift == 1)
    ? std::abs(S.lambda_I) * I_bound * S.T_mat
    : (mu_absmax + std::abs(S.lambda_I) * I_bound) * S.T_mat;
  double margin = drift_span + 4.0 * S.sigma * std::sqrt(S.T_mat);
  if (margin < 1.0) margin = 1.0;

  // Align the grid with the size of one shock.  log s' = log s + drift +/-
  // sigma*sqrt(dt) is interpolated, so unless sigma*sqrt(dt) is an integer
  // number of cells every step spreads mass over two nodes and the scheme
  // acquires a large spurious diffusion.
  int n_half;
  if (n_logS <= 0) {
    n_half = static_cast<int>(std::ceil(margin / shock - 1e-12));
    if (n_half < 2) n_half = 2;
    n_logS = 2 * n_half + 1;
    margin = n_half * shock;
    G.shock_cells = 1;
    G.aligned = true;
  } else {
    if (n_logS % 2 == 0) n_logS += 1;      // keep the initial state on a node
    n_half = (n_logS - 1) / 2;
    int k = static_cast<int>(std::floor(n_half * shock / margin + 1e-12));
    if (k >= 1) {
      margin = n_half * shock / k;         // widen to the nearest aligned value
      G.shock_cells = k;
      G.aligned = true;
    } else {
      G.shock_cells = 0;
      G.aligned = false;
    }
  }
  G.n_logS = n_logS;

  // |j_t| <= |J0| + nu_bar * (1 - exp(-kappa_J t))/kappa_J
  //        <= |J0| + nu_bar * min(1/kappa_J, T).
  double J_bound = std::abs(S.J0);
  if (S.kappa_J > 1e-12) {
    J_bound += nu_bar * std::min(1.0 / S.kappa_J, S.T_mat);
  } else {
    J_bound += nu_bar * S.T_mat;
  }
  if (J_bound < 0.5) J_bound = 0.5;

  G.margin = margin; G.I_bound = I_bound; G.J_bound = J_bound;

  G.off = indiff_uniform_grid(-margin, margin, n_logS);
  G.I   = indiff_uniform_grid(-I_bound, I_bound, n_I);
  G.Q   = indiff_uniform_grid(-S.Q_bar, S.Q_bar, n_Q);
  G.J   = indiff_uniform_grid(-J_bound, J_bound, n_J);

  // Accumulator grid.  The reachable set is [0, exp(margin)*T] (arithmetic) or
  // [-margin*T, margin*T] (geometric), but that is far wider than the
  // distribution of a_T, and this dimension carries a numerical diffusion that
  // is linear in its cell width.  Sizing it by the standard deviation of the
  // running average buys resolution where the mass is; the reachable-set bound
  // is kept as a hard cap so the grid never widens beyond it.  Under the
  // exogenous dynamics (1/T) int (log S_u - log S0) du has standard deviation
  // sigma*sqrt(T/3) and mean bounded by (mu_max + |lambda_I|*I_bound)*T/2.
  //
  // Under discrete monitoring the average is over M fixings at t_k = kT/M
  // rather than over the whole path, and Var((1/M) sum_k W_{t_k}) =
  // T(M+1)(2M+1)/(6M^2), which exceeds the continuous T/3 and does so by a
  // factor of 3 at M = 1.  Using the continuous variance there would leave the
  // accumulator grid far too narrow, so the exact discrete factors are used.
  double var_fac = 1.0 / 3.0, mean_fac = 0.5;
  if (S.monitor_mode == 1) {
    double M = 0.0;
    for (size_t m = 0; m < fix_w.size(); m++) if (fix_w[m] != 0.0) M += 1.0;
    if (M > 0.0) {
      var_fac  = (M + 1.0) * (2.0 * M + 1.0) / (6.0 * M * M);
      mean_fac = (M + 1.0) / (2.0 * M);
    }
  }
  double drift_a = (mu_absmax + std::abs(S.lambda_I) * I_bound)
                     * S.T_mat * mean_fac;
  double sd_a = S.sigma * std::sqrt(S.T_mat * var_fac);
  double k_sd = (S.accum_sd > 0.0) ? S.accum_sd : 5.0;

  if (n_R % 2 == 0) n_R += 1;              // keep a = 0 on the grid
  G.n_R = n_R;

  double R_lo, R_hi;
  double reach = (mu_absmax + std::abs(S.lambda_I) * I_bound) * S.T_mat
               + 4.0 * S.sigma * std::sqrt(S.T_mat);
  if (reach < margin) reach = margin;
  if (S.asian_type == 0) {
    R_lo = 0.0;                            // a = int (S_u/S0) du >= 0
    R_hi = std::min(std::exp(reach) * S.T_mat,
                    S.T_mat * std::exp(drift_a + k_sd * sd_a));
    if (R_hi < S.T_mat) R_hi = S.T_mat;
  } else {
    double hw = std::min(reach * S.T_mat, (drift_a + k_sd * sd_a) * S.T_mat);
    if (hw <= 0.0) hw = reach * S.T_mat;
    R_lo = -hw;                            // a = int (log S_u - log S0) du
    R_hi =  hw;
  }
  G.R = indiff_uniform_grid(R_lo, R_hi, n_R);

  G.off_lo = G.off[0]; G.off_dx = (n_logS > 1) ? (G.off[1] - G.off[0]) : 1.0;
  G.I_lo   = G.I[0];   G.I_dx   = (n_I    > 1) ? (G.I[1]   - G.I[0])   : 1.0;
  G.Q_lo   = G.Q[0];   G.Q_dx   = (n_Q    > 1) ? (G.Q[1]   - G.Q[0])   : 1.0;
  G.J_lo   = G.J[0];   G.J_dx   = (n_J    > 1) ? (G.J[1]   - G.J[0])   : 1.0;
  G.R_lo   = G.R[0];   G.R_dx   = (n_R    > 1) ? (G.R[1]   - G.R[0])   : 1.0;

  return G;
}

static void fill_terminal(std::vector<double>& V, const IndiffSpec& S,
                          const IndiffGrids& G) {
  for (int is = 0; is < G.n_logS; is++) {
    double s = std::exp(G.logS_at(S.N, is));
    for (int ii = 0; ii < G.n_I; ii++) {
      for (int iq = 0; iq < G.n_Q; iq++) {
        double q = G.Q[iq];
        // Terminal liquidation charge L(q) = ell_1*|q| + (Gamma_Q/2)q^2.  There
        // is no charge on j: the dealer's transient impact enters only through
        // the pre-expiry execution costs, so the terminal layer is flat in j.
        double base = q * s - S.ell_1 * std::abs(q) - 0.5 * S.Gamma_Q * q * q;
        for (int ij = 0; ij < G.n_J; ij++) {
          for (int ia = 0; ia < G.n_R; ia++) {
            double phi = indiff_payoff(G.R[ia], S.S0, S.K, S.T_mat,
                                       S.asian_type, S.option_type, S.phi_cap);
            V[G.idx(is, ii, iq, ij, ia)] = base - S.theta * S.n_opt * phi;
          }
        }
      }
    }
  }
}

struct ClampCount {
  long long S, I, Q, J, R;
  long long S_tot, I_tot, Q_tot, J_tot, R_tot;
  ClampCount() : S(0), I(0), Q(0), J(0), R(0),
                 S_tot(0), I_tot(0), Q_tot(0), J_tot(0), R_tot(0) {}
};

struct StepTables {
  std::vector<IndiffBracket> brS;   // (is * n_I + ii) * 2 + xi_idx
  std::vector<IndiffBracket> brI;   // ii * 2 + zeta_idx
  std::vector<IndiffBracket> brA;   // ((is * n_I + ii) * n_R + ia) * 2 + xi_idx
  std::vector<IndiffBracket> brQ;   // iq * nc + c
  std::vector<IndiffBracket> brJ;   // ij * nc + c
  std::vector<char> feasQ;          // iq * nc + c : |q'| <= Q_bar
  std::vector<double> cost_c;       // per-control execution cost
  std::vector<double> Sval;         // exp(absolute log price) at this step
  std::vector<double> Snext;        // (is * n_I + ii) * 2 + xi_idx : s'_xi
  std::vector<double> Qn;           // iq * nc + c : q'
  std::vector<double> Jn;           // ij * nc + c : j'
};

static void build_step_tables(StepTables& tb, const IndiffSpec& S,
                              const IndiffGrids& G,
                              const std::vector<double>& controls,
                              int m, double eta_m, double fix_w_m,
                              double dt, double sqdt, ClampCount& cc) {
  const int nc = static_cast<int>(controls.size());

  tb.brS.resize(static_cast<size_t>(G.n_logS) * G.n_I * 2);
  tb.brI.resize(static_cast<size_t>(G.n_I) * 2);
  tb.brA.resize(static_cast<size_t>(G.n_logS) * G.n_I * G.n_R * 2);
  tb.brQ.resize(static_cast<size_t>(G.n_Q) * nc);
  tb.brJ.resize(static_cast<size_t>(G.n_J) * nc);
  tb.feasQ.resize(static_cast<size_t>(G.n_Q) * nc);
  tb.cost_c.resize(static_cast<size_t>(nc));
  tb.Sval.resize(static_cast<size_t>(G.n_logS));
  tb.Snext.resize(static_cast<size_t>(G.n_logS) * G.n_I * 2);
  tb.Qn.resize(static_cast<size_t>(G.n_Q) * nc);
  tb.Jn.resize(static_cast<size_t>(G.n_J) * nc);

  for (int c = 0; c < nc; c++)
    tb.cost_c[c] = exec_cost(controls[c], S.k_A, S.k_B, S.psi_cost);
  for (int is = 0; is < G.n_logS; is++)
    tb.Sval[is] = std::exp(G.logS_at(m, is));

  const double off_hi = G.off_lo + (G.n_logS - 1) * G.off_dx;

  for (int is = 0; is < G.n_logS; is++) {
    double logs_m = G.logS_at(m, is);
    for (int ii = 0; ii < G.n_I; ii++) {
      for (int xi_idx = 0; xi_idx < 2; xi_idx++) {
        double xi = (xi_idx == 0) ? 1.0 : -1.0;
        double offn = step_logS_off(G.off[is], G.I[ii], S.lambda_I,
                                    S.sigma, dt, sqdt, xi);
        IndiffBracket b = indiff_bracket(G.off_lo, G.off_dx, G.n_logS, offn);
        tb.brS[(static_cast<size_t>(is) * G.n_I + ii) * 2 + xi_idx] = b;
        cc.S_tot++; if (b.clamped) cc.S++;

        // s'_xi for the forward admissibility test.  This is the analytic
        // successor of the model, so it is taken from the unclamped offset:
        // the constraint is a statement about the price process, not about
        // where the grid happens to end.
        tb.Snext[(static_cast<size_t>(is) * G.n_I + ii) * 2 + xi_idx] =
          std::exp(G.logS0 + G.shift[m + 1] + offn);

        // Successor log price, clamped to the successor grid, for the
        // trapezoidal and discrete-fixing accumulators.
        double offc = offn;
        if (offc < G.off_lo) offc = G.off_lo;
        if (offc > off_hi)   offc = off_hi;
        double logs_next = G.logS0 + G.shift[m + 1] + offc;

        for (int ia = 0; ia < G.n_R; ia++) {
          double an = accum_next(G.R[ia], logs_m, logs_next, G.logS0,
                                 S.asian_type, S.accum_rule, S.monitor_mode,
                                 fix_w_m, dt);
          IndiffBracket ba = indiff_bracket(G.R_lo, G.R_dx, G.n_R, an);
          tb.brA[((static_cast<size_t>(is) * G.n_I + ii) * G.n_R + ia) * 2
                 + xi_idx] = ba;
          cc.R_tot++; if (ba.clamped) cc.R++;
        }
      }
    }
  }

  for (int ii = 0; ii < G.n_I; ii++) {
    for (int z_idx = 0; z_idx < 2; z_idx++) {
      double zeta = (z_idx == 0) ? 1.0 : -1.0;
      double inx = step_I(G.I[ii], S.kappa_I, dt, eta_m, sqdt, zeta);
      IndiffBracket b = indiff_bracket(G.I_lo, G.I_dx, G.n_I, inx);
      tb.brI[static_cast<size_t>(ii) * 2 + z_idx] = b;
      cc.I_tot++; if (b.clamped) cc.I++;
    }
  }

  double q_tol = S.Q_bar * 1e-12 + 1e-12;
  for (int iq = 0; iq < G.n_Q; iq++) {
    for (int c = 0; c < nc; c++) {
      double qn = step_Q(G.Q[iq], controls[c], dt);
      size_t o = static_cast<size_t>(iq) * nc + c;
      tb.Qn[o] = qn;
      tb.feasQ[o] = (std::abs(qn) <= S.Q_bar + q_tol) ? 1 : 0;
      IndiffBracket b = indiff_bracket(G.Q_lo, G.Q_dx, G.n_Q, qn);
      tb.brQ[o] = b;
      if (tb.feasQ[o]) { cc.Q_tot++; if (b.clamped) cc.Q++; }
    }
  }

  for (int ij = 0; ij < G.n_J; ij++) {
    for (int c = 0; c < nc; c++) {
      double jn = step_J(G.J[ij], controls[c], S.kappa_J, dt);
      size_t o = static_cast<size_t>(ij) * nc + c;
      tb.Jn[o] = jn;
      IndiffBracket b = indiff_bracket(G.J_lo, G.J_dx, G.n_J, jn);
      tb.brJ[o] = b;
      cc.J_tot++; if (b.clamped) cc.J++;
    }
  }
}

// Candidate value of one control at one node in the cached layout.  Factored
// out so that the empty-admissible-set fallback evaluates nu = 0 through
// exactly the same arithmetic as the main maximisation loop.
static inline double cand_cached(const double* const* Qa, const double* p,
                                 const IndiffBracket& bq,
                                 const IndiffBracket& bj,
                                 double gt_c, int n_J, double gamma_ra) {
  double wq = bq.w, wj = bj.w;
  double u00 = (1.0 - wq) * (1.0 - wj);
  double u01 = (1.0 - wq) * wj;
  double u10 = wq * (1.0 - wj);
  double u11 = wq * wj;
  size_t o00 = static_cast<size_t>(bq.lo) * n_J + bj.lo;
  size_t o01 = o00 + 1;
  size_t o10 = o00 + n_J;
  size_t o11 = o10 + 1;

  double vb[4];
  for (int b = 0; b < 4; b++) {
    const double* qa = Qa[b];
    vb[b] = u00 * qa[o00] + u01 * qa[o01] + u10 * qa[o10] + u11 * qa[o11];
  }
  return gt_c + branch_ce(vb, p, gamma_ra);
}

// Cached backward step over a contiguous range of (log-price, impact) planes.
//
// Plane pl = is*n_I + ii writes only V_curr[idx(is, ii, ., ., .)], its own slot
// of infeas, and reads only V_next and the step tables, so the planes are
// independent: any partition of [0, n_planes) into ranges reproduces the serial
// result bit for bit.  The scratch buffers are local to the call, hence private
// to whichever thread runs the range.
static void backward_planes_cached(size_t pl_begin, size_t pl_end,
                                   std::vector<double>& V_curr,
                                   const std::vector<double>& V_next,
                                   std::vector<double>* policy,
                                   std::vector<long long>& infeas,
                                   const IndiffSpec& S, const IndiffGrids& G,
                                   const StepTables& tb,
                                   const std::vector<double>& controls,
                                   int c_zero, double cash) {
  const int nc = static_cast<int>(controls.size());
  const size_t blk = G.blk();
  const size_t nQJ = static_cast<size_t>(G.n_Q) * G.n_J;

  double p[4];
  indiff_branch_probs(S.rho, p);

  const double* Vn = V_next.data();
  double* Vc = V_curr.data();
  double* Pol = (policy != NULL) ? policy->data() : NULL;

  {
    std::vector<double> Pbuf(4 * blk);
    std::vector<double> Qbuf(4 * blk);
    std::vector<double> gterm(nQJ * nc);
    std::vector<char> price_ok(nQJ);

    for (size_t pl_u = pl_begin; pl_u < pl_end; pl_u++) {
      const int pl = static_cast<int>(pl_u);
      const int is = pl / G.n_I;
      const int ii = pl - is * G.n_I;
      const double s_val = tb.Sval[is];
      long long n_infeas_pl = 0;

      // s'_xi at this plane, for the forward leg of the admissibility test.
      // It does not depend on (q, j, nu), so it is hoisted out entirely.
      const double s_up = tb.Snext[(static_cast<size_t>(is) * G.n_I + ii) * 2];
      const double s_dn =
        tb.Snext[(static_cast<size_t>(is) * G.n_I + ii) * 2 + 1];

      // g(s, q, j, nu) and node admissibility do not depend on a or on the
      // branch, so they are hoisted above the (a, branch) loops.
      for (int iq = 0; iq < G.n_Q; iq++) {
        double q = G.Q[iq];
        for (int ij = 0; ij < G.n_J; ij++) {
          double j = G.J[ij];
          double px = s_val + S.lambda_bar_P * q + S.lambda_bar_T * j;
          size_t qj = static_cast<size_t>(iq) * G.n_J + ij;
          price_ok[qj] = (px >= S.eps_exec) ? 1 : 0;
          double* gt = &gterm[qj * nc];
          for (int c = 0; c < nc; c++)
            gt[c] = -cash * (px * controls[c] + tb.cost_c[c]);
        }
      }

      // Stage 1: collapse (s, i) for each branch.
      for (int b = 0; b < 4; b++) {
        int xi_idx = (b < 2) ? 0 : 1;
        int z_idx  = (b % 2 == 0) ? 0 : 1;
        const IndiffBracket& bs =
          tb.brS[(static_cast<size_t>(is) * G.n_I + ii) * 2 + xi_idx];
        const IndiffBracket& bi = tb.brI[static_cast<size_t>(ii) * 2 + z_idx];

        double ws = bs.w, wi = bi.w;
        double w00 = (1.0 - ws) * (1.0 - wi);
        double w01 = (1.0 - ws) * wi;
        double w10 = ws * (1.0 - wi);
        double w11 = ws * wi;

        const double* V00 = Vn + G.idx(bs.lo,     bi.lo,     0, 0, 0);
        const double* V01 = Vn + G.idx(bs.lo,     bi.lo + 1, 0, 0, 0);
        const double* V10 = Vn + G.idx(bs.lo + 1, bi.lo,     0, 0, 0);
        const double* V11 = Vn + G.idx(bs.lo + 1, bi.lo + 1, 0, 0, 0);
        double* P = &Pbuf[b * blk];
        for (size_t t = 0; t < blk; t++)
          P[t] = w00 * V00[t] + w01 * V01[t] + w10 * V10[t] + w11 * V11[t];
      }

      // Stage 2: collapse a, giving Qbuf[b][ia][iq'][ij'].
      for (int b = 0; b < 4; b++) {
        int xi_idx = (b < 2) ? 0 : 1;
        const IndiffBracket* brA_b =
          &tb.brA[((static_cast<size_t>(is) * G.n_I + ii) * G.n_R) * 2];
        const double* P = &Pbuf[b * blk];
        double* Qa = &Qbuf[b * blk];
        for (int ia = 0; ia < G.n_R; ia++) {
          const IndiffBracket& ba = brA_b[static_cast<size_t>(ia) * 2 + xi_idx];
          double wa = ba.w;
          int la = ba.lo;
          double* dst = Qa + static_cast<size_t>(ia) * nQJ;
          for (size_t qj = 0; qj < nQJ; qj++) {
            const double* pv = P + qj * G.n_R;
            dst[qj] = (1.0 - wa) * pv[la] + wa * pv[la + 1];
          }
        }
      }

      // Stage 3: (q, j) interpolation, certainty equivalent, maximisation.
      for (int ia = 0; ia < G.n_R; ia++) {
        const double* Qa[4];
        for (int b = 0; b < 4; b++)
          Qa[b] = &Qbuf[b * blk] + static_cast<size_t>(ia) * nQJ;

        for (int iq = 0; iq < G.n_Q; iq++) {
          const IndiffBracket* brq = &tb.brQ[static_cast<size_t>(iq) * nc];
          const char* fq = &tb.feasQ[static_cast<size_t>(iq) * nc];
          const double* qn = &tb.Qn[static_cast<size_t>(iq) * nc];

          for (int ij = 0; ij < G.n_J; ij++) {
            const IndiffBracket* brj = &tb.brJ[static_cast<size_t>(ij) * nc];
            const double* jn = &tb.Jn[static_cast<size_t>(ij) * nc];
            size_t qj = static_cast<size_t>(iq) * G.n_J + ij;
            const double* gt = &gterm[qj * nc];
            bool ok = (price_ok[qj] != 0);

            double best = -std::numeric_limits<double>::infinity();
            int best_c = c_zero;

            for (int c = 0; c < nc; c++) {
              if (!fq[c]) continue;
              // note_v2 (21): the execution price must clear eps now, and must
              // still clear it after one step along both price branches.
              if (!ok) continue;
              double pxn = S.lambda_bar_P * qn[c] + S.lambda_bar_T * jn[c];
              if (s_up + pxn < S.eps_exec) continue;
              if (s_dn + pxn < S.eps_exec) continue;

              double cand = cand_cached(Qa, p, brq[c], brj[c], gt[c],
                                        G.n_J, S.gamma_ra);
              if (cand > best) { best = cand; best_c = c; }
            }

            if (best == -std::numeric_limits<double>::infinity()) {
              // Unlike the v1 admissible set, the v2 one can be empty. The
              // dealer then stands still: nu = 0 executes nothing, so it needs
              // no execution price. Counted, not silently applied.
              best = cand_cached(Qa, p, brq[c_zero], brj[c_zero], gt[c_zero],
                                 G.n_J, S.gamma_ra);
              best_c = c_zero;
              n_infeas_pl++;
            }

            size_t id = G.idx(is, ii, iq, ij, ia);
            Vc[id] = best;
            if (Pol != NULL) Pol[id] = controls[best_c];
          }
        }
      }

      infeas[pl_u] = n_infeas_pl;
    }
  }
}

// RcppParallel worker wrapping the plane range above.  It touches no R data
// structures and calls no R API, so it is safe to run off the main thread; the
// progress report and interrupt check stay in the caller's loop.
namespace {

struct BackwardStepWorker : public RcppParallel::Worker {
  std::vector<double>& V_curr;
  const std::vector<double>& V_next;
  std::vector<double>* policy;
  std::vector<long long>& infeas;
  const IndiffSpec& S;
  const IndiffGrids& G;
  const StepTables& tb;
  const std::vector<double>& controls;
  int c_zero;
  double cash;

  BackwardStepWorker(std::vector<double>& V_curr_,
                     const std::vector<double>& V_next_,
                     std::vector<double>* policy_,
                     std::vector<long long>& infeas_,
                     const IndiffSpec& S_, const IndiffGrids& G_,
                     const StepTables& tb_,
                     const std::vector<double>& controls_,
                     int c_zero_, double cash_)
    : V_curr(V_curr_), V_next(V_next_), policy(policy_), infeas(infeas_),
      S(S_), G(G_), tb(tb_), controls(controls_), c_zero(c_zero_),
      cash(cash_) {}

  void operator()(std::size_t begin, std::size_t end) {
    backward_planes_cached(begin, end, V_curr, V_next, policy, infeas, S, G,
                           tb, controls, c_zero, cash);
  }
};

}  // namespace

// One backward step, cached path, parallelised over (log-price, impact) planes.
//
// n_threads <= 0 leaves the thread count to RcppParallel (its default, or
// RCPP_PARALLEL_NUM_THREADS / setThreadOptions()).  TBB does not survive
// fork(), so a forked child runs the whole range serially.
static void backward_step_cached(std::vector<double>& V_curr,
                                 const std::vector<double>& V_next,
                                 std::vector<double>* policy,
                                 std::vector<long long>& infeas,
                                 const IndiffSpec& S, const IndiffGrids& G,
                                 const StepTables& tb,
                                 const std::vector<double>& controls,
                                 int c_zero, double cash, int n_threads) {
  const size_t n_planes = static_cast<size_t>(G.n_logS) * G.n_I;

  if (n_threads == 1 || n_planes < 2 || RcppParallel::isProcessForkedChild()) {
    backward_planes_cached(0, n_planes, V_curr, V_next, policy, infeas, S, G,
                           tb, controls, c_zero, cash);
    return;
  }

  // Each worker call allocates ~8*blk doubles of scratch, so hand out chunks
  // rather than single planes: aim for a few chunks per thread, which keeps the
  // allocation amortised while leaving room for load balancing.
  const int threads = (n_threads > 0) ? n_threads : 8;
  size_t grain = n_planes / (static_cast<size_t>(threads) * 4);
  if (grain < 1) grain = 1;

  BackwardStepWorker worker(V_curr, V_next, policy, infeas, S, G, tb, controls,
                            c_zero, cash);
  RcppParallel::parallelFor(0, n_planes, worker, grain,
                            (n_threads > 0) ? n_threads : -1);
}

// Candidate value of one control at one node, reference layout.  As in the
// cached path this is factored out so the empty-admissible-set fallback runs
// the same arithmetic as the main maximisation loop.
static inline double cand_reference(const double* Vn, const IndiffSpec& S,
                                    const IndiffGrids& G, const double* p,
                                    int is, int m, double i_val,
                                    double logs_m, double a, double px,
                                    double nu, double qn, double jn,
                                    double cash, double eta_m, double fix_w_m,
                                    double dt, double sqdt, double off_hi) {
  IndiffBracket bq = indiff_bracket(G.Q_lo, G.Q_dx, G.n_Q, qn);
  IndiffBracket bj = indiff_bracket(G.J_lo, G.J_dx, G.n_J, jn);

  double vb[4];
  for (int b = 0; b < 4; b++) {
    double offn = step_logS_off(G.off[is], i_val, S.lambda_I,
                                S.sigma, dt, sqdt, indiff_xi_sign(b));
    double inx = step_I(i_val, S.kappa_I, dt, eta_m, sqdt,
                        indiff_zeta_sign(b));
    double offc = offn;
    if (offc < G.off_lo) offc = G.off_lo;
    if (offc > off_hi)   offc = off_hi;
    double logs_next = G.logS0 + G.shift[m + 1] + offc;
    double an = accum_next(a, logs_m, logs_next, G.logS0, S.asian_type,
                           S.accum_rule, S.monitor_mode, fix_w_m, dt);

    IndiffBracket bs = indiff_bracket(G.off_lo, G.off_dx, G.n_logS, offn);
    IndiffBracket bi = indiff_bracket(G.I_lo, G.I_dx, G.n_I, inx);
    IndiffBracket ba = indiff_bracket(G.R_lo, G.R_dx, G.n_R, an);
    vb[b] = interp5(Vn, G, bs, bi, bq, bj, ba);
  }

  double g = px * nu + exec_cost(nu, S.k_A, S.k_B, S.psi_cost);
  return -cash * g + branch_ce(vb, p, S.gamma_ra);
}

// One backward step, reference path: no caching, one 5-linear interpolation per
// (node, control, branch).  Serial by construction; used as the test oracle.
static void backward_step_reference(std::vector<double>& V_curr,
                                    const std::vector<double>& V_next,
                                    std::vector<double>* policy,
                                    long long& n_infeas,
                                    const IndiffSpec& S, const IndiffGrids& G,
                                    const std::vector<double>& controls,
                                    int c_zero, double cash,
                                    int m, double eta_m, double fix_w_m,
                                    double dt, double sqdt) {
  const int nc = static_cast<int>(controls.size());
  double p[4];
  indiff_branch_probs(S.rho, p);
  double q_tol = S.Q_bar * 1e-12 + 1e-12;
  const double off_hi = G.off_lo + (G.n_logS - 1) * G.off_dx;

  const double* Vn = V_next.data();

  for (int is = 0; is < G.n_logS; is++) {
    double logs_m = G.logS_at(m, is);
    double s_val = std::exp(logs_m);
    for (int ii = 0; ii < G.n_I; ii++) {
      double i_val = G.I[ii];

      // s'_xi for the forward leg of the admissibility test, from the
      // unclamped offset, exactly as the cached path builds tb.Snext.
      double s_up = std::exp(G.logS0 + G.shift[m + 1] +
        step_logS_off(G.off[is], i_val, S.lambda_I, S.sigma, dt, sqdt, 1.0));
      double s_dn = std::exp(G.logS0 + G.shift[m + 1] +
        step_logS_off(G.off[is], i_val, S.lambda_I, S.sigma, dt, sqdt, -1.0));

      for (int iq = 0; iq < G.n_Q; iq++) {
        double q = G.Q[iq];
        for (int ij = 0; ij < G.n_J; ij++) {
          double j = G.J[ij];
          double px = s_val + S.lambda_bar_P * q + S.lambda_bar_T * j;
          bool ok = (px >= S.eps_exec);
          for (int ia = 0; ia < G.n_R; ia++) {
            double a = G.R[ia];

            double best = -std::numeric_limits<double>::infinity();
            int best_c = c_zero;

            for (int c = 0; c < nc; c++) {
              double nu = controls[c];
              double qn = step_Q(q, nu, dt);
              if (std::abs(qn) > S.Q_bar + q_tol) continue;
              // note_v2 (21): the execution price must clear eps now, and must
              // still clear it after one step along both price branches.
              if (!ok) continue;
              double jn = step_J(j, nu, S.kappa_J, dt);
              double pxn = S.lambda_bar_P * qn + S.lambda_bar_T * jn;
              if (s_up + pxn < S.eps_exec) continue;
              if (s_dn + pxn < S.eps_exec) continue;

              double cand = cand_reference(Vn, S, G, p, is, m, i_val, logs_m,
                                           a, px, nu, qn, jn, cash, eta_m,
                                           fix_w_m, dt, sqdt, off_hi);
              if (cand > best) { best = cand; best_c = c; }
            }

            if (best == -std::numeric_limits<double>::infinity()) {
              // Unlike the v1 admissible set, the v2 one can be empty. The
              // dealer then stands still: nu = 0 executes nothing, so it needs
              // no execution price. Counted, not silently applied.
              double nu0 = controls[c_zero];
              best = cand_reference(Vn, S, G, p, is, m, i_val, logs_m, a, px,
                                    nu0, step_Q(q, nu0, dt),
                                    step_J(j, nu0, S.kappa_J, dt), cash,
                                    eta_m, fix_w_m, dt, sqdt, off_hi);
              best_c = c_zero;
              n_infeas++;
            }

            size_t id = G.idx(is, ii, iq, ij, ia);
            V_curr[id] = best;
            if (policy != NULL) (*policy)[id] = controls[best_c];
          }
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

//' Utility-indifference Bellman engine (single theta)
//'
//' Solves the CARA dealer problem of design.md Section 2 for one value of
//' \code{theta} and returns the value function at the initial state.
//'
//' @param S0,K,T_mat,N Contract and time-grid parameters.
//' @param mu_vec Length-N vector of drifts.
//' @param sigma,r_cont Volatility and continuously compounded rate.
//' @param lambda_I,kappa_I Exogenous impact loading and mean-reversion rate.
//' @param eta_vec Length-N vector of impact noise amplitudes.
//' @param rho Shock correlation.
//' @param lambda_bar_T,lambda_bar_P,kappa_J Dealer execution-impact parameters.
//' @param k_A,k_B,psi_cost Temporary execution cost parameters.
//' @param gamma_ra Absolute risk aversion.
//' @param Gamma_Q,ell_1 Quadratic and linear terminal liquidation charges on
//'   inventory. There is no terminal charge on the dealer impact state.
//' @param Q_bar Inventory bound.
//' @param eps_exec Execution-price floor of the admissible set.
//' @param phi_cap Payoff cap; values <= 0 mean no cap.
//' @param n_opt Option notional.
//' @param I0,Q0,J0 Initial exogenous impact, inventory and dealer impact state.
//' @param control_set Admissible trading rates; must contain 0.
//' @param n_logS,n_I,n_Q,n_J,n_R Grid sizes. \code{n_logS <= 0} asks the engine
//'   to size the log-price grid so that one shock spans exactly one cell.
//' @param asian_type 0 = arithmetic, 1 = geometric.
//' @param option_type 0 = call, 1 = put.
//' @param theta -1 (long), 0 (baseline) or +1 (short).
//' @param accum_rule 0 = left-endpoint, 1 = trapezoidal accumulator quadrature.
//'   Ignored under discrete monitoring.
//' @param monitor_mode 0 = continuous monitoring, 1 = discrete fixing dates.
//' @param fix_w Length-N vector of accumulator weights; entry \code{m} is
//'   applied at step \code{m} and is non-zero only when \code{t_{m+1}} is a
//'   fixing date. Used only when \code{monitor_mode = 1}.
//' @param accum_sd Accumulator grid half-width in standard deviations of the
//'   running average; capped by the reachable-set bound.
//' @param grid_drift 1 = let the log-price grid track the deterministic drift.
//' @param store_policy Whether to return optimal-policy paths.
//' @param n_threads Worker threads for the cached path; 0 or less lets
//'   RcppParallel choose, and 1 runs the backward sweep serially.
//' @param engine_mode 0 = cached path, 1 = reference path.
//' @param verbose Whether to print progress.
//' @return A list with the value at the initial state, the grids, clamp
//'   diagnostics and (optionally) forward policy paths.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List indiff_bellman_engine_cpp(
    double S0, double K, double T_mat, int N,
    Rcpp::NumericVector mu_vec,
    double sigma, double r_cont,
    double lambda_I, double kappa_I,
    Rcpp::NumericVector eta_vec,
    double rho,
    double lambda_bar_T, double lambda_bar_P, double kappa_J,
    double k_A, double k_B, double psi_cost,
    double gamma_ra,
    double Gamma_Q, double ell_1,
    double Q_bar, double eps_exec,
    double phi_cap,
    double n_opt,
    double I0, double Q0, double J0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Q, int n_J, int n_R,
    int asian_type, int option_type, int theta,
    int accum_rule, int monitor_mode, Rcpp::NumericVector fix_w,
    double accum_sd, int grid_drift,
    bool store_policy, int n_threads, int engine_mode, bool verbose
) {
  if (N <= 0) Rcpp::stop("N must be a positive integer");
  if (T_mat <= 0.0) Rcpp::stop("T (maturity) must be positive");
  if (sigma <= 0.0) Rcpp::stop("sigma must be positive");
  if (static_cast<int>(mu_vec.size()) != N)
    Rcpp::stop("mu_vec must have length N");
  if (static_cast<int>(eta_vec.size()) != N)
    Rcpp::stop("eta_vec must have length N");
  if (static_cast<int>(fix_w.size()) != N)
    Rcpp::stop("fix_w must have length N");
  if (n_I < 2 || n_Q < 2 || n_J < 2 || n_R < 2 || (n_logS > 0 && n_logS < 2))
    Rcpp::stop("all grid sizes must be at least 2");
  if (control_set.size() < 1)
    Rcpp::stop("control_set must be non-empty");

  std::vector<double> mu(mu_vec.begin(), mu_vec.end());
  std::vector<double> eta(eta_vec.begin(), eta_vec.end());
  std::vector<double> fixw(fix_w.begin(), fix_w.end());
  std::vector<double> controls(control_set.begin(), control_set.end());
  const int nc = static_cast<int>(controls.size());

  int c_zero = -1;
  double best_abs = std::numeric_limits<double>::infinity();
  for (int c = 0; c < nc; c++) {
    double a = std::abs(controls[c]);
    if (a < best_abs) { best_abs = a; c_zero = c; }
  }
  if (best_abs > 0.0) Rcpp::stop("control_set must contain 0");

  IndiffSpec S;
  S.S0 = S0; S.K = K; S.T_mat = T_mat; S.N = N;
  S.sigma = sigma; S.r_cont = r_cont;
  S.lambda_I = lambda_I; S.kappa_I = kappa_I; S.rho = rho;
  S.lambda_bar_T = lambda_bar_T; S.lambda_bar_P = lambda_bar_P;
  S.kappa_J = kappa_J;
  S.k_A = k_A; S.k_B = k_B; S.psi_cost = psi_cost;
  S.gamma_ra = gamma_ra; S.Gamma_Q = Gamma_Q; S.ell_1 = ell_1;
  S.Q_bar = Q_bar; S.eps_exec = eps_exec;
  S.phi_cap = phi_cap; S.n_opt = n_opt;
  S.I0 = I0; S.Q0 = Q0; S.J0 = J0;
  S.asian_type = asian_type; S.option_type = option_type; S.theta = theta;
  S.accum_rule = accum_rule; S.monitor_mode = monitor_mode;
  S.accum_sd = accum_sd; S.grid_drift = grid_drift;

  IndiffGrids G = build_grids(S, mu, eta, fixw, controls,
                              n_logS, n_I, n_Q, n_J, n_R);
  // build_grids may enlarge n_logS to align it with the shock, enlarge n_R by
  // one so that a = 0 stays on the grid, and collapse n_I when lambda_I == 0.
  n_logS = G.n_logS;
  n_R = G.n_R;
  n_I = G.n_I;

  const double grid_limit = 2.0e8;   // ~1.6 GB per value array
  double approx = static_cast<double>(n_logS) * n_I * n_Q * n_J * n_R;
  if (approx > grid_limit)
    Rcpp::stop("requested grid has %.3g nodes, which exceeds the safety limit "
               "of %.3g; reduce the grid sizes", approx, grid_limit);

  const size_t gs = G.size();
  std::vector<double> V_next(gs), V_curr(gs);
  fill_terminal(V_next, S, G);

  std::vector<double> policy;
  if (store_policy) policy.assign(gs, 0.0);

  const double dt = T_mat / N;
  const double sqdt = std::sqrt(dt);
  const double delta_r = indiff_delta_r(r_cont, dt);

  // (s, i, a) are exogenous: neither the dealer's trades nor the option
  // position moves them, so the zero-shock trajectory can be computed up front.
  // The backward sweep then only has to record the policy restricted to that
  // trajectory, an n_Q x n_J slice per step rather than the full 5-D array.
  std::vector<double> path_off(N + 1), path_logS(N + 1), path_I(N + 1),
                      path_A(N + 1);
  path_off[0] = 0.0; path_I[0] = I0; path_A[0] = 0.0;
  path_logS[0] = G.logS_at(0, 0) - G.off[0];   // = logS0 + shift[0]
  const double off_hi = G.off_lo + (G.n_logS - 1) * G.off_dx;
  for (int m = 0; m < N; m++) {
    double offn = step_logS_off(path_off[m], path_I[m], lambda_I,
                                sigma, dt, sqdt, 0.0);
    if (offn < G.off_lo) offn = G.off_lo;
    if (offn > off_hi)   offn = off_hi;
    path_off[m + 1]  = offn;
    path_logS[m + 1] = G.logS0 + G.shift[m + 1] + offn;
    path_I[m + 1]    = step_I(path_I[m], kappa_I, dt, eta[m], sqdt, 0.0);
    path_A[m + 1]    = accum_next(path_A[m], path_logS[m], path_logS[m + 1],
                                  G.logS0, asian_type, accum_rule,
                                  monitor_mode, fixw[m], dt);
  }
  std::vector<double> pol_slice;
  if (store_policy)
    pol_slice.assign(static_cast<size_t>(N) * n_Q * n_J, 0.0);

  ClampCount cc;
  StepTables tb;

  // One slot per (log-price, impact) plane, written only by the worker that
  // owns the plane, so the count is thread-count independent like everything
  // else in the sweep.
  std::vector<long long> infeas(static_cast<size_t>(G.n_logS) * G.n_I, 0);
  long long n_infeasible = 0;

  for (int m = N - 1; m >= 0; m--) {
    double cash = indiff_beta(r_cont, T_mat, m * dt) * delta_r;

    if (engine_mode == 1) {
      backward_step_reference(V_curr, V_next, store_policy ? &policy : NULL,
                              n_infeasible, S, G, controls, c_zero, cash,
                              m, eta[m], fixw[m], dt, sqdt);
    } else {
      build_step_tables(tb, S, G, controls, m, eta[m], fixw[m], dt, sqdt, cc);
      backward_step_cached(V_curr, V_next, store_policy ? &policy : NULL,
                           infeas, S, G, tb, controls, c_zero, cash,
                           n_threads);
      for (size_t pl = 0; pl < infeas.size(); pl++) n_infeasible += infeas[pl];
    }

    if (store_policy) {
      IndiffBracket bs = indiff_bracket(G.off_lo, G.off_dx, G.n_logS,
                                        path_off[m]);
      IndiffBracket bi = indiff_bracket(G.I_lo, G.I_dx, G.n_I, path_I[m]);
      IndiffBracket ba = indiff_bracket(G.R_lo, G.R_dx, G.n_R, path_A[m]);
      double ws = bs.w, wi = bi.w, wa = ba.w;
      double* out = &pol_slice[static_cast<size_t>(m) * n_Q * n_J];
      for (int iq = 0; iq < n_Q; iq++) {
        for (int ij = 0; ij < n_J; ij++) {
          double v = 0.0;
          for (int ds = 0; ds < 2; ds++) {
            double cs = ds ? ws : (1.0 - ws);
            for (int di = 0; di < 2; di++) {
              double ci = di ? wi : (1.0 - wi);
              for (int da = 0; da < 2; da++) {
                double ca = da ? wa : (1.0 - wa);
                v += cs * ci * ca *
                  policy[G.idx(bs.lo + ds, bi.lo + di, iq, ij, ba.lo + da)];
              }
            }
          }
          out[static_cast<size_t>(iq) * n_J + ij] = v;
        }
      }
    }

    V_next.swap(V_curr);

    if (verbose && ((N - m) % 5 == 0 || m == 0)) {
      Rcpp::Rcout << "  indiff engine (theta = " << theta << "): step "
                  << (N - m) << " / " << N << "\n";
    }
    Rcpp::checkUserInterrupt();
  }

  // Value at the initial state (s0, I0, Q0, J0, a0 = 0).  With grid_drift the
  // time-0 origin is logS0, so the offset coordinate is exactly 0.
  IndiffBracket bs0 = indiff_bracket(G.off_lo, G.off_dx, G.n_logS, 0.0);
  IndiffBracket bi0 = indiff_bracket(G.I_lo, G.I_dx, G.n_I, I0);
  IndiffBracket bq0 = indiff_bracket(G.Q_lo, G.Q_dx, G.n_Q, Q0);
  IndiffBracket bj0 = indiff_bracket(G.J_lo, G.J_dx, G.n_J, J0);
  IndiffBracket ba0 = indiff_bracket(G.R_lo, G.R_dx, G.n_R, 0.0);
  double value = interp5(V_next.data(), G, bs0, bi0, bq0, bj0, ba0);

  std::vector<double> logS_grid0(static_cast<size_t>(G.n_logS));
  for (int is = 0; is < G.n_logS; is++) logS_grid0[is] = G.logS_at(0, is);

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("value")     = value,
    Rcpp::Named("logS_grid") = logS_grid0,
    Rcpp::Named("logS_shift")= G.shift,
    Rcpp::Named("I_grid")    = G.I,
    Rcpp::Named("Q_grid")    = G.Q,
    Rcpp::Named("J_grid")    = G.J,
    Rcpp::Named("R_grid")    = G.R,
    Rcpp::Named("margin")    = G.margin,
    Rcpp::Named("I_bound")   = G.I_bound,
    Rcpp::Named("J_bound")   = G.J_bound,
    Rcpp::Named("n_logS")    = G.n_logS,
    Rcpp::Named("n_R")       = G.n_R,
    Rcpp::Named("n_I")       = G.n_I,
    Rcpp::Named("shock_cells")  = G.shock_cells,
    Rcpp::Named("grid_aligned") = G.aligned,
    Rcpp::Named("collapsed_I")  = G.collapsed_I,
    Rcpp::Named("n_infeasible") = static_cast<double>(n_infeasible),
    Rcpp::Named("initial_state_interior") =
      !(bs0.clamped || bi0.clamped || bq0.clamped || bj0.clamped || ba0.clamped)
  );

  out["n_clamped"] = Rcpp::NumericVector::create(
    Rcpp::Named("logS") = static_cast<double>(cc.S),
    Rcpp::Named("I")    = static_cast<double>(cc.I),
    Rcpp::Named("Q")    = static_cast<double>(cc.Q),
    Rcpp::Named("J")    = static_cast<double>(cc.J),
    Rcpp::Named("R")    = static_cast<double>(cc.R)
  );
  out["n_bracket"] = Rcpp::NumericVector::create(
    Rcpp::Named("logS") = static_cast<double>(cc.S_tot),
    Rcpp::Named("I")    = static_cast<double>(cc.I_tot),
    Rcpp::Named("Q")    = static_cast<double>(cc.Q_tot),
    Rcpp::Named("J")    = static_cast<double>(cc.J_tot),
    Rcpp::Named("R")    = static_cast<double>(cc.R_tot)
  );

  if (store_policy) {
    // Forward pass along the exogenous trajectory, using the recorded slices.
    std::vector<double> nu_path(N), Q_path(N + 1), J_path(N + 1);
    double q = Q0, j = J0;
    Q_path[0] = q; J_path[0] = j;
    double nu_abs_max = 0.0;
    for (int c = 0; c < nc; c++)
      nu_abs_max = std::max(nu_abs_max, std::abs(controls[c]));

    for (int m = 0; m < N; m++) {
      IndiffBracket bq = indiff_bracket(G.Q_lo, G.Q_dx, G.n_Q, q);
      IndiffBracket bj = indiff_bracket(G.J_lo, G.J_dx, G.n_J, j);
      const double* sl = &pol_slice[static_cast<size_t>(m) * n_Q * n_J];
      double nu = (1.0 - bq.w) * (1.0 - bj.w) * sl[bq.lo * n_J + bj.lo]
                + (1.0 - bq.w) * bj.w         * sl[bq.lo * n_J + bj.lo + 1]
                + bq.w * (1.0 - bj.w)         * sl[(bq.lo + 1) * n_J + bj.lo]
                + bq.w * bj.w                 * sl[(bq.lo + 1) * n_J + bj.lo + 1];

      if (nu >  nu_abs_max) nu =  nu_abs_max;
      if (nu < -nu_abs_max) nu = -nu_abs_max;
      double qn = step_Q(q, nu, dt);
      if (qn >  Q_bar) { nu = ( Q_bar - q) / dt; qn =  Q_bar; }
      if (qn < -Q_bar) { nu = (-Q_bar - q) / dt; qn = -Q_bar; }

      nu_path[m] = nu;
      j = step_J(j, nu, kappa_J, dt);
      q = qn;
      Q_path[m + 1] = q;
      J_path[m + 1] = j;
    }

    std::vector<double> S_path(N + 1);
    for (int m = 0; m <= N; m++) S_path[m] = std::exp(path_logS[m]);

    out["nu_path"] = nu_path;
    out["Q_path"]  = Q_path;
    out["J_path"]  = J_path;
    out["S_path"]  = S_path;
    out["I_path"]  = path_I;
    out["A_path"]  = path_A;
  }

  return out;
}
