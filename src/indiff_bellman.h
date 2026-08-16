#ifndef ASIANOPTION_INDIFF_BELLMAN_H
#define ASIANOPTION_INDIFF_BELLMAN_H

#include <vector>
#include <cmath>
#include <algorithm>

// Shared scalar helpers for the utility-indifference Bellman engine.
// The equation numbering refers to Section 2 of design.md, which is the
// single source of truth for the model.

// delta_r = (1 - exp(-r*dt)) / r, with the removable singularity at r = 0
// handled exactly (delta_r -> dt).
inline double indiff_delta_r(double r, double dt) {
  if (std::abs(r) < 1e-12) return dt;
  return -std::expm1(-r * dt) / r;
}

// beta_m = exp(r * (T - t_m)).
inline double indiff_beta(double r, double T, double t_m) {
  return std::exp(r * (T - t_m));
}

// Four-branch probabilities p_{xi,zeta} = (1 + rho*xi*zeta)/4 in the branch
// order used throughout the engine:
//   b = 0 : (xi, zeta) = (+1, +1)
//   b = 1 : (xi, zeta) = (+1, -1)
//   b = 2 : (xi, zeta) = (-1, +1)
//   b = 3 : (xi, zeta) = (-1, -1)
inline void indiff_branch_probs(double rho, double* p4) {
  p4[0] = 0.25 * (1.0 + rho);
  p4[1] = 0.25 * (1.0 - rho);
  p4[2] = 0.25 * (1.0 - rho);
  p4[3] = 0.25 * (1.0 + rho);
}

// Sign of xi for branch b (b = 0,1 are up-moves in S).
inline double indiff_xi_sign(int b) { return (b < 2) ? 1.0 : -1.0; }
// Sign of zeta for branch b (b = 0,2 are up-moves in I).
inline double indiff_zeta_sign(int b) { return (b % 2 == 0) ? 1.0 : -1.0; }

// Terminal option payoff Phi(a).  The accumulator a carries the same scaling
// as the legacy engine: a = int (S_u / S0) du for the arithmetic case and
// a = int (log S_u - log S0) du for the geometric case.
// asian_type: 0 = arithmetic, 1 = geometric.  option_type: 0 = call, 1 = put.
// phi_cap <= 0 means "no cap"
// Under GBM, CARA utility of Q_T S_T − θΦ has
// infinite exponential moments in continuous state space.
inline double indiff_payoff(double a, double S0, double K, double T,
                            int asian_type, int option_type, double phi_cap) {
  double avg = (asian_type == 0) ? (S0 * a / T) : (S0 * std::exp(a / T));
  double phi = (option_type == 0) ? std::max(avg - K, 0.0)
                                  : std::max(K - avg, 0.0);
  if (phi_cap > 0.0 && phi > phi_cap) phi = phi_cap;
  return phi;
}

// Bracket index + interpolation weight on a uniform grid.
struct IndiffBracket {
  int lo;
  double w;
  bool clamped;
};

inline IndiffBracket indiff_bracket(double lo, double dx, int n, double val) {
  IndiffBracket b;
  b.clamped = false;
  if (n <= 1) { b.lo = 0; b.w = 0.0; return b; }

  double t = (dx > 0.0) ? (val - lo) / dx : 0.0;
  const double tol = 1e-9;

  if (t <= 0.0) {
    b.lo = 0; b.w = 0.0;
    b.clamped = (t < -tol);
    return b;
  }
  if (t >= static_cast<double>(n - 1)) {
    b.lo = n - 2; b.w = 1.0;
    b.clamped = (t > static_cast<double>(n - 1) + tol);
    return b;
  }
  int idx = static_cast<int>(t);
  if (idx > n - 2) idx = n - 2;
  b.lo = idx;
  b.w = t - static_cast<double>(idx);
  return b;
}

// Quadrature weights c_m, m = 0..N, with which the engine builds the
// accumulator: a_T = sum_m c_m * h(S_{t_m}).  These mirror accum_next()
// exactly, so anything derived from them describes the accumulator the engine
// actually forms rather than its continuous-time idealisation.
//   monitor_mode 1 : step m adds fix_w[m] * h(S_{t_{m+1}}), so node m+1 carries
//                    weight fix_w[m] and node 0 carries none;
//   accum_rule  0 : step m adds h(S_{t_m}) * dt (left endpoint);
//   accum_rule  1 : step m adds (h(S_{t_m}) + h(S_{t_{m+1}})) * dt/2, summing
//                   to the trapezoid weights dt/2, dt, ..., dt, dt/2.
inline std::vector<double> indiff_accum_weights(
    int N, double dt, int accum_rule, int monitor_mode,
    const std::vector<double>& fix_w) {
  std::vector<double> c(static_cast<size_t>(N) + 1, 0.0);
  if (monitor_mode == 1) {
    for (int m = 0; m < N; m++) {
      if (static_cast<size_t>(m) < fix_w.size())
        c[static_cast<size_t>(m) + 1] += fix_w[static_cast<size_t>(m)];
    }
    return c;
  }
  for (int m = 0; m < N; m++) {
    if (accum_rule == 0) {
      c[static_cast<size_t>(m)] += dt;
    } else {
      c[static_cast<size_t>(m)]     += 0.5 * dt;
      c[static_cast<size_t>(m) + 1] += 0.5 * dt;
    }
  }
  return c;
}

struct IndiffAccumMoments {
  std::vector<double> mean;   // mean[m] = E[a_{t_m}], length N+1
  double sd;                  // standard deviation of a_T
};

// Mean path and terminal standard deviation of the ARITHMETIC accumulator
// a_t = int_0^t (S_u / S_0) du, discretised with the weights above.  These size
// and centre the accumulator grid; the geometric accumulator is handled
// separately, by the log-scale bound it is already correctly described by.
//
// Under the exogenous dynamics log(S_{t_m}/S_0) is Gaussian with mean
// A_m - sigma^2 t_m / 2, A_m = sum_{k<m} mu_k dt, and variance sigma^2 t_m, so
// with X_m = S_{t_m}/S_0,
//   E[X_m]     = exp(A_m),
//   E[X_m X_l] = exp(A_m + A_l + sigma^2 min(t_m, t_l)),
// and a_t = sum_m c_m X_m gives
//   mean[m] = sum_{k<=m} c_k E[X_k],
//   Var(a_T) = sum_{m,l} c_m c_l E[X_m X_l] - mean[N]^2.
//
// This is the discrete counterpart of the Turnbull-Wakeman / Kemna-Vorst
// arithmetic-Asian moments and agrees with them to five decimals at N = 25.
// The discrete form is used rather than the continuous closed form because it
// is exact for a time-varying mu, for either accum_rule, and for discrete
// monitoring, none of which the continuous formula covers.  It treats the
// lattice shock as Gaussian, which overstates E[X_m] by O(dt^2) per step; that
// is far inside the tolerance of a quantity used only to size a grid.
inline IndiffAccumMoments indiff_accum_moments(
    int N, double dt, const std::vector<double>& mu_vec, double sigma,
    int accum_rule, int monitor_mode, const std::vector<double>& fix_w) {
  IndiffAccumMoments M;
  M.mean.assign(static_cast<size_t>(N) + 1, 0.0);
  M.sd = 0.0;
  if (N < 0) return M;

  std::vector<double> c = indiff_accum_weights(N, dt, accum_rule,
                                               monitor_mode, fix_w);
  std::vector<double> A(static_cast<size_t>(N) + 1, 0.0);
  std::vector<double> EX(static_cast<size_t>(N) + 1, 0.0);
  for (int m = 1; m <= N; m++) {
    double mu_m = (static_cast<size_t>(m - 1) < mu_vec.size())
                    ? mu_vec[static_cast<size_t>(m - 1)] : 0.0;
    A[static_cast<size_t>(m)] = A[static_cast<size_t>(m) - 1] + mu_m * dt;
  }
  for (int m = 0; m <= N; m++)
    EX[static_cast<size_t>(m)] = std::exp(A[static_cast<size_t>(m)]);

  // Mean path by the same recursion accum_next() runs, with h(S) replaced by
  // its expectation.  The running accumulator is NOT a partial sum of the
  // terminal weights -- under the trapezoid rule the current node carries
  // dt/2, not dt -- and it must start at exactly 0, which is where the engine
  // starts it.
  for (int m = 0; m < N; m++) {
    double inc;
    if (monitor_mode == 1) {
      double w = (static_cast<size_t>(m) < fix_w.size())
                   ? fix_w[static_cast<size_t>(m)] : 0.0;
      inc = w * EX[static_cast<size_t>(m) + 1];
    } else if (accum_rule == 0) {
      inc = EX[static_cast<size_t>(m)] * dt;
    } else {
      inc = 0.5 * (EX[static_cast<size_t>(m)] + EX[static_cast<size_t>(m) + 1]) * dt;
    }
    M.mean[static_cast<size_t>(m) + 1] = M.mean[static_cast<size_t>(m)] + inc;
  }

  double M2 = 0.0;
  for (int m = 0; m <= N; m++) {
    if (c[static_cast<size_t>(m)] == 0.0) continue;
    for (int l = 0; l <= N; l++) {
      if (c[static_cast<size_t>(l)] == 0.0) continue;
      double t_min = dt * ((m < l) ? m : l);
      M2 += c[static_cast<size_t>(m)] * c[static_cast<size_t>(l)]
              * std::exp(A[static_cast<size_t>(m)] + A[static_cast<size_t>(l)]
                         + sigma * sigma * t_min);
    }
  }

  double var = M2 - M.mean[static_cast<size_t>(N)] * M.mean[static_cast<size_t>(N)];
  M.sd = (var > 0.0) ? std::sqrt(var) : 0.0;
  return M;
}

inline std::vector<double> indiff_uniform_grid(double lo, double hi, int n) {
  std::vector<double> g(static_cast<size_t>(n));
  if (n <= 1) { g[0] = 0.5 * (lo + hi); return g; }
  double dx = (hi - lo) / (n - 1);
  for (int i = 0; i < n; i++) g[static_cast<size_t>(i)] = lo + i * dx;
  return g;
}

#endif
