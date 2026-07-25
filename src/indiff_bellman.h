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
// phi_cap <= 0 means "no cap" (see flag F1 in design.md).
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

inline std::vector<double> indiff_uniform_grid(double lo, double hi, int n) {
  std::vector<double> g(static_cast<size_t>(n));
  if (n <= 1) { g[0] = 0.5 * (lo + hi); return g; }
  double dx = (hi - lo) / (n - 1);
  for (int i = 0; i < n; i++) g[static_cast<size_t>(i)] = lo + i * dx;
  return g;
}

#endif
