#include "hjb_bellman.h"
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <limits>
#include <algorithm>

static int find_bracket(const std::vector<double>& grid, int n, double val, double& weight) {
  if (n <= 1) { weight = 0.0; return 0; }

  if (val <= grid[0]) { weight = 0.0; return 0; }
  if (val >= grid[n - 1]) { weight = 1.0; return n - 2; }

  double dx = grid[1] - grid[0];
  int idx = static_cast<int>((val - grid[0]) / dx);
  if (idx >= n - 1) idx = n - 2;
  if (idx < 0) idx = 0;

  double denom = (grid[idx + 1] - grid[idx]);
  weight = (denom > 0.0) ? (val - grid[idx]) / denom : 0.0;
  if (weight < 0.0) weight = 0.0;
  if (weight > 1.0) weight = 1.0;

  return idx;
}

static double trilinear_interp(
    const std::vector<double>& V,
    const std::vector<double>& grid_x, int n_x,
    const std::vector<double>& grid_y, int n_y,
    const std::vector<double>& grid_z, int n_z,
    double x, double y, double z
) {
  double wx, wy, wz;
  int ix = find_bracket(grid_x, n_x, x, wx);
  int iy = find_bracket(grid_y, n_y, y, wy);
  int iz = find_bracket(grid_z, n_z, z, wz);

  int stride_x = n_y * n_z;
  int stride_y = n_z;

  double c000 = V[ix * stride_x + iy * stride_y + iz];
  double c001 = V[ix * stride_x + iy * stride_y + (iz + 1)];
  double c010 = V[ix * stride_x + (iy + 1) * stride_y + iz];
  double c011 = V[ix * stride_x + (iy + 1) * stride_y + (iz + 1)];
  double c100 = V[(ix + 1) * stride_x + iy * stride_y + iz];
  double c101 = V[(ix + 1) * stride_x + iy * stride_y + (iz + 1)];
  double c110 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + iz];
  double c111 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + (iz + 1)];

  double c00 = c000 * (1.0 - wz) + c001 * wz;
  double c01 = c010 * (1.0 - wz) + c011 * wz;
  double c10 = c100 * (1.0 - wz) + c101 * wz;
  double c11 = c110 * (1.0 - wz) + c111 * wz;

  double c0 = c00 * (1.0 - wy) + c01 * wy;
  double c1 = c10 * (1.0 - wy) + c11 * wy;

  return c0 * (1.0 - wx) + c1 * wx;
}

static double trilinear_interp_slice(
    const double* V,
    const std::vector<double>& grid_x, int n_x,
    const std::vector<double>& grid_y, int n_y,
    const std::vector<double>& grid_z, int n_z,
    double x, double y, double z
) {
  double wx, wy, wz;
  int ix = find_bracket(grid_x, n_x, x, wx);
  int iy = find_bracket(grid_y, n_y, y, wy);
  int iz = find_bracket(grid_z, n_z, z, wz);

  int stride_x = n_y * n_z;
  int stride_y = n_z;

  double c000 = V[ix * stride_x + iy * stride_y + iz];
  double c001 = V[ix * stride_x + iy * stride_y + (iz + 1)];
  double c010 = V[ix * stride_x + (iy + 1) * stride_y + iz];
  double c011 = V[ix * stride_x + (iy + 1) * stride_y + (iz + 1)];
  double c100 = V[(ix + 1) * stride_x + iy * stride_y + iz];
  double c101 = V[(ix + 1) * stride_x + iy * stride_y + (iz + 1)];
  double c110 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + iz];
  double c111 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + (iz + 1)];

  double c00 = c000 * (1.0 - wz) + c001 * wz;
  double c01 = c010 * (1.0 - wz) + c011 * wz;
  double c10 = c100 * (1.0 - wz) + c101 * wz;
  double c11 = c110 * (1.0 - wz) + c111 * wz;

  double c0 = c00 * (1.0 - wy) + c01 * wy;
  double c1 = c10 * (1.0 - wy) + c11 * wy;

  return c0 * (1.0 - wx) + c1 * wx;
}

static std::vector<double> make_uniform_grid(double lo, double hi, int n) {
  std::vector<double> grid(n);
  if (n <= 1) { grid[0] = 0.5 * (lo + hi); return grid; }
  double dx = (hi - lo) / (n - 1);
  for (int i = 0; i < n; i++) grid[i] = lo + i * dx;
  return grid;
}

static double policy_interp_at_m(
    const std::vector<double>& policy_full,
    int m, int grid_size,
    double log_s, double i_val, double y_val,
    const std::vector<double>& logS_grid, int n_logS,
    const std::vector<double>& I_grid, int n_I,
    const std::vector<double>& Y_grid, int n_Y
) {
  const double* slice = policy_full.data() + static_cast<size_t>(m) * grid_size;
  return trilinear_interp_slice(
    slice, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
    log_s, i_val, y_val
  );
}

static std::vector<double> forward_pass_policy(
    const std::vector<double>& policy_full,
    double S0, double I0, double T, int N, int asian_type,
    double kappa, double lambda_bar_T, double lambda_bar_P, double r_cont,
    const std::vector<double>& logS_grid, int n_logS,
    const std::vector<double>& I_grid, int n_I,
    const std::vector<double>& Y_grid, int n_Y
) {
  int grid_size = n_logS * n_I * n_Y;
  double dt = T / N;
  double alpha_m = 1.0 - kappa * dt;

  std::vector<double> nu_path(static_cast<size_t>(N), 0.0);
  double log_s = std::log(S0);
  double i_val = I0;
  double y_val = 0.0;

  for (int m = 0; m < N; m++) {
    double nu = policy_interp_at_m(
      policy_full, m, grid_size,
      log_s, i_val, y_val,
      logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y
    );
    nu_path[static_cast<size_t>(m)] = nu;

    double s_val = std::exp(log_s);
    double i_drift = (-kappa * i_val + nu) * dt;
    i_val = i_val + i_drift;

    if (asian_type == 0) {
      y_val = y_val + (s_val / S0) * dt;
    } else {
      double log_S0_const = std::log(S0);
      y_val = y_val + (log_s - log_S0_const) * dt;
    }

    double log_s_drift = (r_cont + lambda_bar_T * alpha_m * i_val + lambda_bar_P * nu) * dt;
    log_s = log_s + log_s_drift;
  }

  return nu_path;
}


static std::pair<double, double> solve_bellman_step(
    double log_s, double i_val, double y_val,
    int asian_type,
    double log_S0,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double alpha_curr, double sigma, double r_cont,
    double k_A, double k_B, double psi_cost,
    double dt, double sqrt_dt, double eta_m, double p, double discount,
    const std::vector<double>& control_set,
    const std::vector<double>& V_next,
    const std::vector<double>& logS_grid, int n_logS,
    const std::vector<double>& I_grid, int n_I,
    const std::vector<double>& Y_grid, int n_Y
) {
  double best_val = -std::numeric_limits<double>::infinity();
  double best_nu  = 0.0;

  for (size_t jc = 0; jc < control_set.size(); jc++) {
    double nu = control_set[jc];

    double i_drift = (-kappa * i_val + nu) * dt;
    double i_plus  = i_val + i_drift + eta_m * sqrt_dt;
    double i_minus = i_val + i_drift - eta_m * sqrt_dt;

    double log_s_drift = (r_cont + lambda_bar_T * alpha_curr * i_val +
                          lambda_bar_P * nu) * dt;
    double log_s_plus  = log_s + log_s_drift + sigma * sqrt_dt;
    double log_s_minus = log_s + log_s_drift - sigma * sqrt_dt;

    double y_next;
    if (asian_type == 0) {
      y_next = y_val + std::exp(log_s - log_S0) * dt;
    } else {
      y_next = y_val + (log_s - log_S0) * dt;
    }

    double cont_plus = trilinear_interp(
      V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
      log_s_plus, i_plus, y_next
    );
    double cont_minus = trilinear_interp(
      V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
      log_s_minus, i_minus, y_next
    );

    double cost = running_cost(nu, k_A, k_B, psi_cost) * dt;

    double J = -cost + discount * (p * cont_plus + (1.0 - p) * cont_minus);

    if (J > best_val) {
      best_val = J;
      best_nu  = nu;
    }
  }

  return {best_val, best_nu};
}


static Rcpp::List hjb_bellman_engine_single(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    const std::vector<double>& eta_vec,
    double p, double I0,
    const std::vector<double>& control_set,
    int n_logS, int n_I, int n_Y,
    int asian_type,
    double payoff_sign,
    bool store_policy
) {
  double dt = T / N;
  double sqrt_dt = std::sqrt(dt);
  double alpha_m = 1.0 - kappa * dt;
  double discount = std::exp(-r_cont * dt);

  double log_S0 = std::log(S0);
  double max_drift = std::abs(r_cont) +
    std::abs(lambda_bar_T) * 10.0 + std::abs(lambda_bar_P) * 10.0;
  double margin = max_drift * T + 4.0 * sigma * std::sqrt(T);
  if (margin < 1.0) margin = 1.0;

  std::vector<double> logS_grid = make_uniform_grid(log_S0 - margin, log_S0 + margin, n_logS);

  double nu_abs_max = 0.0;
  for (size_t j = 0; j < control_set.size(); j++)
    nu_abs_max = std::max(nu_abs_max, std::abs(control_set[j]));

  double eta_max = 0.0;
  for (int m = 0; m < N; m++)
    eta_max = std::max(eta_max, eta_vec[m]);

  double I_bound = 0.0;
  if (kappa > 1e-12) {
    I_bound = nu_abs_max / kappa + 4.0 * eta_max / std::sqrt(2.0 * kappa);
  } else {
    I_bound = nu_abs_max * T + 4.0 * eta_max * std::sqrt(T);
  }
  if (I_bound < 1.0) I_bound = 1.0;

  std::vector<double> I_grid = make_uniform_grid(-I_bound, I_bound, n_I);

  double S_max_approx = S0 * std::exp(margin);
  double Y_lo, Y_hi;
  if (asian_type == 0) {
    Y_lo = 0.0;
    Y_hi = (S_max_approx / S0) * T;
  } else {
    Y_lo = -margin * T;
    Y_hi = margin * T;
  }
  std::vector<double> Y_grid = make_uniform_grid(Y_lo, Y_hi, n_Y);

  int grid_size = n_logS * n_I * n_Y;
  std::vector<double> V_next(grid_size, 0.0);
  std::vector<double> V_curr(grid_size, 0.0);

  std::vector<double> policy_full;
  if (store_policy) policy_full.resize(static_cast<size_t>(N) * grid_size, 0.0);

  for (int is = 0; is < n_logS; is++) {
    for (int ii = 0; ii < n_I; ii++) {
      for (int iy = 0; iy < n_Y; iy++) {
        int idx = is * n_I * n_Y + ii * n_Y + iy;
        double y_val = Y_grid[iy];

        double intrinsic;
        if (asian_type == 0) {
          intrinsic = std::max(S0 * y_val / T - K, 0.0);
        } else {
          intrinsic = std::max(S0 * std::exp(y_val / T) - K, 0.0);
        }
        V_next[idx] = payoff_sign * intrinsic;
      }
    }
  }

  for (int m = N - 1; m >= 0; m--) {
    double eta_m = eta_vec[m];

    for (int is = 0; is < n_logS; is++) {
      double log_s = logS_grid[is];

      for (int ii = 0; ii < n_I; ii++) {
        double i_val = I_grid[ii];

        for (int iy = 0; iy < n_Y; iy++) {
          double y_val = Y_grid[iy];

          auto result = solve_bellman_step(
            log_s, i_val, y_val,
            asian_type,
            log_S0,
            kappa, lambda_bar_T, lambda_bar_P,
            alpha_m, sigma, r_cont,
            k_A, k_B, psi_cost,
            dt, sqrt_dt, eta_m, p, discount,
            control_set,
            V_next,
            logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y
          );

          int idx = is * n_I * n_Y + ii * n_Y + iy;
          V_curr[idx] = result.first;

          if (store_policy) {
            policy_full[static_cast<size_t>(m) * grid_size + idx] = result.second;
          }
        }
      }
    }

    V_next.swap(V_curr);
  }

  double value = trilinear_interp(
    V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
    log_S0, I0, 0.0
  );

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("value")    = value,
    Rcpp::Named("logS_grid")= logS_grid,
    Rcpp::Named("I_grid")   = I_grid,
    Rcpp::Named("Y_grid")   = Y_grid
  );

  if (store_policy) {
    out["policy"] = policy_full;
    out["policy_dims"] = Rcpp::IntegerVector::create(N, n_logS, n_I, n_Y);
  }

  return out;
}

// [[Rcpp::export]]
Rcpp::List hjb_arithmetic_value_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y,
    double payoff_sign,
    bool store_policy = false
) {
  std::vector<double> eta(eta_vec.begin(), eta_vec.end());
  std::vector<double> controls(control_set.begin(), control_set.end());

  return hjb_bellman_engine_single(
    S0, K, T, N, sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta, p, I0, controls,
    n_logS, n_I, n_Y,
    /* asian_type */ 0,
    payoff_sign,
    store_policy
  );
}

// [[Rcpp::export]]
Rcpp::List hjb_geometric_value_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y,
    double payoff_sign,
    bool store_policy = false
) {
  std::vector<double> eta(eta_vec.begin(), eta_vec.end());
  std::vector<double> controls(control_set.begin(), control_set.end());

  return hjb_bellman_engine_single(
    S0, K, T, N, sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta, p, I0, controls,
    n_logS, n_I, n_Y,
    /* asian_type */ 1,
    payoff_sign,
    store_policy
  );
}

// [[Rcpp::export]]
Rcpp::List hjb_arithmetic_quotes_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y
) {
  auto V0    = hjb_arithmetic_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                        k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                        /*payoff_sign*/ 0.0, false);
  auto Vplus = hjb_arithmetic_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                        k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                        /*payoff_sign*/ +1.0, false);
  auto Vminus= hjb_arithmetic_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                        k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                        /*payoff_sign*/ -1.0, false);

  double v0    = Rcpp::as<double>(V0["value"]);
  double vplus = Rcpp::as<double>(Vplus["value"]);
  double vminus= Rcpp::as<double>(Vminus["value"]);

  double seller_indiff = v0 - vminus;
  double buyer_indiff  = vplus - v0;
  double bid = std::min(seller_indiff, buyer_indiff);
  double ask = std::max(seller_indiff, buyer_indiff);

  return Rcpp::List::create(
    Rcpp::Named("bid")             = bid,
    Rcpp::Named("ask")             = ask,
    Rcpp::Named("mid")             = 0.5 * (bid + ask),
    Rcpp::Named("seller_indiff")   = seller_indiff,
    Rcpp::Named("buyer_indiff")    = buyer_indiff,
    Rcpp::Named("V0")              = v0,
    Rcpp::Named("Vplus")           = vplus,
    Rcpp::Named("Vminus")          = vminus
  );
}

// [[Rcpp::export]]
Rcpp::List hjb_arithmetic_quotes_with_policy_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y
) {
  std::vector<double> eta(eta_vec.begin(), eta_vec.end());
  std::vector<double> controls(control_set.begin(), control_set.end());

  auto V0    = hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         0, 0.0, false);
  auto Vplus = hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         0, +1.0, true);
  auto Vminus= hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         0, -1.0, true);

  double v0    = Rcpp::as<double>(V0["value"]);
  double vplus = Rcpp::as<double>(Vplus["value"]);
  double vminus= Rcpp::as<double>(Vminus["value"]);

  double seller_indiff = v0 - vminus;
  double buyer_indiff  = vplus - v0;
  double bid = std::min(seller_indiff, buyer_indiff);
  double ask = std::max(seller_indiff, buyer_indiff);

  std::vector<double> logS_vec = Rcpp::as<std::vector<double>>(V0["logS_grid"]);
  std::vector<double> I_vec    = Rcpp::as<std::vector<double>>(V0["I_grid"]);
  std::vector<double> Y_vec    = Rcpp::as<std::vector<double>>(V0["Y_grid"]);

  std::vector<double> policy_short = Rcpp::as<std::vector<double>>(Vminus["policy"]);
  std::vector<double> policy_long  = Rcpp::as<std::vector<double>>(Vplus["policy"]);

  std::vector<double> seller_nu = forward_pass_policy(
    policy_short, S0, I0, T, N, 0,
    kappa, lambda_bar_T, lambda_bar_P, r_cont,
    logS_vec, n_logS, I_vec, n_I, Y_vec, n_Y
  );
  std::vector<double> buyer_nu = forward_pass_policy(
    policy_long, S0, I0, T, N, 0,
    kappa, lambda_bar_T, lambda_bar_P, r_cont,
    logS_vec, n_logS, I_vec, n_I, Y_vec, n_Y
  );

  return Rcpp::List::create(
    Rcpp::Named("bid")             = bid,
    Rcpp::Named("ask")             = ask,
    Rcpp::Named("mid")             = 0.5 * (bid + ask),
    Rcpp::Named("seller_indiff")   = seller_indiff,
    Rcpp::Named("buyer_indiff")    = buyer_indiff,
    Rcpp::Named("V0")              = v0,
    Rcpp::Named("Vplus")           = vplus,
    Rcpp::Named("Vminus")          = vminus,
    Rcpp::Named("optimal_nu_seller")   = seller_nu,
    Rcpp::Named("optimal_nu_buyer")    = buyer_nu
  );
}

// [[Rcpp::export]]
Rcpp::List hjb_geometric_quotes_with_policy_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y
) {
  std::vector<double> eta(eta_vec.begin(), eta_vec.end());
  std::vector<double> controls(control_set.begin(), control_set.end());

  auto V0    = hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         1, 0.0, false);
  auto Vplus = hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         1, +1.0, true);
  auto Vminus= hjb_bellman_engine_single(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                         k_A,k_B,psi_cost,eta,p,I0,controls,n_logS,n_I,n_Y,
                                         1, -1.0, true);

  double v0    = Rcpp::as<double>(V0["value"]);
  double vplus = Rcpp::as<double>(Vplus["value"]);
  double vminus= Rcpp::as<double>(Vminus["value"]);

  double seller_indiff = v0 - vminus;
  double buyer_indiff  = vplus - v0;
  double bid = std::min(seller_indiff, buyer_indiff);
  double ask = std::max(seller_indiff, buyer_indiff);

  std::vector<double> logS_vec = Rcpp::as<std::vector<double>>(V0["logS_grid"]);
  std::vector<double> I_vec    = Rcpp::as<std::vector<double>>(V0["I_grid"]);
  std::vector<double> Y_vec    = Rcpp::as<std::vector<double>>(V0["Y_grid"]);

  std::vector<double> policy_short = Rcpp::as<std::vector<double>>(Vminus["policy"]);
  std::vector<double> policy_long  = Rcpp::as<std::vector<double>>(Vplus["policy"]);

  std::vector<double> seller_nu = forward_pass_policy(
    policy_short, S0, I0, T, N, 1,
    kappa, lambda_bar_T, lambda_bar_P, r_cont,
    logS_vec, n_logS, I_vec, n_I, Y_vec, n_Y
  );
  std::vector<double> buyer_nu = forward_pass_policy(
    policy_long, S0, I0, T, N, 1,
    kappa, lambda_bar_T, lambda_bar_P, r_cont,
    logS_vec, n_logS, I_vec, n_I, Y_vec, n_Y
  );

  return Rcpp::List::create(
    Rcpp::Named("bid")             = bid,
    Rcpp::Named("ask")             = ask,
    Rcpp::Named("mid")             = 0.5 * (bid + ask),
    Rcpp::Named("seller_indiff")   = seller_indiff,
    Rcpp::Named("buyer_indiff")    = buyer_indiff,
    Rcpp::Named("V0")              = v0,
    Rcpp::Named("Vplus")           = vplus,
    Rcpp::Named("Vminus")          = vminus,
    Rcpp::Named("optimal_nu_seller")   = seller_nu,
    Rcpp::Named("optimal_nu_buyer")    = buyer_nu
  );
}

// [[Rcpp::export]]
Rcpp::List hjb_geometric_quotes_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y
) {
  auto V0    = hjb_geometric_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                       k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                       0.0, false);
  auto Vplus = hjb_geometric_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                       k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                       +1.0, false);
  auto Vminus= hjb_geometric_value_cpp(S0,K,T,N,sigma,r_cont,kappa,lambda_bar_T,lambda_bar_P,
                                       k_A,k_B,psi_cost,eta_vec,p,I0,control_set,n_logS,n_I,n_Y,
                                       -1.0, false);

  double v0    = Rcpp::as<double>(V0["value"]);
  double vplus = Rcpp::as<double>(Vplus["value"]);
  double vminus= Rcpp::as<double>(Vminus["value"]);

  double seller_indiff = v0 - vminus;
  double buyer_indiff  = vplus - v0;
  double bid = std::min(seller_indiff, buyer_indiff);
  double ask = std::max(seller_indiff, buyer_indiff);

  return Rcpp::List::create(
    Rcpp::Named("bid")             = bid,
    Rcpp::Named("ask")             = ask,
    Rcpp::Named("mid")             = 0.5 * (bid + ask),
    Rcpp::Named("seller_indiff")   = seller_indiff,
    Rcpp::Named("buyer_indiff")    = buyer_indiff,
    Rcpp::Named("V0")              = v0,
    Rcpp::Named("Vplus")           = vplus,
    Rcpp::Named("Vminus")          = vminus
  );
}
