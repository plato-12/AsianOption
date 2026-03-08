#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
List price_arithmetic_asian_diffusion_mc_cpp(
    double S0, double K, double r, double sigma,
    double T_mat, double lambda_T, double I0, double kappa,
    NumericVector eta_values, double rho,
    int n_steps, int n_sims,
    std::string option_type,
    bool use_control_variate,
    int seed
) {
  if (seed != 0) {
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed = base_env["set.seed"];
    set_seed(seed);
  }

  double dt = T_mat / n_steps;
  double sqrt_dt = std::sqrt(dt);
  double discount = std::exp(-r * T_mat);

  double half_sigma_sq = 0.5 * sigma * sigma;
  double rho_complement = std::sqrt(1.0 - rho * rho);

  NumericVector arith_payoffs(n_sims);
  NumericVector geom_payoffs(n_sims);

  for (int j = 0; j < n_sims; j++) {
    double S = S0;
    double I = I0;
    double Y = 0.0;
    double sum_log_S = std::log(S0);

    for (int m = 0; m < n_steps; m++) {
      double N1 = R::rnorm(0.0, 1.0);
      double N2 = R::rnorm(0.0, 1.0);
      double Z1 = N1;
      double Z2 = rho * N1 + rho_complement * N2;

      Y += S * dt;

      double eta_m = eta_values[m];
      I = I + (-kappa * I) * dt + eta_m * Z2 * sqrt_dt;

      double log_S_increment = (r - half_sigma_sq + lambda_T * I) * dt + sigma * Z1 * sqrt_dt;
      S = S * std::exp(log_S_increment);

      sum_log_S += std::log(S);
    }

    double A = Y / T_mat;

    double G = std::exp(sum_log_S / (n_steps + 1));

    if (option_type == "call") {
      arith_payoffs[j] = discount * std::max(0.0, A - K);
      geom_payoffs[j] = discount * std::max(0.0, G - K);
    } else {
      arith_payoffs[j] = discount * std::max(0.0, K - A);
      geom_payoffs[j] = discount * std::max(0.0, K - G);
    }
  }

  double mean_arith = Rcpp::mean(arith_payoffs);
  double std_arith = Rcpp::sd(arith_payoffs);

  double price_estimate;
  double std_error;
  double geom_mc_price = Rcpp::mean(geom_payoffs);

  if (use_control_variate) {
    NumericVector diffs = arith_payoffs - geom_payoffs;
    double mean_diff = Rcpp::mean(diffs);
    double std_diff = Rcpp::sd(diffs);

    price_estimate = mean_diff;
    std_error = std_diff / std::sqrt((double)n_sims);
  } else {
    price_estimate = mean_arith;
    std_error = std_arith / std::sqrt((double)n_sims);
  }

  double correlation = 0.0;
  {
    double mean_a = Rcpp::mean(arith_payoffs);
    double mean_g = Rcpp::mean(geom_payoffs);
    double cov = 0.0, var_a = 0.0, var_g = 0.0;
    for (int j = 0; j < n_sims; j++) {
      double da = arith_payoffs[j] - mean_a;
      double dg = geom_payoffs[j] - mean_g;
      cov += da * dg;
      var_a += da * da;
      var_g += dg * dg;
    }
    if (var_a > 0 && var_g > 0) {
      correlation = cov / std::sqrt(var_a * var_g);
    }
  }

  return List::create(
    Named("price_estimate") = price_estimate,
    Named("std_error") = std_error,
    Named("geom_mc_price") = geom_mc_price,
    Named("raw_arith_price") = mean_arith,
    Named("raw_arith_se") = std_arith / std::sqrt((double)n_sims),
    Named("correlation") = correlation,
    Named("n_sims") = n_sims,
    Named("n_steps") = n_steps
  );
}
