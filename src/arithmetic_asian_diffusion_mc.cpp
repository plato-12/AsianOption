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

  // Precompute drift coefficient for log-Euler scheme
  double half_sigma_sq = 0.5 * sigma * sigma;
  double rho_complement = std::sqrt(1.0 - rho * rho);

  NumericVector arith_payoffs(n_sims);
  NumericVector geom_payoffs(n_sims);

  for (int j = 0; j < n_sims; j++) {
    double S = S0;
    double I = I0;
    double Y = 0.0;          // running arithmetic integral
    double sum_log_S = std::log(S0);  // running sum of log(S) for geometric average

    for (int m = 0; m < n_steps; m++) {
      // Generate correlated normals
      double N1 = R::rnorm(0.0, 1.0);
      double N2 = R::rnorm(0.0, 1.0);
      double Z1 = N1;                              // drives S
      double Z2 = rho * N1 + rho_complement * N2;  // drives I

      // Accumulate Y before updating S (left-point rule: dY = S dt)
      Y += S * dt;

      // Update I: Euler-Maruyama
      // dI = -kappa * I dt + eta(t) dW^I
      double eta_m = eta_values[m];
      I = I + (-kappa * I) * dt + eta_m * Z2 * sqrt_dt;

      // Update S: log-Euler (exact for GBM part)
      // log S_{m+1} = log S_m + (r - 0.5*sigma^2 + lambda_T*I_m)*dt + sigma*Z1*sqrt(dt)
      double log_S_increment = (r - half_sigma_sq + lambda_T * I) * dt + sigma * Z1 * sqrt_dt;
      S = S * std::exp(log_S_increment);

      // Accumulate log(S) for geometric average
      sum_log_S += std::log(S);
    }

    // Arithmetic average: Y_T / T
    double A = Y / T_mat;

    // Geometric average: exp(sum_log_S / (n_steps+1))
    double G = std::exp(sum_log_S / (n_steps + 1));

    // Compute payoffs
    if (option_type == "call") {
      arith_payoffs[j] = discount * std::max(0.0, A - K);
      geom_payoffs[j] = discount * std::max(0.0, G - K);
    } else {
      arith_payoffs[j] = discount * std::max(0.0, K - A);
      geom_payoffs[j] = discount * std::max(0.0, K - G);
    }
  }

  // Compute statistics
  double mean_arith = Rcpp::mean(arith_payoffs);
  double std_arith = Rcpp::sd(arith_payoffs);

  double price_estimate;
  double std_error;
  double geom_mc_price = Rcpp::mean(geom_payoffs);

  if (use_control_variate) {
    // Control variate: use difference (arith - geom) + E[geom]
    // The geometric closed-form is passed separately from R
    NumericVector diffs = arith_payoffs - geom_payoffs;
    double mean_diff = Rcpp::mean(diffs);
    double std_diff = Rcpp::sd(diffs);

    // Return the mean difference; R will add the closed-form geometric price
    price_estimate = mean_diff;
    std_error = std_diff / std::sqrt((double)n_sims);
  } else {
    price_estimate = mean_arith;
    std_error = std_arith / std::sqrt((double)n_sims);
  }

  // Correlation between arithmetic and geometric payoffs
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
