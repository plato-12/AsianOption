#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// [[Rcpp::export]]
List price_kemna_vorst_arithmetic_cpp(
    double S0, double K, double r, double sigma,
    double T0, double T_mat, int n, int M,
    std::string option_type = "call",
    bool use_control_variate = true,
    int seed = 0
) {
  if (seed != 0) {
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed = base_env["set.seed"];
    set_seed(seed);
  }

  double tau = T_mat - T0;
  double dt = tau / n;
  double discount = std::exp(-r * tau);

  double drift = (r - 0.5 * sigma * sigma) * dt;
  double vol_sqrt_dt = sigma * std::sqrt(dt);

  double sigma_G = sigma / std::sqrt(3.0);
  double d_star = 0.5 * (r - sigma * sigma / 6.0) * tau;
  double d = (std::log(S0 / K) + 0.5 * (r + sigma * sigma / 6.0) * tau) /
             (sigma * std::sqrt(tau / 3.0));
  double d2 = d - sigma_G * std::sqrt(tau);

  double geometric_price;
  if (option_type == "call") {
    geometric_price = std::exp(d_star) * S0 * R::pnorm(d, 0.0, 1.0, 1, 0) -
                      K * R::pnorm(d2, 0.0, 1.0, 1, 0);
  } else {
    geometric_price = K * R::pnorm(-d2, 0.0, 1.0, 1, 0) -
                      std::exp(d_star) * S0 * R::pnorm(-d, 0.0, 1.0, 1, 0);
  }

  NumericVector arithmetic_payoffs(M);
  NumericVector geometric_payoffs(M);
  NumericVector differences(M);

  for (int j = 0; j < M; j++) {
    NumericVector S(n + 1);
    S[0] = S0;

    double log_S = std::log(S0);
    double sum_log_S = log_S;

    for (int i = 1; i <= n; i++) {
      double Z = R::rnorm(0.0, 1.0);

      log_S = log_S + drift + vol_sqrt_dt * Z;
      S[i] = std::exp(log_S);

      sum_log_S += log_S;
    }

    double A = 0.0;
    for (int i = 0; i <= n; i++) {
      A += S[i];
    }
    A /= (n + 1);

    double G = std::exp(sum_log_S / (n + 1));

    double Y, W;
    if (option_type == "call") {
      Y = discount * std::max(0.0, A - K);
      W = discount * std::max(0.0, G - K);
    } else {
      Y = discount * std::max(0.0, K - A);
      W = discount * std::max(0.0, K - G);
    }

    arithmetic_payoffs[j] = Y;
    geometric_payoffs[j] = W;
    differences[j] = Y - W;
  }

  double mean_diff = Rcpp::mean(differences);
  double std_diff = Rcpp::sd(differences);

  double price_estimate;
  double std_error;

  if (use_control_variate) {
    price_estimate = geometric_price + mean_diff;
    std_error = std_diff / std::sqrt(M);
  } else {
    double mean_arith = Rcpp::mean(arithmetic_payoffs);
    double std_arith = Rcpp::sd(arithmetic_payoffs);
    price_estimate = mean_arith;
    std_error = std_arith / std::sqrt(M);
  }

  double ci_margin = 1.96 * std_error;
  double lower_ci = price_estimate - ci_margin;
  double upper_ci = price_estimate + ci_margin;

  double correlation = 0.0;
  if (use_control_variate) {
    double mean_Y = Rcpp::mean(arithmetic_payoffs);
    double mean_W = Rcpp::mean(geometric_payoffs);
    double cov = 0.0;
    double var_Y = 0.0;
    double var_W = 0.0;

    for (int j = 0; j < M; j++) {
      double diff_Y = arithmetic_payoffs[j] - mean_Y;
      double diff_W = geometric_payoffs[j] - mean_W;
      cov += diff_Y * diff_W;
      var_Y += diff_Y * diff_Y;
      var_W += diff_W * diff_W;
    }

    if (var_Y > 0 && var_W > 0) {
      correlation = cov / std::sqrt(var_Y * var_W);
    }
  }

  double variance_reduction_factor = 1.0;
  if (use_control_variate && correlation > 0) {
    variance_reduction_factor = 1.0 - correlation * correlation;
  }

  return List::create(
    Named("price") = price_estimate,
    Named("std_error") = std_error,
    Named("lower_ci") = lower_ci,
    Named("upper_ci") = upper_ci,
    Named("geometric_price") = geometric_price,
    Named("correlation") = correlation,
    Named("variance_reduction_factor") = variance_reduction_factor,
    Named("n_simulations") = M,
    Named("n_steps") = n
  );
}

// [[Rcpp::export]]
List price_kemna_vorst_arithmetic_binomial_cpp(
    double S0, double K, double r, double u, double d,
    int n, int M,
    std::string option_type = "call",
    bool use_control_variate = true,
    int seed = 0
) {
  double r_continuous = std::log(r);

  double dt = 1.0 / n;

  double sigma = std::log(u / d) / (2.0 * std::sqrt(dt));

  return price_kemna_vorst_arithmetic_cpp(
    S0, K, r_continuous, sigma,
    0.0, 1.0,
    n, M, option_type, use_control_variate, seed
  );
}
