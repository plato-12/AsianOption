#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//' Kemna-Vorst Monte Carlo Simulation for Arithmetic Average Asian Option
//'
//' Implements the Kemna-Vorst (1990) Monte Carlo method with variance reduction
//' using the geometric average as a control variate. This is the standard
//' benchmark implementation WITHOUT price impact.
//'
//' @param S0 Initial stock price at time T0
//' @param K Strike price
//' @param r Continuously compounded risk-free rate
//' @param sigma Volatility (annualized)
//' @param T0 Start of averaging period
//' @param T Maturity time
//' @param n Number of averaging points (observations)
//' @param M Number of Monte Carlo simulations
//' @param option_type String: "call" or "put"
//' @param use_control_variate Boolean: use variance reduction (default TRUE)
//' @param seed Integer: random seed for reproducibility (default 0 = no seed)
//'
//' @return List containing:
//' \describe{
//'   \item{price}{Estimated option price}
//'   \item{std_error}{Standard error of the estimate}
//'   \item{lower_ci}{Lower 95\% confidence interval}
//'   \item{upper_ci}{Upper 95\% confidence interval}
//'   \item{geometric_price}{Analytical geometric average price (control variate)}
//'   \item{correlation}{Correlation between arithmetic and geometric payoffs}
//'   \item{variance_reduction_factor}{Ratio of variances (with/without control variate)}
//' }
//'
//' @details
//' Implements the Kemna & Vorst (1990) Monte Carlo algorithm with control variate
//' variance reduction:
//'
//' 1. Generate price paths under risk-neutral dynamics
//' 2. Calculate arithmetic and geometric averages for each path
//' 3. Calculate discounted payoffs for both averages
//' 4. Use the geometric average (with known analytical price) as a control variate
//'    to reduce variance of the arithmetic average estimate
//'
//' The variance reduction can be dramatic (factor 10-70) because the
//' correlation between arithmetic and geometric averages is typically > 0.95.
//' This is a standard benchmark method WITHOUT price impact.
//'
//' @references
//' Kemna, A.G.Z. and Vorst, A.C.F. (1990). "A Pricing Method for Options Based
//' on Average Asset Values." \emph{Journal of Banking and Finance}, 14, 113-129.
//'
//' @examples
//' \donttest{
//' # Basic example
//' result <- price_kemna_vorst_arithmetic_cpp(
//'   S0 = 100, K = 100, r = 0.05, sigma = 0.2,
//'   T0 = 0, T = 1, n = 50, M = 10000,
//'   option_type = "call"
//' )
//' print(result$price)
//' print(result$std_error)
//' }
//'
//' @export
// [[Rcpp::export]]
List price_kemna_vorst_arithmetic_cpp(
    double S0, double K, double r, double sigma,
    double T0, double T, int n, int M,
    std::string option_type = "call",
    bool use_control_variate = true,
    int seed = 0
) {
  if (seed != 0) {
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed = base_env["set.seed"];
    set_seed(seed);
  }

  double tau = T - T0;
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


//' Kemna-Vorst Monte Carlo with Binomial Parameters
//'
//' Alternative interface using discrete binomial parameters instead of
//' continuous parameters.
//'
//' @param S0 Initial stock price
//' @param K Strike price
//' @param r Gross risk-free rate per period
//' @param u Up factor
//' @param d Down factor
//' @param n Number of time steps
//' @param M Number of Monte Carlo simulations
//' @param option_type String: "call" or "put"
//' @param use_control_variate Boolean: use variance reduction
//' @param seed Integer: random seed
//'
//' @return List with pricing results (same as price_kemna_vorst_arithmetic_cpp)
//'
//' @export
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
