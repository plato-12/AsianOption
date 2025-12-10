#include <Rcpp.h>
#include "utils.h"
#include <cmath>
#include <algorithm>

//' Price European Call Option with Price Impact
//'
//' Computes the exact price of a European call option using the
//' binomial tree model with price impact from hedging activities.
//'
//' @param S0 Initial stock price (positive)
//' @param K Strike price (positive)
//' @param r Gross risk-free rate per period (e.g., 1.05 for 5\% rate)
//' @param u Base up factor in CRR model (e.g., 1.2)
//' @param d Base down factor in CRR model (e.g., 0.8)
//' @param lambda Price impact coefficient (non-negative)
//' @param v_u Hedging volume on up move (non-negative)
//' @param v_d Hedging volume on down move (non-negative)
//' @param n Number of time steps (positive integer)
//'
//' @return European call option price
//'
//' @details
//' Computes European call option prices using the binomial model with price impact.
//' Unlike path-dependent Asian options, European options only depend on the terminal
//' stock price, allowing for efficient O(n) computation. Price impact from hedging
//' activities modifies the up and down factors. See the package vignettes and
//' reference paper for detailed mathematical formulations.
//'
//' @references
//' Tiwari, P., & Majumdar, S. (2024). Asian option valuation under price impact.
//' arXiv preprint. \doi{10.48550/arXiv.2512.07154}
//'
//' @examples
//' \dontrun{
//' # Basic example with 10 time steps
//' price_european_call_cpp(
//'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
//'   lambda = 0.1, v_u = 1.0, v_d = 1.0, n = 10
//' )
//' }
//'
//' @export
// [[Rcpp::export]]
double price_european_call_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n
) {
    AdjustedFactors factors = compute_adjusted_factors(r, u, d, lambda, v_u, v_d);

    double discount = std::pow(r, -n);

    double option_value = 0.0;

    for (int k = 0; k <= n; ++k) {
        double S_n = S0 * std::pow(factors.u_tilde, k) * std::pow(factors.d_tilde, n - k);

        double payoff = std::max(0.0, S_n - K);

        double binom_coeff = binomial_coefficient(n, k);
        double prob = binom_coeff * std::pow(factors.p_adj, k) *
                     std::pow(1.0 - factors.p_adj, n - k);

        option_value += prob * payoff;
    }

    option_value *= discount;

    return option_value;
}

//' Price European Put Option with Price Impact
//'
//' Computes the exact price of a European put option using the
//' binomial tree model with price impact from hedging activities.
//'
//' @param S0 Initial stock price (positive)
//' @param K Strike price (positive)
//' @param r Gross risk-free rate per period (e.g., 1.05 for 5\% rate)
//' @param u Base up factor in CRR model (e.g., 1.2)
//' @param d Base down factor in CRR model (e.g., 0.8)
//' @param lambda Price impact coefficient (non-negative)
//' @param v_u Hedging volume on up move (non-negative)
//' @param v_d Hedging volume on down move (non-negative)
//' @param n Number of time steps (positive integer)
//'
//' @return European put option price
//'
//' @details
//' Computes European put option prices using the binomial model with price impact.
//' Unlike path-dependent Asian options, European options only depend on the terminal
//' stock price, allowing for efficient O(n) computation. Price impact from hedging
//' activities modifies the up and down factors. See the package vignettes and
//' reference paper for detailed mathematical formulations.
//'
//' @references
//' Tiwari, P., & Majumdar, S. (2024). Asian option valuation under price impact.
//' arXiv preprint. \doi{10.48550/arXiv.2512.07154}
//'
//' @examples
//' \dontrun{
//' # Basic example with 10 time steps
//' price_european_put_cpp(
//'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
//'   lambda = 0.1, v_u = 1.0, v_d = 1.0, n = 10
//' )
//' }
//'
//' @export
// [[Rcpp::export]]
double price_european_put_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n
) {
    AdjustedFactors factors = compute_adjusted_factors(r, u, d, lambda, v_u, v_d);

    double discount = std::pow(r, -n);

    double option_value = 0.0;

    for (int k = 0; k <= n; ++k) {
        double S_n = S0 * std::pow(factors.u_tilde, k) * std::pow(factors.d_tilde, n - k);

        double payoff = std::max(0.0, K - S_n);

        double binom_coeff = binomial_coefficient(n, k);
        double prob = binom_coeff * std::pow(factors.p_adj, k) *
                     std::pow(1.0 - factors.p_adj, n - k);

        option_value += prob * payoff;
    }

    option_value *= discount;

    return option_value;
}
