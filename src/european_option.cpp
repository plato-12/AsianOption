#include <Rcpp.h>
#include "utils.h"
#include <cmath>
#include <algorithm>

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
