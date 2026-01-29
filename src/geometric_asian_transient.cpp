#include <Rcpp.h>
#include "utils.h"
#include <vector>
#include <cmath>

// External function from geometric_asian.cpp
std::vector<std::vector<int>> generate_all_paths(int n);

// [[Rcpp::export]]
double price_geometric_asian_transient_cpp(
    double S0, double K, double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    Rcpp::NumericVector volumes,
    std::string option_type = "call"
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }

    int n = volumes.size();

    // Convert R vector to C++ vector
    std::vector<double> vol_vec(n);
    for (int i = 0; i < n; ++i) {
        vol_vec[i] = volumes[i];
    }

    // Generate all 2^n paths
    std::vector<std::vector<int>> all_paths = generate_all_paths(n);

    double discount = std::pow(r, -n);
    double option_value = 0.0;

    // Price over all paths
    for (const auto& path : all_paths) {
        // Generate price path with transient impact
        std::vector<double> prices = generate_price_path_transient(
            S0, path, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        );

        // Compute geometric mean
        double G = geometric_mean(prices);

        // Compute payoff
        double payoff;
        if (option_type == "call") {
            payoff = std::max(0.0, G - K);
        } else {
            payoff = std::max(0.0, K - G);
        }

        // Compute path probability with transient impact
        double path_prob = compute_path_probability_transient(
            path, r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        );

        option_value += path_prob * payoff;
    }

    option_value *= discount;

    return option_value;
}

// Monte Carlo version for large n
// [[Rcpp::export]]
Rcpp::List price_geometric_asian_transient_mc_cpp(
    double S0, double K, double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    Rcpp::NumericVector volumes,
    int n_simulations = 100000,
    int seed = 42,
    std::string option_type = "call"
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }

    int n = volumes.size();

    // Convert R vector to C++ vector
    std::vector<double> vol_vec(n);
    for (int i = 0; i < n; ++i) {
        vol_vec[i] = volumes[i];
    }

    // Set random seed
    Rcpp::Environment base_env("package:base");
    Rcpp::Function set_seed = base_env["set.seed"];
    set_seed(seed);

    double discount = std::pow(r, -n);
    double sum_payoffs = 0.0;
    double sum_payoffs_sq = 0.0;

    // Monte Carlo simulation
    for (int sim = 0; sim < n_simulations; ++sim) {
        std::vector<double> prices(n + 1);
        prices[0] = S0;

        double I_m = 0.0;  // Transient accumulator
        std::vector<int> path(n);

        // Generate one path
        for (int i = 0; i < n; ++i) {
            // Compute risk-neutral probability at time i
            TransientAdjustedFactors factors = compute_transient_adjusted_factors(
                r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec[i], I_m
            );

            // Sample move
            double u_rand = R::runif(0.0, 1.0);
            int epsilon_i;
            if (u_rand < factors.p_adj) {
                epsilon_i = 1;
                prices[i + 1] = prices[i] * factors.u_tilde / u;
            } else {
                epsilon_i = -1;
                prices[i + 1] = prices[i] * factors.d_tilde / d;
            }

            path[i] = epsilon_i;

            // Update transient accumulator
            I_m = update_transient_accumulator(I_m, alpha, psi, vol_vec[i], epsilon_i);
        }

        // Compute geometric mean
        double G = geometric_mean(prices);

        // Compute payoff
        double payoff;
        if (option_type == "call") {
            payoff = std::max(0.0, G - K);
        } else {
            payoff = std::max(0.0, K - G);
        }

        sum_payoffs += payoff;
        sum_payoffs_sq += payoff * payoff;
    }

    // Compute price and standard error
    double mean_payoff = sum_payoffs / n_simulations;
    double price = discount * mean_payoff;

    double variance = (sum_payoffs_sq / n_simulations) - (mean_payoff * mean_payoff);
    double std_error = discount * std::sqrt(variance / n_simulations);

    // Confidence interval
    double z_95 = 1.96;
    double ci_lower = price - z_95 * std_error;
    double ci_upper = price + z_95 * std_error;

    return Rcpp::List::create(
        Rcpp::Named("price") = price,
        Rcpp::Named("std_error") = std_error,
        Rcpp::Named("ci_lower") = ci_lower,
        Rcpp::Named("ci_upper") = ci_upper,
        Rcpp::Named("n_simulations") = n_simulations
    );
}
