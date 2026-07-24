#include <Rcpp.h>
#include "utils.h"
#include <vector>
#include <cmath>
#include <algorithm>

// External function from geometric_asian.cpp
std::vector<std::vector<int>> generate_all_paths(int n);

// [[Rcpp::export]]
Rcpp::List arithmetic_asian_bounds_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n,
    std::string option_type = "call"
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }
    AdjustedFactors factors = compute_adjusted_factors(r, u, d, lambda, v_u, v_d);

    std::vector<std::vector<int>> all_paths = generate_all_paths(n);

    double discount = std::pow(r, -n);

    double lower_bound = 0.0;
    double EQ_G = 0.0;

    for (const auto& path : all_paths) {
        std::vector<double> prices = generate_price_path(S0, path,
                                                         factors.u_tilde,
                                                         factors.d_tilde);

        double G = geometric_mean(prices);

        double payoff;
        if (option_type == "call") {
            payoff = std::max(0.0, G - K);
        } else {
            payoff = std::max(0.0, K - G);
        }

        int n_ups = 0;
        for (int move : path) {
            if (move == 1) n_ups++;
        }

        double path_prob = std::pow(factors.p_adj, n_ups) *
                          std::pow(1.0 - factors.p_adj, n - n_ups);

        lower_bound += path_prob * payoff;
        EQ_G += path_prob * G;
    }

    lower_bound *= discount;

    double u_n = std::pow(factors.u_tilde, n);
    double d_n = std::pow(factors.d_tilde, n);
    double spread = std::pow(u_n - d_n, 2) / (4.0 * u_n * d_n);
    double rho_star = std::exp(spread);

    double upper_bound = lower_bound + discount * (rho_star - 1.0) * EQ_G;

    return Rcpp::List::create(
        Rcpp::Named("lower_bound") = lower_bound,
        Rcpp::Named("upper_bound") = upper_bound,
        Rcpp::Named("rho_star") = rho_star,
        Rcpp::Named("EQ_G") = EQ_G,
        Rcpp::Named("V0_G") = lower_bound
    );
}

// Compute path-specific rho parameter
// @param prices Vector of stock prices along the path
// @return rho(omega) = exp((S_M - S_m)^2 / (4 * S_m * S_M))
double compute_path_rho(const std::vector<double>& prices) {
    double S_min = *std::min_element(prices.begin(), prices.end());
    double S_max = *std::max_element(prices.begin(), prices.end());

    if (S_min <= 0 || S_max <= 0) {
        Rcpp::stop("Invalid prices: all prices must be positive");
    }

    double spread = std::pow(S_max - S_min, 2) / (4.0 * S_min * S_max);
    return std::exp(spread);
}


// [[Rcpp::export]]
Rcpp::List arithmetic_asian_bounds_extended_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n,
    bool compute_path_specific = false,
    std::string option_type = "call"
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }

    AdjustedFactors factors = compute_adjusted_factors(r, u, d, lambda, v_u, v_d);

    std::vector<std::vector<int>> all_paths = generate_all_paths(n);

    double discount = std::pow(r, -n);

    double lower_bound = 0.0;
    double EQ_G = 0.0;

    for (const auto& path : all_paths) {
        std::vector<double> prices = generate_price_path(S0, path,
                                                         factors.u_tilde,
                                                         factors.d_tilde);

        double G = geometric_mean(prices);

        double payoff;
        if (option_type == "call") {
            payoff = std::max(0.0, G - K);
        } else {
            payoff = std::max(0.0, K - G);
        }

        int n_ups = 0;
        for (int move : path) {
            if (move == 1) n_ups++;
        }

        double path_prob = std::pow(factors.p_adj, n_ups) *
                          std::pow(1.0 - factors.p_adj, n - n_ups);

        lower_bound += path_prob * payoff;
        EQ_G += path_prob * G;
    }

    lower_bound *= discount;

    double u_n = std::pow(factors.u_tilde, n);
    double d_n = std::pow(factors.d_tilde, n);
    double spread_global = std::pow(u_n - d_n, 2) / (4.0 * u_n * d_n);
    double rho_star = std::exp(spread_global);

    double upper_bound_global = lower_bound + discount * (rho_star - 1.0) * EQ_G;

    double upper_bound_path_specific = NA_REAL;
    long long n_paths_used = 0;

    if (compute_path_specific) {
        n_paths_used = all_paths.size();
        double sum_path_specific = 0.0;

        for (const auto& path : all_paths) {
            std::vector<double> prices = generate_price_path(S0, path,
                                                             factors.u_tilde,
                                                             factors.d_tilde);

            double G = geometric_mean(prices);
            double rho_omega = compute_path_rho(prices);

            int n_ups = 0;
            for (int move : path) {
                if (move == 1) n_ups++;
            }

            double path_prob = std::pow(factors.p_adj, n_ups) *
                              std::pow(1.0 - factors.p_adj, n - n_ups);

            sum_path_specific += path_prob * (rho_omega - 1.0) * G;
        }

        upper_bound_path_specific = lower_bound + discount * sum_path_specific;
    }

    return Rcpp::List::create(
        Rcpp::Named("lower_bound") = lower_bound,
        Rcpp::Named("upper_bound_global") = upper_bound_global,
        Rcpp::Named("upper_bound_path_specific") = upper_bound_path_specific,
        Rcpp::Named("rho_star") = rho_star,
        Rcpp::Named("EQ_G") = EQ_G,
        Rcpp::Named("V0_G") = lower_bound,
        Rcpp::Named("n_paths_used") = n_paths_used
    );
}
