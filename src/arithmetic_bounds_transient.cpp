#include <Rcpp.h>
#include "utils.h"
#include <vector>
#include <cmath>
#include <algorithm>

// External function
std::vector<std::vector<int>> generate_all_paths(int n);

// [[Rcpp::export]]
Rcpp::List arithmetic_asian_bounds_transient_cpp(
    double S0, double K, double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    Rcpp::NumericVector volumes,
    bool compute_path_specific = false,
    std::string option_type = "call"
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }

    int n = volumes.size();

    std::vector<double> vol_vec(n);
    for (int i = 0; i < n; ++i) {
        vol_vec[i] = volumes[i];
    }

    std::vector<std::vector<int>> all_paths = generate_all_paths(n);

    double discount = std::pow(r, -n);

    double lower_bound = 0.0;
    double EQ_G = 0.0;

    for (const auto& path : all_paths) {
        std::vector<double> prices = generate_price_path_transient(
            S0, path, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        );

        double G = geometric_mean(prices);
        EQ_G += compute_path_probability_transient(
            path, r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        ) * G;

        double payoff_G;
        if (option_type == "call") {
            payoff_G = std::max(0.0, G - K);
        } else {
            payoff_G = std::max(0.0, K - G);
        }

        lower_bound += compute_path_probability_transient(
            path, r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        ) * payoff_G;
    }

    lower_bound *= discount;
    double V0_G = lower_bound;

    double v_max = *std::max_element(vol_vec.begin(), vol_vec.end());
    double lambda_eff = lambda_P + lambda_T;
    double v_max_impact = std::pow(v_max, psi);

    double transient_max_accum = 0.0;
    if (alpha < 1.0) {
        transient_max_accum = n * lambda_T * alpha * v_max_impact / (1.0 - alpha);
    } else {
        transient_max_accum = lambda_T * v_max_impact * n * n;
    }

    double S_max_star = S0 * std::pow(u, n) * std::exp(n * lambda_eff * v_max_impact + transient_max_accum);
    double S_min_star = S0 * std::pow(d, n) * std::exp(-n * lambda_eff * v_max_impact - transient_max_accum);

    double rho_star = std::exp(
        0.25 * std::pow(S_max_star - S_min_star, 2) / (S_min_star * S_max_star)
    );

    double upper_bound_global = V0_G + (rho_star - 1.0) * discount * EQ_G;

    double upper_bound_path_specific = NA_REAL;
    int n_paths_used = 0;

    if (compute_path_specific) {
        double path_specific_sum = 0.0;

        for (const auto& path : all_paths) {
            std::vector<double> prices = generate_price_path_transient(
                S0, path, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
            );

            double G = geometric_mean(prices);

            double S_min_path = *std::min_element(prices.begin(), prices.end());
            double S_max_path = *std::max_element(prices.begin(), prices.end());

            double rho_path = std::exp(
                0.25 * std::pow(S_max_path - S_min_path, 2) / (S_min_path * S_max_path)
            );

            double path_prob = compute_path_probability_transient(
                path, r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
            );

            path_specific_sum += path_prob * G * (rho_path - 1.0);
        }

        upper_bound_path_specific = V0_G + discount * path_specific_sum;
        n_paths_used = all_paths.size();
    }

    return Rcpp::List::create(
        Rcpp::Named("lower_bound") = lower_bound,
        Rcpp::Named("upper_bound_global") = upper_bound_global,
        Rcpp::Named("upper_bound_path_specific") = upper_bound_path_specific,
        Rcpp::Named("rho_star") = rho_star,
        Rcpp::Named("EQ_G") = EQ_G,
        Rcpp::Named("V0_G") = V0_G,
        Rcpp::Named("n_paths_used") = n_paths_used
    );
}
