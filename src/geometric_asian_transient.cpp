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

    std::vector<double> vol_vec(n);
    for (int i = 0; i < n; ++i) {
        vol_vec[i] = volumes[i];
    }

    std::vector<std::vector<int>> all_paths = generate_all_paths(n);

    double discount = std::pow(r, -n);
    double option_value = 0.0;

    for (const auto& path : all_paths) {
        std::vector<double> prices = generate_price_path_transient(
            S0, path, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        );

        double G = geometric_mean(prices);

        double payoff;
        if (option_type == "call") {
            payoff = std::max(0.0, G - K);
        } else {
            payoff = std::max(0.0, K - G);
        }

        double path_prob = compute_path_probability_transient(
            path, r, u, d, lambda_P, lambda_T, alpha, psi, vol_vec
        );

        option_value += path_prob * payoff;
    }

    option_value *= discount;

    return option_value;
}
