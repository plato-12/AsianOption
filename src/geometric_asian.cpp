#include <Rcpp.h>
#include "utils.h"
#include <vector>
#include <cmath>

// Helper function to generate all binary paths of length n
void generate_all_paths_recursive(
    int n,
    int current_step,
    std::vector<int>& current_path,
    std::vector<std::vector<int>>& all_paths
) {
    if (current_step == n) {
        all_paths.push_back(current_path);
        return;
    }

    current_path[current_step] = 1;
    generate_all_paths_recursive(n, current_step + 1, current_path, all_paths);

    current_path[current_step] = 0;
    generate_all_paths_recursive(n, current_step + 1, current_path, all_paths);
}

// [[Rcpp::export]]
std::vector<std::vector<int>> generate_all_paths(int n) {
    std::vector<std::vector<int>> all_paths;
    std::vector<int> current_path(n);

    generate_all_paths_recursive(n, 0, current_path, all_paths);

    return all_paths;
}

// [[Rcpp::export]]
double price_geometric_asian_cpp(
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

    double option_value = 0.0;

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

        option_value += path_prob * payoff;
    }

    option_value *= discount;

    return option_value;
}

