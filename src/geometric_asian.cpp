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

//' Price Geometric Asian Option with Price Impact
//'
//' Computes the exact price of a geometric Asian option (call or put) using the
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
//' @param option_type Type of option: "call" or "put" (default: "call")
//'
//' @return Geometric Asian option price
//'
//' @details
//' The function uses exact enumeration, computing all 2^n possible price paths
//' in the binomial tree. This is an exact calculation with no sampling or
//' approximation. Price impact from hedging activities modifies the up and down
//' factors before computing the geometric average and option payoff.
//'
//' @references
//' Tiwari, P., & Majumdar, S. (2024). Asian option valuation under price impact.
//' arXiv preprint. \doi{10.48550/arXiv.2512.07154}
//'
//' @examples
//' \dontrun{
//' # Call option example with 3 time steps
//' price_geometric_asian_cpp(
//'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
//'   lambda = 0.1, v_u = 1.0, v_d = 1.0, n = 3, option_type = "call"
//' )
//'
//' # Put option example
//' price_geometric_asian_cpp(
//'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
//'   lambda = 0.1, v_u = 1.0, v_d = 1.0, n = 3, option_type = "put"
//' )
//' }
//'
//' @export
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

