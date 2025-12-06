#include <Rcpp.h>
#include "utils.h"
#include <vector>
#include <cmath>
#include <algorithm>
#include <random>
#include <set>

// External function from geometric_asian.cpp
std::vector<std::vector<int>> generate_all_paths(int n);

//' Compute Bounds for Arithmetic Asian Option
//'
//' Computes lower and upper bounds for the arithmetic Asian option (call or put)
//' using Jensen's inequality and the relationship between arithmetic
//' and geometric means.
//'
//' @param S0 Initial stock price
//' @param K Strike price
//' @param r Gross risk-free rate
//' @param u Base up factor
//' @param d Base down factor
//' @param lambda Price impact coefficient
//' @param v_u Hedging volume on up move
//' @param v_d Hedging volume on down move
//' @param n Number of time steps
//' @param option_type Type of option: "call" or "put" (default: "call")
//'
//' @return List containing:
//' \itemize{
//'   \item \code{lower_bound}: Lower bound (geometric option price)
//'   \item \code{upper_bound}: Upper bound
//'   \item \code{rho_star}: Spread parameter
//'   \item \code{EQ_G}: Expected geometric average
//' }
//'
//' @details
//' Lower bound: \eqn{V_0^A \ge V_0^G} (by AM-GM inequality)
//'
//' Upper bound: \eqn{V_0^A \le V_0^G + (rho^* - 1) \cdot E^Q(G_n) / r^n}
//'
//' where \eqn{rho^* = \exp((u_{tilde}^n - d_{tilde}^n)^2 / (4 \cdot u_{tilde}^n \cdot d_{tilde}^n))}
//'
//' @export
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

// Convert integer index to binary path
// @param idx Path index (0 to 2^n - 1)
// @param n Number of steps
// @return Vector of 0s and 1s representing the path
std::vector<int> index_to_path(int idx, int n) {
    std::vector<int> path(n);
    for (int j = 0; j < n; j++) {
        path[j] = (idx >> j) & 1;  // Extract j-th bit
    }
    return path;
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

//' Compute Arithmetic Asian Bounds with Path-Specific Upper Bound
//'
//' Computes lower bound (geometric option) and two upper bounds:
//' global (using rho*) and path-specific (using sampled paths).
//'
//' @param S0 Initial stock price
//' @param K Strike price
//' @param r Gross risk-free rate
//' @param u Base up factor
//' @param d Base down factor
//' @param lambda Price impact coefficient
//' @param v_u Hedging volume on up move
//' @param v_d Hedging volume on down move
//' @param n Number of time steps
//' @param compute_path_specific If TRUE, compute path-specific bound
//' @param max_sample_size Maximum number of paths to sample (default 100000)
//' @param sample_fraction Fraction of paths to sample (default 0.1 = 10\%)
//' @param option_type Type of option: "call" or "put" (default: "call")
//'
//' @return List with components:
//' \itemize{
//'   \item \code{lower_bound}: Lower bound (geometric option price)
//'   \item \code{upper_bound_global}: Global upper bound using rho*
//'   \item \code{upper_bound_path_specific}: Path-specific upper bound (NA if not computed)
//'   \item \code{rho_star}: Spread parameter
//'   \item \code{EQ_G}: Expected geometric average
//'   \item \code{V0_G}: Geometric option price (same as lower_bound)
//'   \item \code{n_paths_sampled}: Number of paths sampled
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List arithmetic_asian_bounds_extended_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n,
    bool compute_path_specific = false,
    int max_sample_size = 100000,
    double sample_fraction = 0.1,
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
    int n_paths_sampled = 0;

    if (compute_path_specific) {
        long long total_paths = 1LL << n;

        long long desired_sample = (long long)(sample_fraction * total_paths);
        n_paths_sampled = std::min((long long)max_sample_size, desired_sample);
        n_paths_sampled = std::max(1, n_paths_sampled);

        if (n_paths_sampled >= total_paths) {
            n_paths_sampled = total_paths;
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

        } else {
            std::random_device rd;
            std::mt19937 gen(rd());

            std::set<int> sampled_indices;
            std::uniform_int_distribution<> dis(0, total_paths - 1);

            while ((int)sampled_indices.size() < n_paths_sampled) {
                sampled_indices.insert(dis(gen));
            }

            double sum_path_specific = 0.0;

            for (int idx : sampled_indices) {
                std::vector<int> path = index_to_path(idx, n);

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

            double scaling = (double)total_paths / (double)n_paths_sampled;
            upper_bound_path_specific = lower_bound + discount * scaling * sum_path_specific;
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("lower_bound") = lower_bound,
        Rcpp::Named("upper_bound_global") = upper_bound_global,
        Rcpp::Named("upper_bound_path_specific") = upper_bound_path_specific,
        Rcpp::Named("rho_star") = rho_star,
        Rcpp::Named("EQ_G") = EQ_G,
        Rcpp::Named("V0_G") = lower_bound,
        Rcpp::Named("n_paths_sampled") = n_paths_sampled
    );
}
