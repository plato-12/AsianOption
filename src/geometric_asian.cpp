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
//' The function enumerates all 2^n possible price paths and computes:
//' \itemize{
//'   \item Geometric average: \eqn{G = (S_0 \cdot S_1 \cdot \ldots \cdot S_n)^{1/(n+1)}}
//'   \item Call payoff: \eqn{\max(0, G - K)}
//'   \item Put payoff: \eqn{\max(0, K - G)}
//'   \item Option value: \eqn{(1/r^n) \cdot \sum_{paths} p^k (1-p)^{(n-k)} \cdot payoff}
//' }
//'
//' Price impact modifies the up and down factors:
//' \itemize{
//'   \item Adjusted up factor: \eqn{u_{tilde} = u \cdot \exp(\lambda \cdot v_u)}
//'   \item Adjusted down factor: \eqn{d_{tilde} = d \cdot \exp(-\lambda \cdot v_d)}
//' }
//'
//' @references
//' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979). Option pricing:
//' A simplified approach. Journal of Financial Economics, 7(3), 229-263.
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

//' Price Geometric Asian Option using Monte Carlo Simulation
//'
//' Computes the price of a geometric Asian option using Monte Carlo simulation.
//' This method is more efficient for large n (> 20) compared to exact enumeration.
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
//' @param n_simulations Number of Monte Carlo paths to simulate (default: 100000)
//' @param option_type Type of option: "call" or "put" (default: "call")
//' @param seed Random seed for reproducibility (default: -1 for no seed)
//'
//' @return A list containing:
//' \itemize{
//'   \item price: Estimated option price
//'   \item std_error: Standard error of the estimate
//'   \item n_simulations: Number of simulations used
//' }
//'
//' @details
//' The Monte Carlo method randomly samples price paths according to the
//' risk-neutral probability p_adj. For each simulated path:
//' \itemize{
//'   \item Generate n Bernoulli(p_adj) random draws for up/down moves
//'   \item Compute the geometric average of prices along the path
//'   \item Calculate the payoff: max(0, G - K) for calls or max(0, K - G) for puts
//' }
//'
//' The option price is estimated as the mean of discounted payoffs, with
//' standard error = sd(payoffs) / sqrt(n_simulations).
//'
//' Monte Carlo is recommended for n > 20 where exact enumeration becomes
//' computationally prohibitive (2^n paths).
//'
//' @references
//' Glasserman, P. (2003). Monte Carlo Methods in Financial Engineering.
//' Springer.
//'
//' @examples
//' \dontrun{
//' # Price option with n=25 using Monte Carlo
//' result <- price_geometric_asian_mc_cpp(
//'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
//'   lambda = 0.1, v_u = 1.0, v_d = 1.0, n = 25,
//'   n_simulations = 100000, option_type = "call", seed = 42
//' )
//' print(result$price)
//' print(result$std_error)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List price_geometric_asian_mc_cpp(
    double S0, double K, double r, double u, double d,
    double lambda, double v_u, double v_d, int n,
    int n_simulations = 100000,
    std::string option_type = "call",
    int seed = -1
) {
    if (option_type != "call" && option_type != "put") {
        Rcpp::stop("option_type must be either 'call' or 'put'");
    }

    if (n_simulations <= 0) {
        Rcpp::stop("n_simulations must be positive");
    }

    if (seed >= 0) {
        Rcpp::Environment base_env("package:base");
        Rcpp::Function set_seed = base_env["set.seed"];
        set_seed(seed);
    }

    AdjustedFactors factors = compute_adjusted_factors(r, u, d, lambda, v_u, v_d);

    double discount = std::pow(r, -n);

    std::vector<double> payoffs(n_simulations);

    GetRNGstate();

    for (int sim = 0; sim < n_simulations; ++sim) {
        std::vector<int> path(n);
        for (int i = 0; i < n; ++i) {
            path[i] = (R::runif(0.0, 1.0) < factors.p_adj) ? 1 : 0;
        }

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

        payoffs[sim] = discount * payoff;
    }

    PutRNGstate();

    double sum = 0.0;
    double sum_sq = 0.0;

    for (double payoff : payoffs) {
        sum += payoff;
        sum_sq += payoff * payoff;
    }

    double mean_price = sum / n_simulations;
    double variance = (sum_sq / n_simulations) - (mean_price * mean_price);
    double std_error = std::sqrt(variance / n_simulations);

    return Rcpp::List::create(
        Rcpp::Named("price") = mean_price,
        Rcpp::Named("std_error") = std_error,
        Rcpp::Named("n_simulations") = n_simulations
    );
}
