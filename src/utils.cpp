#include "utils.h"

AdjustedFactors compute_adjusted_factors(
    double r, double u, double d,
    double lambda, double v_u, double v_d
) {
    AdjustedFactors factors;

    factors.u_tilde = u * std::exp(lambda * v_u);
    factors.d_tilde = d * std::exp(-lambda * v_d);

    factors.p_adj = (r - factors.d_tilde) / (factors.u_tilde - factors.d_tilde);

    if (factors.p_adj < 0.0 || factors.p_adj > 1.0) {
        Rcpp::stop("Invalid risk-neutral probability: p_adj must be in [0,1]");
    }

    return factors;
}

double geometric_mean(const std::vector<double>& prices) {
    if (prices.empty()) {
        Rcpp::stop("Cannot compute geometric mean of empty vector");
    }

    double log_sum = 0.0;
    for (double price : prices) {
        if (price <= 0.0) {
            Rcpp::stop("All prices must be positive for geometric mean");
        }
        log_sum += std::log(price);
    }

    return std::exp(log_sum / prices.size());
}

double arithmetic_mean(const std::vector<double>& prices) {
    if (prices.empty()) {
        Rcpp::stop("Cannot compute arithmetic mean of empty vector");
    }

    double sum = 0.0;
    for (double price : prices) {
        sum += price;
    }

    return sum / prices.size();
}

std::vector<double> generate_price_path(
    double S0,
    const std::vector<int>& path,
    double u_tilde,
    double d_tilde
) {
    int n = path.size();
    std::vector<double> prices(n + 1);

    prices[0] = S0;

    int n_ups = 0;
    int n_downs = 0;

    for (int i = 0; i < n; ++i) {
        if (path[i] == 1) {
            n_ups++;
        } else {
            n_downs++;
        }

        prices[i + 1] = S0 * std::pow(u_tilde, n_ups) * std::pow(d_tilde, n_downs);
    }

    return prices;
}

double binomial_coefficient(int n, int k) {
    if (k < 0 || k > n) {
        return 0.0;
    }

    if (k == 0 || k == n) {
        return 1.0;
    }

    if (k > n - k) {
        k = n - k;
    }

    double result = 1.0;
    for (int i = 0; i < k; ++i) {
        result *= (n - i);
        result /= (i + 1);
    }

    return result;
}
