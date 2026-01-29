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

// Transient impact model functions

TransientAdjustedFactors compute_transient_adjusted_factors(
    double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    double v_m, double I_m
) {
    TransientAdjustedFactors factors;

    // Effective impact coefficient
    double lambda_eff = lambda_P + lambda_T;

    // Volume impact term
    double v_impact = std::pow(v_m, psi);

    // Adjusted up and down factors
    // u_tilde = u * exp(lambda_P * v^psi + lambda_T * (I_m + v^psi))
    // d_tilde = d * exp(-lambda_P * v^psi + lambda_T * (I_m - v^psi))

    factors.u_tilde = u * std::exp(lambda_eff * v_impact + lambda_T * I_m);
    factors.d_tilde = d * std::exp(-lambda_eff * v_impact + lambda_T * I_m);

    // Risk-neutral probability (I_m cancels out in numerator and denominator)
    factors.p_adj = (r - factors.d_tilde) / (factors.u_tilde - factors.d_tilde);

    if (factors.p_adj < 0.0 || factors.p_adj > 1.0) {
        Rcpp::stop("Invalid risk-neutral probability in transient model: p_adj must be in [0,1]");
    }

    factors.I_m = I_m;

    return factors;
}

double update_transient_accumulator(
    double I_m, double alpha, double psi,
    double v_m, int epsilon_m
) {
    // I_{m+1} = alpha * I_m + epsilon_m * v_m^psi
    double v_impact = std::pow(v_m, psi);
    return alpha * I_m + epsilon_m * v_impact;
}

std::vector<double> generate_price_path_transient(
    double S0,
    const std::vector<int>& path,
    double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    const std::vector<double>& volumes
) {
    int n = path.size();
    std::vector<double> prices(n + 1);

    prices[0] = S0;

    double I_m = 0.0;  // Initial transient accumulator

    for (int i = 0; i < n; ++i) {
        // Compute volume impact
        double v_impact = std::pow(volumes[i], psi);
        double lambda_eff = lambda_P + lambda_T;

        // Compute adjusted factors
        double u_tilde = u * std::exp(lambda_eff * v_impact + lambda_T * I_m);
        double d_tilde = d * std::exp(-lambda_eff * v_impact + lambda_T * I_m);

        // Update price based on move
        if (path[i] == 1) {
            prices[i + 1] = prices[i] * u_tilde;
        } else {
            prices[i + 1] = prices[i] * d_tilde;
        }

        // Update transient accumulator: I_{m+1} = alpha * I_m + epsilon_m * v^psi
        I_m = alpha * I_m + path[i] * v_impact;
    }

    return prices;
}

double compute_path_probability_transient(
    const std::vector<int>& path,
    double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    const std::vector<double>& volumes
) {
    int n = path.size();
    double prob = 1.0;
    double I_m = 0.0;

    for (int i = 0; i < n; ++i) {
        // Compute adjusted risk-neutral probability at time i
        TransientAdjustedFactors factors = compute_transient_adjusted_factors(
            r, u, d, lambda_P, lambda_T, alpha, psi, volumes[i], I_m
        );

        // Multiply by probability of this move
        if (path[i] == 1) {
            prob *= factors.p_adj;
        } else {
            prob *= (1.0 - factors.p_adj);
        }

        // Update transient accumulator
        I_m = update_transient_accumulator(I_m, alpha, psi, volumes[i], path[i]);
    }

    return prob;
}
