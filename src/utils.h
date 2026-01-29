#ifndef UTILS_H
#define UTILS_H

#include <Rcpp.h>
#include <vector>
#include <cmath>

struct AdjustedFactors {
    double u_tilde;
    double d_tilde;
    double p_adj;
};

// Transient impact factors (path-dependent)
struct TransientAdjustedFactors {
    double u_tilde;
    double d_tilde;
    double p_adj;
    double I_m;  // Transient accumulator at time m
};

// Permanent impact model (original)
AdjustedFactors compute_adjusted_factors(
    double r, double u, double d,
    double lambda, double v_u, double v_d
);

// Transient impact: compute adjusted factors at time m given path history
TransientAdjustedFactors compute_transient_adjusted_factors(
    double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    double v_m, double I_m
);

// Compute transient accumulator update: I_{m+1} = alpha * I_m + epsilon_m * v_m^psi
double update_transient_accumulator(
    double I_m, double alpha, double psi,
    double v_m, int epsilon_m
);

// Generate price path with transient impact (path-dependent factors)
std::vector<double> generate_price_path_transient(
    double S0,
    const std::vector<int>& path,
    double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    const std::vector<double>& volumes
);

// Compute path probability with path-dependent risk-neutral probabilities
double compute_path_probability_transient(
    const std::vector<int>& path,
    double r, double u, double d,
    double lambda_P, double lambda_T,
    double alpha, double psi,
    const std::vector<double>& volumes
);

double geometric_mean(const std::vector<double>& prices);

double arithmetic_mean(const std::vector<double>& prices);

std::vector<double> generate_price_path(
    double S0,
    const std::vector<int>& path,
    double u_tilde,
    double d_tilde
);

double binomial_coefficient(int n, int k);

#endif
