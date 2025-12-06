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

AdjustedFactors compute_adjusted_factors(
    double r, double u, double d,
    double lambda, double v_u, double v_d
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
