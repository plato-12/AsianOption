#ifndef HJB_BELLMAN_H
#define HJB_BELLMAN_H

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

inline double running_cost(double nu, double k_A, double k_B, double psi_cost) {
    double nu_plus = std::max(nu, 0.0);
    double nu_minus = std::max(-nu, 0.0);
    return k_A * std::pow(nu_plus, 1.0 + psi_cost) +
           k_B * std::pow(nu_minus, 1.0 + psi_cost);
}

#endif
