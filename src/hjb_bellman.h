#ifndef HJB_BELLMAN_H
#define HJB_BELLMAN_H

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

// Convex running cost: C(nu) = k_A * (nu+)^{1+psi} + k_B * (nu-)^{1+psi}
inline double running_cost(double nu, double k_A, double k_B, double psi_cost) {
    double nu_plus = std::max(nu, 0.0);
    double nu_minus = std::max(-nu, 0.0);
    return k_A * std::pow(nu_plus, 1.0 + psi_cost) +
           k_B * std::pow(nu_minus, 1.0 + psi_cost);
}

// Trilinear interpolation on a flat 3D array with boundary clamping
// Grid is indexed as V[is * n_I * n_Y + ii * n_Y + iy]
double trilinear_interp(
    const std::vector<double>& V,
    const std::vector<double>& grid_x, int n_x,
    const std::vector<double>& grid_y, int n_y,
    const std::vector<double>& grid_z, int n_z,
    double x, double y, double z
);

#endif
