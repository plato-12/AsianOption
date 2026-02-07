#include "hjb_bellman.h"

// Find bracketing index and fractional weight for 1D interpolation
// Returns the lower index and sets weight to the fractional position
static int find_bracket(const std::vector<double>& grid, int n, double val, double& weight) {
    if (n <= 1) {
        weight = 0.0;
        return 0;
    }

    // Clamp to grid boundaries
    if (val <= grid[0]) {
        weight = 0.0;
        return 0;
    }
    if (val >= grid[n - 1]) {
        weight = 1.0;
        return n - 2;
    }

    // Uniform grid: compute index directly
    double dx = grid[1] - grid[0];
    int idx = static_cast<int>((val - grid[0]) / dx);
    if (idx >= n - 1) idx = n - 2;
    if (idx < 0) idx = 0;

    weight = (val - grid[idx]) / (grid[idx + 1] - grid[idx]);
    if (weight < 0.0) weight = 0.0;
    if (weight > 1.0) weight = 1.0;

    return idx;
}

// Trilinear interpolation on flat 3D array
double trilinear_interp(
    const std::vector<double>& V,
    const std::vector<double>& grid_x, int n_x,
    const std::vector<double>& grid_y, int n_y,
    const std::vector<double>& grid_z, int n_z,
    double x, double y, double z
) {
    double wx, wy, wz;
    int ix = find_bracket(grid_x, n_x, x, wx);
    int iy = find_bracket(grid_y, n_y, y, wy);
    int iz = find_bracket(grid_z, n_z, z, wz);

    int stride_x = n_y * n_z;
    int stride_y = n_z;

    double c000 = V[ix * stride_x + iy * stride_y + iz];
    double c001 = V[ix * stride_x + iy * stride_y + (iz + 1)];
    double c010 = V[ix * stride_x + (iy + 1) * stride_y + iz];
    double c011 = V[ix * stride_x + (iy + 1) * stride_y + (iz + 1)];
    double c100 = V[(ix + 1) * stride_x + iy * stride_y + iz];
    double c101 = V[(ix + 1) * stride_x + iy * stride_y + (iz + 1)];
    double c110 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + iz];
    double c111 = V[(ix + 1) * stride_x + (iy + 1) * stride_y + (iz + 1)];

    double c00 = c000 * (1.0 - wz) + c001 * wz;
    double c01 = c010 * (1.0 - wz) + c011 * wz;
    double c10 = c100 * (1.0 - wz) + c101 * wz;
    double c11 = c110 * (1.0 - wz) + c111 * wz;

    double c0 = c00 * (1.0 - wy) + c01 * wy;
    double c1 = c10 * (1.0 - wy) + c11 * wy;

    return c0 * (1.0 - wx) + c1 * wx;
}

// Build a uniform grid of n points from lo to hi
static std::vector<double> make_uniform_grid(double lo, double hi, int n) {
    std::vector<double> grid(n);
    if (n == 1) {
        grid[0] = 0.5 * (lo + hi);
        return grid;
    }
    double dx = (hi - lo) / (n - 1);
    for (int i = 0; i < n; i++) {
        grid[i] = lo + i * dx;
    }
    return grid;
}

// Solve one-step Bellman optimization at state (log_s, i_val, y_val, time m)
// given the continuation value function V_next.
// problem_type: 0 = seller (inf), 1 = buyer (sup)
// Returns {best_J, best_nu}
static std::pair<double, double> solve_bellman_step(
    double log_s, double i_val, double y_val,
    int asian_type, int problem_type,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double alpha_curr, double sigma, double r_cont,
    double k_A, double k_B, double psi_cost,
    double dt, double sqrt_dt, double eta_m, double p, double discount,
    const std::vector<double>& control_set, int n_controls,
    const std::vector<double>& V_next,
    const std::vector<double>& logS_grid, int n_logS,
    const std::vector<double>& I_grid, int n_I,
    const std::vector<double>& Y_grid, int n_Y
) {
    double s_val = std::exp(log_s);
    bool is_seller = (problem_type == 0);
    double best_val = is_seller ?
        std::numeric_limits<double>::infinity() :
        -std::numeric_limits<double>::infinity();
    double best_nu = 0.0;

    for (int jc = 0; jc < n_controls; jc++) {
        double nu = control_set[jc];

        // Impact state transitions (eq 30)
        double i_drift = (-kappa * i_val + nu) * dt;
        double i_plus  = i_val + i_drift + eta_m * sqrt_dt;
        double i_minus = i_val + i_drift - eta_m * sqrt_dt;

        // Stock price transitions in log space (eq 31)
        double log_s_drift = (r_cont + lambda_bar_T * alpha_curr * i_val +
                              lambda_bar_P * nu) * dt;
        double log_s_plus  = log_s + log_s_drift + sigma * sqrt_dt;
        double log_s_minus = log_s + log_s_drift - sigma * sqrt_dt;

        // Running average/integral update (eq 32)
        double y_next;
        if (asian_type == 0) {
            y_next = y_val + s_val * dt;
        } else {
            y_next = y_val + log_s * dt;
        }

        // Interpolate continuation values
        double cont_plus = trilinear_interp(
            V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
            log_s_plus, i_plus, y_next
        );
        double cont_minus = trilinear_interp(
            V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
            log_s_minus, i_minus, y_next
        );

        // One-step objective:
        //   Seller: J = +C(nu)*dt + disc*(p*cont+ + (1-p)*cont-)
        //   Buyer:  J = -C(nu)*dt + disc*(p*cont+ + (1-p)*cont-)
        double cost = running_cost(nu, k_A, k_B, psi_cost) * dt;
        double cost_term = is_seller ? cost : -cost;
        double J = cost_term + discount * (p * cont_plus + (1.0 - p) * cont_minus);

        bool is_better = is_seller ? (J < best_val) : (J > best_val);
        if (is_better) {
            best_val = J;
            best_nu = nu;
        }
    }

    return {best_val, best_nu};
}

// Internal backward-induction engine
// asian_type: 0 = arithmetic, 1 = geometric
// problem_type: 0 = seller (inf, cost added), 1 = buyer (sup, cost subtracted)
// store_policy: if true, stores full policy for all time steps
static Rcpp::List hjb_bellman_engine(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    const std::vector<double>& eta_vec,
    double p, double I0,
    const std::vector<double>& control_set,
    int n_logS, int n_I, int n_Y,
    int asian_type,
    int problem_type,
    bool store_policy
) {
    double dt = T / N;
    double sqrt_dt = std::sqrt(dt);
    double alpha_m = 1.0 - kappa * dt;
    double discount = std::exp(-r_cont * dt);
    int n_controls = static_cast<int>(control_set.size());

    // --- Build grids ---
    double log_S0 = std::log(S0);
    double max_drift = std::abs(r_cont) +
        std::abs(lambda_bar_T) * 10.0 + std::abs(lambda_bar_P) * 10.0;
    double margin = max_drift * T + 4.0 * sigma * std::sqrt(T);
    if (margin < 1.0) margin = 1.0;
    std::vector<double> logS_grid = make_uniform_grid(
        log_S0 - margin, log_S0 + margin, n_logS);

    // I grid: OU steady-state bounds
    double nu_abs_max = 0.0;
    for (int j = 0; j < n_controls; j++) {
        if (std::abs(control_set[j]) > nu_abs_max)
            nu_abs_max = std::abs(control_set[j]);
    }
    double eta_max = 0.0;
    for (int m = 0; m < N; m++) {
        if (eta_vec[m] > eta_max) eta_max = eta_vec[m];
    }
    double I_bound = 0.0;
    if (kappa > 1e-12) {
        I_bound = nu_abs_max / kappa +
            4.0 * eta_max / std::sqrt(2.0 * kappa);
    } else {
        I_bound = nu_abs_max * T + 4.0 * eta_max * std::sqrt(T);
    }
    if (I_bound < 1.0) I_bound = 1.0;
    std::vector<double> I_grid = make_uniform_grid(
        -I_bound, I_bound, n_I);

    // Y (or Z) grid
    double S_max_approx = S0 * std::exp(margin);
    double Y_lo, Y_hi;
    if (asian_type == 0) {
        Y_lo = 0.0;
        Y_hi = S_max_approx * T;
    } else {
        double log_S_min = log_S0 - margin;
        Y_lo = std::min(0.0, log_S_min * T);
        Y_hi = (log_S0 + margin) * T;
    }
    std::vector<double> Y_grid = make_uniform_grid(Y_lo, Y_hi, n_Y);

    // --- Allocate value arrays ---
    int grid_size = n_logS * n_I * n_Y;
    std::vector<double> V_next(grid_size, 0.0);
    std::vector<double> V_curr(grid_size, 0.0);

    // Optionally store full policy
    std::vector<double> policy_full;
    if (store_policy) {
        policy_full.resize(static_cast<size_t>(N) * grid_size, 0.0);
    }

    // --- Terminal condition: m = N ---
    for (int is = 0; is < n_logS; is++) {
        for (int ii = 0; ii < n_I; ii++) {
            for (int iy = 0; iy < n_Y; iy++) {
                int idx = is * n_I * n_Y + ii * n_Y + iy;
                double y_val = Y_grid[iy];
                double payoff;
                if (asian_type == 0) {
                    payoff = std::max(y_val / T - K, 0.0);
                } else {
                    payoff = std::max(std::exp(y_val / T) - K, 0.0);
                }
                V_next[idx] = payoff;
            }
        }
    }

    // --- Backward induction ---
    for (int m = N - 1; m >= 0; m--) {
        double eta_m = eta_vec[m];

        for (int is = 0; is < n_logS; is++) {
            double log_s = logS_grid[is];

            for (int ii = 0; ii < n_I; ii++) {
                double i_val = I_grid[ii];

                for (int iy = 0; iy < n_Y; iy++) {
                    double y_val = Y_grid[iy];

                    auto result = solve_bellman_step(
                        log_s, i_val, y_val,
                        asian_type, problem_type,
                        kappa, lambda_bar_T, lambda_bar_P,
                        alpha_m, sigma, r_cont,
                        k_A, k_B, psi_cost,
                        dt, sqrt_dt, eta_m, p, discount,
                        control_set, n_controls,
                        V_next, logS_grid, n_logS,
                        I_grid, n_I, Y_grid, n_Y
                    );

                    int idx = is * n_I * n_Y + ii * n_Y + iy;
                    V_curr[idx] = result.first;

                    if (store_policy) {
                        policy_full[
                            static_cast<size_t>(m) * grid_size + idx
                        ] = result.second;
                    }
                }
            }
        }

        V_next.swap(V_curr);
    }

    // --- Extract value at (log(S0), I0, 0) ---
    double value = trilinear_interp(
        V_next, logS_grid, n_logS, I_grid, n_I, Y_grid, n_Y,
        log_S0, I0, 0.0
    );

    // --- Find optimal initial control via V_1 (in V_curr after swap) ---
    auto result0 = solve_bellman_step(
        log_S0, I0, 0.0,
        asian_type, problem_type,
        kappa, lambda_bar_T, lambda_bar_P,
        alpha_m, sigma, r_cont,
        k_A, k_B, psi_cost,
        dt, sqrt_dt, eta_vec[0], p, discount,
        control_set, n_controls,
        V_curr, logS_grid, n_logS,
        I_grid, n_I, Y_grid, n_Y
    );
    double best_nu_0 = result0.second;

    // --- Forward pass: extract optimal trading rates along expected path ---
    // At each step, use the value function V_{m+1} to find nu*(m, state)
    // and advance the state using the p-weighted expected transition.
    // We need V at every time step, so we must re-run backward induction
    // and store all V_m. We already have V_0 in V_next and V_1 in V_curr.
    // Instead, for efficiency, we re-do backward induction storing all V_m.

    // Store all value functions: V[m] for m = 0, ..., N
    // V[N] = terminal, V[0] = initial
    std::vector<std::vector<double>> V_all(N + 1,
        std::vector<double>(grid_size, 0.0));

    // Terminal
    for (int is = 0; is < n_logS; is++) {
        for (int ii = 0; ii < n_I; ii++) {
            for (int iy = 0; iy < n_Y; iy++) {
                int idx = is * n_I * n_Y + ii * n_Y + iy;
                double y_val = Y_grid[iy];
                double payoff;
                if (asian_type == 0) {
                    payoff = std::max(y_val / T - K, 0.0);
                } else {
                    payoff = std::max(std::exp(y_val / T) - K, 0.0);
                }
                V_all[N][idx] = payoff;
            }
        }
    }

    // Backward pass storing all V_m
    for (int m = N - 1; m >= 0; m--) {
        double eta_m = eta_vec[m];
        for (int is = 0; is < n_logS; is++) {
            double log_s = logS_grid[is];
            for (int ii = 0; ii < n_I; ii++) {
                double i_val = I_grid[ii];
                for (int iy = 0; iy < n_Y; iy++) {
                    double y_val = Y_grid[iy];
                    auto res = solve_bellman_step(
                        log_s, i_val, y_val,
                        asian_type, problem_type,
                        kappa, lambda_bar_T, lambda_bar_P,
                        alpha_m, sigma, r_cont,
                        k_A, k_B, psi_cost,
                        dt, sqrt_dt, eta_m, p, discount,
                        control_set, n_controls,
                        V_all[m + 1], logS_grid, n_logS,
                        I_grid, n_I, Y_grid, n_Y
                    );
                    int idx = is * n_I * n_Y + ii * n_Y + iy;
                    V_all[m][idx] = res.first;
                }
            }
        }
    }

    // Forward pass along expected path
    std::vector<double> optimal_nu(N, 0.0);
    double log_s_fwd = log_S0;
    double i_fwd = I0;
    double y_fwd = 0.0;

    for (int m = 0; m < N; m++) {
        double eta_m = eta_vec[m];

        // Solve for optimal nu at current state using V_{m+1}
        auto res = solve_bellman_step(
            log_s_fwd, i_fwd, y_fwd,
            asian_type, problem_type,
            kappa, lambda_bar_T, lambda_bar_P,
            alpha_m, sigma, r_cont,
            k_A, k_B, psi_cost,
            dt, sqrt_dt, eta_m, p, discount,
            control_set, n_controls,
            V_all[m + 1], logS_grid, n_logS,
            I_grid, n_I, Y_grid, n_Y
        );
        double nu_star = res.second;
        optimal_nu[m] = nu_star;

        // Advance state using p-weighted expected transition
        double i_drift = (-kappa * i_fwd + nu_star) * dt;
        double i_up   = i_fwd + i_drift + eta_m * sqrt_dt;
        double i_down = i_fwd + i_drift - eta_m * sqrt_dt;
        i_fwd = p * i_up + (1.0 - p) * i_down;  // = i_fwd + i_drift

        double log_s_drift = (r_cont + lambda_bar_T * alpha_m * i_fwd +
                              lambda_bar_P * nu_star) * dt;
        double log_s_up   = log_s_fwd + log_s_drift + sigma * sqrt_dt;
        double log_s_down = log_s_fwd + log_s_drift - sigma * sqrt_dt;
        double s_fwd_curr = std::exp(log_s_fwd);

        // Y update (same for up/down)
        if (asian_type == 0) {
            y_fwd = y_fwd + s_fwd_curr * dt;
        } else {
            y_fwd = y_fwd + log_s_fwd * dt;
        }

        log_s_fwd = p * log_s_up + (1.0 - p) * log_s_down;
        // = log_s_fwd + log_s_drift
    }

    // Compute optimal volumes = nu * dt
    std::vector<double> optimal_volumes(N);
    for (int m = 0; m < N; m++) {
        optimal_volumes[m] = optimal_nu[m] * dt;
    }

    Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("value") = value,
        Rcpp::Named("optimal_nu_0") = best_nu_0,
        Rcpp::Named("optimal_nu") = optimal_nu,
        Rcpp::Named("optimal_volumes") = optimal_volumes,
        Rcpp::Named("logS_grid") = logS_grid,
        Rcpp::Named("I_grid") = I_grid,
        Rcpp::Named("Y_grid") = Y_grid
    );

    if (store_policy) {
        out["policy"] = policy_full;
        out["policy_dims"] = Rcpp::IntegerVector::create(
            N, n_logS, n_I, n_Y);
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::List hjb_arithmetic_asian_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y,
    int problem_type
) {
    std::vector<double> eta(eta_vec.begin(), eta_vec.end());
    std::vector<double> controls(control_set.begin(), control_set.end());
    return hjb_bellman_engine(S0, K, T, N, sigma, r_cont,
                              kappa, lambda_bar_T, lambda_bar_P,
                              k_A, k_B, psi_cost,
                              eta, p, I0, controls,
                              n_logS, n_I, n_Y,
                              0, problem_type, false);
}

// [[Rcpp::export]]
Rcpp::List hjb_geometric_asian_cpp(
    double S0, double K, double T, int N,
    double sigma, double r_cont,
    double kappa, double lambda_bar_T, double lambda_bar_P,
    double k_A, double k_B, double psi_cost,
    Rcpp::NumericVector eta_vec,
    double p, double I0,
    Rcpp::NumericVector control_set,
    int n_logS, int n_I, int n_Y,
    int problem_type
) {
    std::vector<double> eta(eta_vec.begin(), eta_vec.end());
    std::vector<double> controls(control_set.begin(), control_set.end());
    return hjb_bellman_engine(S0, K, T, N, sigma, r_cont,
                              kappa, lambda_bar_T, lambda_bar_P,
                              k_A, k_B, psi_cost,
                              eta, p, I0, controls,
                              n_logS, n_I, n_Y,
                              1, problem_type, false);
}
