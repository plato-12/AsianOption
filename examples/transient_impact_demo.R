# Demonstration of Transient Impact Model
# ========================================

# Load package from development directory
devtools::load_all()

cat("=== Transient Impact Model Demo ===\n\n")

# Example 1: Basic Transient Impact Pricing
cat("Example 1: Basic Geometric Asian Option with Transient Impact\n")
cat("--------------------------------------------------------------\n")

volumes <- rep(1, 10)
price_transient <- price_geometric_asian_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05,   # Permanent impact
  lambda_T = 0.05,   # Transient impact
  alpha = 0.5,       # Moderate decay
  psi = 1.0,         # Linear volume impact
  volumes = volumes,
  option_type = "call"
)

cat(sprintf("Transient Impact Price: %.4f\n", price_transient))

# Compare with permanent impact only
price_permanent <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.05,  # Only permanent
  v_u = 1, v_d = 1,
  n = 10,
  option_type = "call"
)

cat(sprintf("Permanent Only Price:  %.4f\n", price_permanent))
cat(sprintf("Difference:            %.4f\n\n", price_transient - price_permanent))


# Example 2: Effect of Decay Parameter (alpha)
cat("Example 2: Effect of Transient Decay Rate (alpha)\n")
cat("--------------------------------------------------\n")

alphas <- c(0, 0.3, 0.5, 0.7, 0.9)
volumes <- rep(1, 8)

cat("alpha  Price     Description\n")
cat("-----  --------  -----------\n")

for (a in alphas) {
  price <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = a, psi = 1,
    volumes = volumes
  )

  desc <- if (a == 0) {
    "No memory"
  } else if (a < 0.5) {
    "Short memory"
  } else if (a < 0.8) {
    "Moderate memory"
  } else {
    "Long memory"
  }

  cat(sprintf("%.1f   %.4f   %s\n", a, price, desc))
}
cat("\n")


# Example 3: Different Volume Impact Functions (psi)
cat("Example 3: Volume Impact Power-Law (psi)\n")
cat("-----------------------------------------\n")

# Use volumes > 1 to see the effect
volumes <- rep(2, 6)

cat("psi   Price     Impact Type\n")
cat("----  --------  -----------\n")

for (p in c(0.5, 0.75, 1.0)) {
  price <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = p,
    volumes = volumes
  )

  impact_type <- if (p == 0.5) {
    "Square-root"
  } else if (p == 1.0) {
    "Linear"
  } else {
    "Intermediate"
  }

  cat(sprintf("%.2f  %.4f   %s\n", p, price, impact_type))
}
cat("\n")


# Example 4: Time-Varying Volumes
cat("Example 4: Time-Varying Hedging Volumes\n")
cat("----------------------------------------\n")

# Increasing volumes over time
volumes_increasing <- seq(0.5, 2.0, length.out = 8)
price_inc <- price_geometric_asian_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes_increasing
)

# Decreasing volumes over time
volumes_decreasing <- seq(2.0, 0.5, length.out = 8)
price_dec <- price_geometric_asian_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes_decreasing
)

# Constant volumes
volumes_constant <- rep(1.25, 8)
price_const <- price_geometric_asian_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes_constant
)

cat(sprintf("Increasing volumes: %.4f\n", price_inc))
cat(sprintf("Constant volumes:   %.4f\n", price_const))
cat(sprintf("Decreasing volumes: %.4f\n\n", price_dec))


# Example 5: Monte Carlo for Large n
cat("Example 5: Monte Carlo Simulation (n = 25)\n")
cat("-------------------------------------------\n")

volumes <- rep(1, 25)
mc_result <- price_geometric_asian_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes,
  method = "monte_carlo",
  n_simulations = 50000,
  seed = 42
)

print(mc_result)
cat("\n")


# Example 6: Arithmetic Asian Bounds
cat("Example 6: Arithmetic Asian Option Bounds\n")
cat("------------------------------------------\n")

volumes <- rep(1, 8)

# Global bounds (fast)
bounds_global <- arithmetic_asian_bounds_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes,
  compute_path_specific = FALSE
)

cat("\nGlobal Bounds (fast):\n")
print(bounds_global)

# Path-specific bounds (tighter but slower)
bounds_path <- arithmetic_asian_bounds_transient(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda_P = 0.05, lambda_T = 0.05,
  alpha = 0.5, psi = 1,
  volumes = volumes,
  compute_path_specific = TRUE
)

cat("\nPath-Specific Bounds (tighter):\n")
print(bounds_path)

cat("\n=== Demo Complete ===\n")
