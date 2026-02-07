# ============================================================================
# HJB Bellman Algorithm Demonstration
# Endogenous Pricing of Asian Options with Price Impact
# ============================================================================

library(AsianOption)

# Helper function to print results nicely
print_result <- function(result, title) {
  cat("\n")
  cat("========================================\n")
  cat(title, "\n")
  cat("========================================\n")
  cat(sprintf("Ask Price:  %.6f\n", result$ask_price))
  cat(sprintf("Bid Price:  %.6f\n", result$bid_price))
  cat(sprintf("Spread:     %.6f", result$spread))
  if (result$spread < 0) {
    cat("  (manipulation surplus)\n")
  } else {
    cat("\n")
  }
  cat("\n")
}

# ============================================================================
# Example 1: Basic Usage - Arithmetic Asian Option
# ============================================================================

cat("\n")
cat("EXAMPLE 1: Basic Arithmetic Asian Option Pricing\n")
cat("=================================================\n")

result1 <- price_arithmetic_asian_hjb(
  S0 = 100,           # Initial stock price
  K = 100,            # Strike price (ATM)
  T = 1,              # 1 year maturity
  N = 20,             # 10 time steps
  sigma = 0.2,        # 20% volatility
  r_cont = 0.05,      # 5% risk-free rate
  kappa = 1,          # Mean reversion rate
  lambda_bar_T = 0.1, # Transient impact coefficient
  lambda_bar_P = 0.1,# Permanent impact coefficient
  k_A = 1.0,         # Buy-side cost coefficient
  k_B = 1.0,         # Sell-side cost coefficient
  psi_cost = 1,       # Linear cost (psi = 1)
  n_I = 21,           # Grid size for impact state
  n_Y = 21            # Grid size for running average
)

print(result1)

# Optimal trading schedule (only if policy was returned)
if (!is.null(result1$optimal_nu) && length(result1$optimal_nu) == result1$params$N) {
  dt <- result1$params$T / result1$params$N
  schedule1 <- data.frame(
    period = 0:(result1$params$N - 1),
    time = (0:(result1$params$N - 1)) * dt,
    seller_nu = result1$optimal_nu,
    seller_volume = result1$optimal_volumes,
    buyer_nu = result1$optimal_nu_buyer,
    buyer_volume = result1$optimal_volumes_buyer
  )
  cat("\nOptimal Trading Schedule (first 5 periods):\n")
  print(head(schedule1, 5))
} else {
  cat("\nOptimal trading schedule not computed (policy not returned by default).\n")
}

# ============================================================================
# Example 2: Zero Price Impact (Benchmark)
# ============================================================================

cat("\n\nEXAMPLE 2: Zero Price Impact - Standard Option Pricing\n")
cat("=======================================================\n")

result2 <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 0,            # No mean reversion needed
  lambda_bar_T = 0,     # No transient impact
  lambda_bar_P = 0,     # No permanent impact
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 11, n_Y = 21, n_logS = 21
)

print_result(result2, "Zero Impact Case")
cat("Note: With no price impact, bid = ask.\n")
if (!is.null(result2$optimal_nu)) {
  cat("Seller's nu:", unique(result2$optimal_nu), "\n")
  cat("Buyer's nu:", unique(result2$optimal_nu_buyer), "\n")
}

# ============================================================================
# Example 3: Effect of Trading Costs
# ============================================================================

cat("\n\nEXAMPLE 3: Impact of Trading Costs on Manipulation\n")
cat("===================================================\n")

# Low cost configuration
low_cost <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.001, k_B = 0.001, psi_cost = 1,
  n_I = 21, n_Y = 21, n_logS = 21
)

# Medium cost configuration
med_cost <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.1, k_B = 0.1, psi_cost = 1,
  n_I = 21, n_Y = 21, n_logS = 21
)

# High cost configuration
high_cost <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 1.0, k_B = 1.0, psi_cost = 1,
  n_I = 21, n_Y = 21, n_logS = 21
)

cat("\nCost Comparison:\n")
cat("================\n")
comparison <- data.frame(
  Cost_Level = c("Low (k=0.001)", "Medium (k=0.1)", "High (k=1.0)"),
  Ask = c(low_cost$ask_price, med_cost$ask_price, high_cost$ask_price),
  Bid = c(low_cost$bid_price, med_cost$bid_price, high_cost$bid_price),
  Spread = c(low_cost$spread, med_cost$spread, high_cost$spread)
)
if (!is.null(low_cost$optimal_nu)) {
  comparison$Max_Seller_Nu <- c(
    max(abs(low_cost$optimal_nu)),
    max(abs(med_cost$optimal_nu)),
    max(abs(high_cost$optimal_nu))
  )
}
print(comparison)
cat("\nObservation: Higher costs → less manipulation → spread closer to 0\n")

# ============================================================================
# Example 4: Moneyness Effect
# ============================================================================

cat("\n\nEXAMPLE 4: Effect of Moneyness (Strike Price)\n")
cat("==============================================\n")

strikes <- c(80, 90, 100, 110, 120)
results_by_strike <- lapply(strikes, function(K) {
  price_arithmetic_asian_hjb(
    S0 = 100, K = K, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 21, n_Y = 21
  )
})

moneyness_df <- data.frame(
  Strike = strikes,
  Moneyness = strikes / 100,
  Ask = sapply(results_by_strike, function(r) r$ask_price),
  Bid = sapply(results_by_strike, function(r) r$bid_price),
  Spread = sapply(results_by_strike, function(r) r$spread)
)
cat("\nPrices by Strike:\n")
print(moneyness_df)
cat("\nObservation: As strike increases, option value decreases (calls)\n")

# ============================================================================
# Example 5: Geometric vs Arithmetic Asian
# ============================================================================

cat("\n\nEXAMPLE 5: Geometric vs Arithmetic Asian Options\n")
cat("=================================================\n")

arith <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 21, n_Y = 21
)

geom <- price_geometric_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 21, n_Z = 21  # Note: n_Z for geometric
)

cat("\nArithmetic Asian:\n")
cat(sprintf("  Ask: %.6f, Bid: %.6f, Spread: %.6f\n",
            arith$ask_price, arith$bid_price, arith$spread))

cat("\nGeometric Asian:\n")
cat(sprintf("  Ask: %.6f, Bid: %.6f, Spread: %.6f\n",
            geom$ask_price, geom$bid_price, geom$spread))

cat("\nObservation: Arithmetic average ≥ Geometric average\n")
cat("                → Arithmetic call ≥ Geometric call\n")

# ============================================================================
# Example 6: Time-Varying Noise Trader Intensity
# ============================================================================

cat("\n\nEXAMPLE 6: Time-Varying Noise Trader Intensity\n")
cat("===============================================\n")

# Decreasing noise trader activity over time
eta_decreasing <- seq(2.0, 0.5, length.out = 10)

result_tv <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  eta = eta_decreasing,  # Vector of length N
  n_I = 21, n_Y = 21
)

cat("\nNoise trader intensity (eta) over time:\n")
cat(sprintf("  Periods 0-4: %.2f, %.2f, %.2f, %.2f, %.2f\n",
            eta_decreasing[1], eta_decreasing[2], eta_decreasing[3],
            eta_decreasing[4], eta_decreasing[5]))
cat(sprintf("  Periods 5-9: %.2f, %.2f, %.2f, %.2f, %.2f\n",
            eta_decreasing[6], eta_decreasing[7], eta_decreasing[8],
            eta_decreasing[9], eta_decreasing[10]))

print_result(result_tv, "Time-Varying Noise Trader Case")

# ============================================================================
# Example 7: Square-Root Cost vs Linear Cost
# ============================================================================

cat("\n\nEXAMPLE 7: Cost Function Comparison\n")
cat("====================================\n")

# Square-root cost (psi = 0.5): C(nu) ~ |nu|^1.5
sqrt_cost <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 0.5,
  n_I = 21, n_Y = 21
)

# Linear cost (psi = 1.0): C(nu) ~ |nu|^2
linear_cost <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1.0,
  n_I = 21, n_Y = 21
)

cat("\nSquare-Root Cost (psi = 0.5):\n")
cat(sprintf("  Ask: %.6f, Spread: %.6f\n", sqrt_cost$ask_price, sqrt_cost$spread))
if (!is.null(sqrt_cost$optimal_nu)) {
  cat(sprintf("  Max |nu|: %.4f\n", max(abs(sqrt_cost$optimal_nu))))
}

cat("\nLinear Cost (psi = 1.0):\n")
cat(sprintf("  Ask: %.6f, Spread: %.6f\n", linear_cost$ask_price, linear_cost$spread))
if (!is.null(linear_cost$optimal_nu)) {
  cat(sprintf("  Max |nu|: %.4f\n", max(abs(linear_cost$optimal_nu))))
}

cat("\nObservation: Lower psi → less convex cost → more aggressive trading\n")

# ============================================================================
# Example 8: Impact Decay Rate (kappa)
# ============================================================================

cat("\n\nEXAMPLE 8: Effect of Transient Impact Decay Rate\n")
cat("=================================================\n")

kappas <- c(0.1, 0.5, 1.0, 2.0, 5.0)
results_by_kappa <- lapply(kappas, function(kappa_val) {
  price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = kappa_val,
    lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 21, n_Y = 21
  )
})

kappa_df <- data.frame(
  Kappa = kappas,
  Half_Life = log(2) / kappas,  # Approximate half-life
  Ask = sapply(results_by_kappa, function(r) r$ask_price),
  Spread = sapply(results_by_kappa, function(r) r$spread)
)
cat("\nPrices by Decay Rate:\n")
print(kappa_df)
cat("\nObservation: Higher kappa → faster decay → impact less persistent\n")

# ============================================================================
# Example 9: Transient vs Permanent Impact Mix
# ============================================================================

cat("\n\nEXAMPLE 9: Transient vs Permanent Impact Composition\n")
cat("=====================================================\n")

# Pure transient
pure_transient <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.15, lambda_bar_P = 0.0,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 21, n_Y = 21
)

# Mixed
mixed <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 21, n_Y = 21
)

# Pure permanent
pure_permanent <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 0, lambda_bar_T = 0.0, lambda_bar_P = 0.15,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  n_I = 21, n_Y = 21
)

impact_comparison <- data.frame(
  Impact_Type = c("Pure Transient", "Mixed", "Pure Permanent"),
  Lambda_T = c(0.15, 0.1, 0.0),
  Lambda_P = c(0.0, 0.05, 0.15),
  Ask = c(pure_transient$ask_price, mixed$ask_price, pure_permanent$ask_price),
  Spread = c(pure_transient$spread, mixed$spread, pure_permanent$spread)
)
cat("\n")
print(impact_comparison)

# ============================================================================
# Example 10: Custom Control Grid
# ============================================================================

cat("\n\nEXAMPLE 10: Custom Control Grid\n")
cat("================================\n")

# Fine control grid near zero (where optimal nu might be)
custom_grid <- c(seq(-5, -1, by = 1),
                 seq(-0.9, 0.9, by = 0.1),
                 seq(1, 5, by = 1))

result_custom <- price_arithmetic_asian_hjb(
  S0 = 100, K = 100, T = 1, N = 10,
  sigma = 0.2, r_cont = 0.05,
  kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
  k_A = 0.01, k_B = 0.01, psi_cost = 1,
  control_set = custom_grid,  # Use custom grid
  n_I = 21, n_Y = 21
)

cat("\nUsing custom control grid with", length(custom_grid), "points\n")
cat("Grid has finer resolution near zero: -0.9, ..., -0.1, 0, 0.1, ..., 0.9\n")
print_result(result_custom, "Custom Control Grid Result")

# ============================================================================
# Summary and Key Takeaways
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("KEY TAKEAWAYS FROM DEMONSTRATIONS\n")
cat("============================================================================\n")
cat("\n")
cat("1. ENDOGENOUS MODEL CREATES MANIPULATION SURPLUS\n")
cat("   - Bid > Ask (negative spread) is expected when impact is significant\n")
cat("   - Seller sells (nu < 0) to push price down\n")
cat("   - Buyer buys (nu > 0) to push price up\n")
cat("\n")
cat("2. ZERO IMPACT → ZERO SPREAD\n")
cat("   - When lambda_bar_T = lambda_bar_P = 0, bid = ask\n")
cat("   - Both parties choose nu = 0 (trading only costs money)\n")
cat("\n")
cat("3. HIGHER COSTS → LESS MANIPULATION\n")
cat("   - Increasing k_A, k_B makes spread less negative (closer to 0)\n")
cat("   - High costs force nu → 0\n")
cat("\n")
cat("4. DECAY RATE (KAPPA) AFFECTS IMPACT PERSISTENCE\n")
cat("   - Higher kappa → faster decay → less memory of past trades\n")
cat("   - Lower kappa → longer memory → more like permanent impact\n")
cat("\n")
cat("5. MONEYNESS AFFECTS VALUE AS EXPECTED\n")
cat("   - For calls: Lower strike → higher value\n")
cat("   - Impact and manipulation patterns consistent across strikes\n")
cat("\n")
cat("6. ARITHMETIC ≥ GEOMETRIC (BY JENSEN'S INEQUALITY)\n")
cat("   - Arithmetic average ≥ Geometric average\n")
cat("   - Arithmetic Asian call ≥ Geometric Asian call\n")
cat("\n")
cat("7. TIME-VARYING PARAMETERS ARE SUPPORTED\n")
cat("   - eta can vary across time periods (vector of length N)\n")
cat("   - Allows modeling changing market conditions\n")
cat("\n")
cat("8. COST FUNCTION SHAPE MATTERS\n")
cat("   - psi controls convexity: psi=0.5 (sqrt), psi=1.0 (quadratic)\n")
cat("   - Less convex (lower psi) → more aggressive optimal trading\n")
cat("\n")
cat("============================================================================\n")
cat("\nDemonstration complete!\n")
cat("See HJB_ALGORITHM_USAGE.md for detailed documentation.\n")
cat("============================================================================\n")
