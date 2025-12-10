## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(AsianOption)

## -----------------------------------------------------------------------------
# Example: Compute adjusted factors and probability
factors <- compute_adjusted_factors(u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)
cat("u_tilde:", factors$u_tilde, "\n")
cat("d_tilde:", factors$d_tilde, "\n")

p_adj <- compute_p_adj(r = 1.05, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)
cat("p_adj:", p_adj, "\n")

## -----------------------------------------------------------------------------
# Valid parameters
check_no_arbitrage(r = 1.05, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)

# Invalid: r too high
check_no_arbitrage(r = 2.0, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)

## -----------------------------------------------------------------------------
# Price with n = 5 (no impact)
price_no_impact <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0, v_u = 0, v_d = 0, n = 5
)
cat("Price (no impact):", price_no_impact, "\n")

# Price with n = 5 (with impact)
price_with_impact <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 5
)
cat("Price (with impact):", price_with_impact, "\n")
cat("Impact effect:", price_with_impact - price_no_impact, "\n")

## -----------------------------------------------------------------------------
# Call option
call_price <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 5,
  option_type = "call"
)

# Put option
put_price <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 5,
  option_type = "put"
)

cat("Call price:", call_price, "\n")
cat("Put price:", put_price, "\n")

## -----------------------------------------------------------------------------
# Compute bounds (global bound only)
bounds_global <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 3,
  compute_path_specific = FALSE
)

print(bounds_global)

## -----------------------------------------------------------------------------
# Compute bounds with path-specific upper bound (exact enumeration)
bounds_ps <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 5,
  compute_path_specific = TRUE
)

print(bounds_ps)

# Estimate price as midpoint
estimated_price <- mean(c(bounds_ps$lower_bound, bounds_ps$upper_bound_path_specific))
cat("\nEstimated price (midpoint):", estimated_price, "\n")

## -----------------------------------------------------------------------------
# Bounds for put option
bounds_put <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 3,
  option_type = "put"
)

print(bounds_put)

## -----------------------------------------------------------------------------
# Small n: very fast
price_small <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 10
)
cat("n=10 (2^10 = 1,024 paths):", price_small, "\n")

# Larger n: still feasible
price_large <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 15
)
cat("n=15 (2^15 = 32,768 paths):", price_large, "\n")

## -----------------------------------------------------------------------------
# European call with price impact
euro_call <- price_european_call(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 20
)
cat("European call:", euro_call, "\n")

# European put with price impact
euro_put <- price_european_put(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 20
)
cat("European put:", euro_put, "\n")

