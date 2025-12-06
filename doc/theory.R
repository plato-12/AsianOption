## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(AsianOptPI)

## -----------------------------------------------------------------------------
# Price with n = 2
price_geometric_asian(
  S0 = 100, K = 100, r = 1.05,
  u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1,
  n = 2
)

## -----------------------------------------------------------------------------
bounds <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05,
  u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1,
  n = 3
)

print(bounds)

## -----------------------------------------------------------------------------
# Valid parameters
check_no_arbitrage(r = 1.05, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)

# Invalid: r too high
check_no_arbitrage(r = 2.0, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)

## -----------------------------------------------------------------------------
# Auto-selects exact method
price_small <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 10
)
cat("n=10 (exact):", price_small, "\n")

# Auto-selects Monte Carlo
price_large <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 30, seed = 42
)
cat("n=30 (MC):", price_large, "\n")

## -----------------------------------------------------------------------------
mc_result <- price_geometric_asian_mc(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 25,
  n_simulations = 50000, seed = 123
)

print(mc_result)

