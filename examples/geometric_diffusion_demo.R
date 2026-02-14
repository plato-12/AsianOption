# Demonstration of price_geometric_asian_diffusion function
# This implements Section 3.2.3 of the paper:
# "Closed-form expression for the geometric Asian price (exogenous limit)"

# Load the package (use devtools::load_all() during development)
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(AsianOption)
}

cat("========================================================\n")
cat("Geometric Asian Option Pricing in Exogenous Diffusion Limit\n")
cat("========================================================\n\n")

# Example 1: Basic pricing with constant eta, no correlation
cat("Example 1: Basic pricing (constant eta, no correlation)\n")
cat("--------------------------------------------------------\n")
price1 <- price_geometric_asian_diffusion(
  S0 = 100,         # Initial stock price
  K = 100,          # Strike price (at-the-money)
  r = 0.05,         # Risk-free rate
  sigma = 0.2,      # Volatility
  T = 1,            # Time to maturity (1 year)
  lambda_T = 0.01,  # Transient impact coefficient
  I0 = 0,           # Initial impact state
  kappa = 1,        # Mean reversion rate
  eta = 0.1,        # Impact noise amplitude
  rho = 0           # No correlation
)
cat(sprintf("Call price: %.4f\n\n", price1))


# Example 2: Effect of initial impact state
cat("Example 2: Effect of initial impact state\n")
cat("--------------------------------------------------------\n")
I0_values <- c(-1, -0.5, 0, 0.5, 1)
cat("I0\t\tCall Price\n")
for (I0_val in I0_values) {
  price <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = I0_val, kappa = 1, eta = 0.1, rho = 0
  )
  cat(sprintf("%.2f\t\t%.4f\n", I0_val, price))
}
cat("\nNote: Positive I0 increases call price (upward drift)\n\n")


# Example 3: Effect of correlation
cat("Example 3: Effect of correlation between stock and impact\n")
cat("--------------------------------------------------------\n")
rho_values <- c(-0.8, -0.4, 0, 0.4, 0.8)
cat("rho\t\tCall Price\n")
for (rho_val in rho_values) {
  price <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = rho_val
  )
  cat(sprintf("%.2f\t\t%.4f\n", rho_val, price))
}
cat("\n")


# Example 4: Time-dependent impact noise
cat("Example 4: Time-dependent impact noise\n")
cat("--------------------------------------------------------\n")
eta_constant <- function(t) rep(0.1, length(t))
eta_increasing <- function(t) 0.1 * (1 + 0.5 * t)
eta_decreasing <- function(t) 0.1 * (2 - t)

price_const <- price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.02, I0 = 0, kappa = 1, eta = eta_constant, rho = 0.3
)

price_inc <- price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.02, I0 = 0, kappa = 1, eta = eta_increasing, rho = 0.3
)

price_dec <- price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.02, I0 = 0, kappa = 1, eta = eta_decreasing, rho = 0.3
)

cat(sprintf("Constant eta(t) = 0.1:           %.4f\n", price_const))
cat(sprintf("Increasing eta(t) = 0.1*(1+0.5t): %.4f\n", price_inc))
cat(sprintf("Decreasing eta(t) = 0.1*(2-t):    %.4f\n", price_dec))
cat("\n")


# Example 5: Mean reversion rate effect
cat("Example 5: Effect of mean reversion rate (kappa)\n")
cat("--------------------------------------------------------\n")
kappa_values <- c(0.1, 0.5, 1, 2, 5)
cat("kappa\t\tCall Price\n")
for (kappa_val in kappa_values) {
  price <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0.5, kappa = kappa_val, eta = 0.1, rho = 0
  )
  cat(sprintf("%.2f\t\t%.4f\n", kappa_val, price))
}
cat("\nNote: Higher kappa means faster mean reversion,\n")
cat("      reducing the impact of I0 on the price\n\n")


# Example 6: Put option pricing
cat("Example 6: Put option pricing\n")
cat("--------------------------------------------------------\n")
call_price <- price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
  option_type = "call"
)

put_price <- price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
  option_type = "put"
)

cat(sprintf("Call price: %.4f\n", call_price))
cat(sprintf("Put price:  %.4f\n\n", put_price))


# Example 7: Comparison across different strikes
cat("Example 7: Price variation across strikes (call options)\n")
cat("--------------------------------------------------------\n")
K_values <- seq(80, 120, by = 10)
cat("Strike (K)\tCall Price\n")
for (K_val in K_values) {
  price <- price_geometric_asian_diffusion(
    S0 = 100, K = K_val, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.02, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )
  cat(sprintf("%d\t\t%.4f\n", K_val, price))
}
cat("\n")


# Example 8: Effect of transient impact coefficient
cat("Example 8: Effect of transient impact coefficient (lambda_T)\n")
cat("--------------------------------------------------------\n")
lambda_T_values <- c(0, 0.01, 0.02, 0.05, 0.1)
cat("lambda_T\tCall Price\n")
for (lambda_T_val in lambda_T_values) {
  price <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = lambda_T_val, I0 = 0.5, kappa = 1, eta = 0.1, rho = 0.3
  )
  cat(sprintf("%.3f\t\t%.4f\n", lambda_T_val, price))
}
cat("\nNote: Higher lambda_T amplifies the impact of I0,\n")
cat("      increasing price when I0 > 0\n\n")


cat("========================================================\n")
cat("Demonstration complete!\n")
cat("========================================================\n")
