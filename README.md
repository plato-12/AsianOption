# AsianOption: Asian Option Pricing with Price Impact

<!-- badges: start -->

[![R-CMD-check](https://github.com/plato-12/AsianOption/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/plato-12/AsianOption/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/AsianOption)](https://CRAN.R-project.org/package=AsianOption)

<!-- badges: end -->

## Overview

AsianOption implements binomial tree pricing for Asian options incorporating market price impact from hedging activities. The package extends the Cox-Ross-Rubinstein (CRR) binomial model to account for price movements caused by large hedging trades.

## Installation

``` r
# Install from CRAN
install.packages("AsianOption")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("plato-12/AsianOption")
```

## Quick Start

### Geometric Asian Option Pricing

``` r
library(AsianOption)

# Price a geometric Asian call option with price impact
price <- price_geometric_asian(
  S0 = 100,      # Initial stock price
  K = 100,       # Strike price
  r = 1.05,      # Gross risk-free rate (5%)
  u = 1.2,       # Up factor
  d = 0.8,       # Down factor
  lambda = 0.1,  # Price impact coefficient
  v_u = 1,       # Hedging volume (up)
  v_d = 1,       # Hedging volume (down)
  n = 10         # Time steps
)
print(price)
```

### Arithmetic Asian Option Bounds

``` r
# Compute bounds for arithmetic Asian options
bounds <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05,
  u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 5
)
print(bounds)
```

### Monte Carlo for Large n

``` r
# For n > 20, Monte Carlo is automatically used
result <- price_geometric_asian(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 50
)
#> Using Monte Carlo method for n=50 (> 20) with 100000 simulations

# Get full Monte Carlo output with error estimates
mc_result <- price_geometric_asian_mc(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 50,
  n_simulations = 100000, seed = 42
)
print(mc_result)
#> Geometric Asian Option Price (Monte Carlo)
#> ==========================================
#> Price:       13.899166
#> Std Error:   0.109300 (0.79%)
#> 95% CI:      [13.684937, 14.113395]
#> Simulations: 100000
```

## Main Functions

-   `price_geometric_asian()`: Price geometric Asian options (calls/puts)
-   `price_geometric_asian_mc()`: Monte Carlo pricing with error estimates
-   `arithmetic_asian_bounds()`: Bounds for arithmetic Asian options
-   `compute_p_adj()`: Compute adjusted risk-neutral probability
-   `check_no_arbitrage()`: Validate no-arbitrage conditions

## Citation

If you use this package in your research, please cite:

Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact. *arXiv preprint*. <https://doi.org/10.48550/arXiv.2512.07154>

## License

GPL (\>= 3)
