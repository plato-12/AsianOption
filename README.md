# AsianOption: Asian Option Pricing under Price Impact

<!-- badges: start -->

[![R-CMD-check](https://github.com/plato-12/AsianOption/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/plato-12/AsianOption/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/AsianOption)](https://CRAN.R-project.org/package=AsianOption)

<!-- badges: end -->

## Overview

AsianOption implements valuation of Asian options under transient and
permanent market impact, as described in Tiwari and Majumdar (2025). The
package provides three complementary pricing approaches:

1. **Kemna-Vorst benchmark** — frictionless Black-Scholes-type pricing
   for geometric and arithmetic Asian options.
2. **Exogenous diffusion** — passive trading regime where the impact
   state evolves as an exogenous Ornstein-Uhlenbeck process. Closed-form
   for geometric Asians; Monte Carlo for arithmetic Asians.
3. **Endogenous HJB** — strategic trading regime solved via a tree-based
   Bellman recursion, producing indifference bid and ask prices.

## Installation

``` r
# Install from CRAN
install.packages("AsianOption")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("plato-12/AsianOption")
```

## Quick Start

### Kemna-Vorst Benchmark (Frictionless)

``` r
library(AsianOption)

# Geometric Asian call (closed-form)
price_kemna_vorst_geometric(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, Time = 1
)

# Arithmetic Asian call (Monte Carlo)
price_kemna_vorst_arithmetic(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, Time = 1
)
```

### Exogenous Diffusion (with Impact)

``` r
# Geometric Asian call — closed-form (Theorem 3.2)
price_geometric_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
  lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.5, rho = 0
)

# Arithmetic Asian call — Monte Carlo
price_arithmetic_asian_diffusion(
  S0 = 100, K = 100, r = 0.05, sigma = 0.2, T_mat = 1,
  lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.5, rho = 0
)
```

### Endogenous HJB (Strategic Trading)

``` r
# Geometric Asian — bid/ask via Bellman scheme (Algorithm 1)
price_geometric_asian_hjb(
  S0 = 100, K = 100, Time = 1, N = 30,
  sigma = 0.2, r = 0.05, kappa = 1,
  lambda_bar_T = 0.05, lambda_bar_P = 0.025,
  k_A = 0.5, k_B = 0.5, psi_cost = 1.0,
  eta = 0.5, rho = 0, I0 = 0
)

# Arithmetic Asian — bid/ask via Bellman scheme
price_arithmetic_asian_hjb(
  S0 = 100, K = 100, Time = 1, N = 30,
  sigma = 0.2, r = 0.05, kappa = 1,
  lambda_bar_T = 0.05, lambda_bar_P = 0.025,
  k_A = 0.5, k_B = 0.5, psi_cost = 1.0,
  eta = 0.5, rho = 0, I0 = 0
)
```

## Main Functions

- `price_kemna_vorst_geometric()`: Kemna-Vorst geometric Asian (frictionless)
- `price_kemna_vorst_arithmetic()`: Kemna-Vorst arithmetic Asian (frictionless)
- `price_geometric_asian_diffusion()`: Exogenous diffusion geometric Asian (closed-form)
- `price_arithmetic_asian_diffusion()`: Exogenous diffusion arithmetic Asian (Monte Carlo)
- `price_geometric_asian_hjb()`: Endogenous HJB geometric Asian (Bellman scheme)
- `price_arithmetic_asian_hjb()`: Endogenous HJB arithmetic Asian (Bellman scheme)

## Citation

If you use this package in your research, please cite:

Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price
impact. *arXiv preprint*.
<https://doi.org/10.48550/arXiv.2512.07154>

## License

GPL (>= 3)
