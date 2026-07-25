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
3. **Endogenous utility-indifference (Bellman scheme)** — a dealer with
   constant absolute risk aversion hedges the option in a market with price
   impact, running a self-financing cash account. Solving the dealer's
   problem with and without the option gives reservation bid and ask quotes.

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

### Endogenous Utility-Indifference (Dealer Hedging under Impact)

``` r
# Geometric Asian — reservation bid/ask for a CARA dealer
res <- price_geometric_asian_indiff(
  S0 = 100, K = 100, T = 1, N = 25,
  sigma = 0.2, r_cont = 0.05,
  gamma = 0.05,                                # dealer risk aversion
  lambda_bar_T = 0.05, lambda_bar_P = 0.025,   # execution impact
  k_A = 0.05, k_B = 0.05, psi_cost = 1,        # temporary execution cost
  kappa_J = 1, Q_bar = 2, nu_bar = 4
)
summary(res)          # quotes plus the numerical diagnostics

# Arithmetic Asian — same interface
price_arithmetic_asian_indiff(
  S0 = 100, K = 100, T = 1, N = 25, sigma = 0.2, r_cont = 0.05
)
```

The execution price is `S + lambda_bar_P * Q + lambda_bar_T * J`, where `Q` is
the dealer's inventory and `J` its transient impact state. This is a **change
of meaning** from the legacy `*_hjb()` functions, where `lambda_bar_*`
appeared in the drift of `S`; the drift loading is now `lambda_I`. See
`NEWS.md`.

Two practical notes. Leave `n_logS = NULL` so the engine aligns the log-price
grid with one shock — an unaligned grid inflates the effective volatility. And
check `inst/scripts/indiff_convergence.R` before quoting a spread, which is
more grid-sensitive than the price level.

## Main Functions

- `price_kemna_vorst_geometric()`: Kemna-Vorst geometric Asian (frictionless)
- `price_kemna_vorst_arithmetic()`: Kemna-Vorst arithmetic Asian (frictionless)
- `price_geometric_asian_diffusion()`: Exogenous diffusion geometric Asian (closed-form)
- `price_arithmetic_asian_diffusion()`: Exogenous diffusion arithmetic Asian (Monte Carlo)
- `price_geometric_asian_indiff()`: Utility-indifference geometric Asian bid/ask
- `price_arithmetic_asian_indiff()`: Utility-indifference arithmetic Asian bid/ask

### Legacy (pre-revision) interface

`price_geometric_asian_hjb()` and `price_arithmetic_asian_hjb()` implement the
earlier cost-minimisation formulation. They are **deprecated** — kept
unchanged so the arXiv v2 numbers remain reproducible, to be made internal in
0.4.0 and removed in 0.5.0. New work should use the `*_indiff()` functions,
which are self-financing, give the bid and ask an explicit economic meaning,
and recover the frictionless benchmark as impact goes to zero.

## Citation

If you use this package in your research, please cite:

Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price
impact. *arXiv preprint*.
<https://doi.org/10.48550/arXiv.2512.07154>

## License

GPL (>= 3)
