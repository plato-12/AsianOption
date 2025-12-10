# AsianOption 0.1.0

*Initial CRAN release - December 2024*

## Overview

The `AsianOption` package implements exact binomial tree pricing for Asian options
with market price impact from hedging activities, following the Cox-Ross-Rubinstein
(CRR) framework extended to incorporate the Kyle (1985) linear impact model.

## Main Features

### Asian Options with Price Impact

#### Geometric Asian Options

- **`price_geometric_asian()`**: Exact pricing via complete path enumeration
  - Supports both call and put options
  - Handles price impact from hedging activities
  - Efficient C++ implementation (Rcpp)
  - Complete enumeration of all 2^n paths (exact, no sampling)
  - Practical for n ≤ 20 time steps

#### Arithmetic Asian Options

- **`arithmetic_asian_bounds()`**: Rigorous bounds via Jensen's inequality
  - **Lower bound**: Geometric Asian option price (AM-GM inequality)
  - **Global upper bound**: Using worst-case spread parameter ρ*
  - **Path-specific upper bound**: Tighter bounds via exact path enumeration
  - Supports both call and put options
  - Returns comprehensive diagnostics (ρ*, E^Q[G_n], bounds)
  - S3 print method for formatted output

### European Options with Price Impact

- **`price_european_call()`**: European call option pricing
- **`price_european_put()`**: European put option pricing
- **`price_european()`**: Unified interface for calls and puts
- Efficient O(n) computation (vs O(2^n) for Asian options)
- Full price impact support

### Benchmark Pricing Functions

The package includes standard pricing methods without price impact for comparison
and validation:

#### Kemna-Vorst Analytical Solutions

- **`price_kemna_vorst_geometric()`**: Closed-form geometric Asian option price
  - Based on Kemna & Vorst (1990)
  - Continuous-time Black-Scholes framework
  - Supports both call and put options

- **`price_kemna_vorst_geometric_binomial()`**: Binomial parameter interface
  - Converts binomial parameters (u, d, n) to continuous (σ, T)
  - Direct comparison with binomial tree methods

#### Kemna-Vorst Monte Carlo

- **`price_kemna_vorst_arithmetic()`**: MC pricing for arithmetic Asian options
  - Control variate variance reduction (using geometric average)
  - Typically 10-20x variance reduction
  - Returns price, standard error, and confidence intervals
  - S3 methods for printing and summary

- **`price_kemna_vorst_arithmetic_binomial()`**: Binomial parameter interface
  - Handles large n efficiently via simulation
  - Reproducible results with seed parameter

#### Black-Scholes Formulas

- **`price_black_scholes_call()`**: Classic BS call option price
- **`price_black_scholes_put()`**: Classic BS put option price
- **`price_black_scholes_binomial()`**: BS price using binomial parameters
  - Shows convergence of CRR to Black-Scholes as n → ∞

### Price Impact Utilities

- **`compute_p_adj()`**: Adjusted risk-neutral probability with price impact
- **`compute_adjusted_factors()`**: Modified up/down factors (ũ, d̃)
- **`check_no_arbitrage()`**: Validates no-arbitrage condition d̃ < r < ũ

## Mathematical Framework

### Price Impact Model

The package implements the Kyle (1985) linear impact model:

**Impact on stock dynamics:**
```
ũ = u · exp(λ·v_u)
d̃ = d · exp(-λ·v_d)
```

where:
- λ: price impact coefficient
- v_u, v_d: hedging volumes for up/down moves

**Adjusted risk-neutral probability:**
```
p^adj = (r - d̃) / (ũ - d̃)
```

**No-arbitrage condition:**
```
d̃ < r < ũ  ⟹  0 ≤ p^adj ≤ 1
```

### Path Enumeration Algorithm

For geometric Asian options, the package computes exact prices by:

1. Enumerating all 2^n possible paths ω ∈ {U, D}^n
2. Computing geometric average G(ω) for each path
3. Computing payoff: max(0, G(ω) - K) for calls
4. Risk-neutral valuation: V_0 = (1/r^n) Σ P(ω) · Payoff(ω)

**Complexity:** O(2^n) - exact with no approximation error

### Bounds for Arithmetic Asian Options

Since arithmetic Asian options lack closed-form solutions, the package provides
rigorous bounds:

**Lower bound (AM-GM):**
```
V_0^A ≥ V_0^G  (arithmetic ≥ geometric)
```

**Global upper bound:**
```
V_0^A ≤ V_0^G + (ρ* - 1)/r^n · E^Q[G_n]
where ρ* = exp[(ũ^n - d̃^n)² / (4·ũ^n·d̃^n)]
```

**Path-specific upper bound:**
```
V_0^A ≤ V_0^G + (1/r^n) Σ_ω P(ω)·(ρ(ω) - 1)·G_n(ω)
where ρ(ω) = exp[(S_M(ω) - S_m(ω))² / (4·S_m(ω)·S_M(ω))]
```

The path-specific bound is typically much tighter than the global bound.

## Implementation Details

### Performance

**Exact enumeration (geometric Asian, path-specific bounds):**

| n   | Paths        | Time      | Feasibility |
|-----|--------------|-----------|-------------|
| 10  | 1,024        | < 1 ms    | Very fast   |
| 15  | 32,768       | ~10 ms    | Fast        |
| 20  | 1,048,576    | ~1 sec    | Feasible    |
| 25  | 33,554,432   | ~30 sec   | Slow        |

**Recommendation:** Use exact methods for n ≤ 20

**European options:** O(n) complexity - can handle n > 100 efficiently

### C++ Core Implementation

All performance-critical code implemented in C++11 via Rcpp:

- `geometric_asian.cpp`: Path enumeration for geometric Asian pricing
- `arithmetic_bounds.cpp`: Bound computation with path-specific option
- `european_option.cpp`: Efficient European option pricing
- `kemna_vorst_mc.cpp`: Monte Carlo with control variates
- `utils.cpp`: Core utility functions

### Input Validation

Comprehensive parameter validation:

- Positivity: S0, K, r, u, d > 0
- Non-negativity: λ, v_u, v_d ≥ 0
- Ordering: u > d
- **No-arbitrage**: d̃ < r < ũ (critical - automatically checked)
- Probability bounds: 0 ≤ p^adj ≤ 1
- Performance warnings: Issued for n > 20

## Documentation

### Vignettes

- **theory**: Mathematical foundations, price impact model, path enumeration,
  bounds theory, computational complexity
- **examples**: 14 comprehensive examples covering pricing, sensitivity analysis,
  parameter effects, comparative studies, and convergence

### Help Files

- Complete Roxygen2 documentation for all exported functions
- Mathematical details with LaTeX equations
- Working examples for each function
- Cross-references between related functions

## S3 Methods

- `print.arithmetic_bounds()`: Formatted display of bounds
- `print.kemna_vorst_arithmetic()`: Formatted MC results with diagnostics
- `summary.kemna_vorst_arithmetic()`: Detailed MC summary

## Installation and Usage

```r
# Install from CRAN (after acceptance)
install.packages("AsianOption")

# Basic example: Geometric Asian call with price impact
library(AsianOption)

price <- price_geometric_asian(
  S0 = 100,           # Initial price
  K = 100,            # Strike
  r = 1.05,           # Risk-free rate (gross: 5%)
  u = 1.2,            # Up factor
  d = 0.8,            # Down factor
  lambda = 0.1,       # Price impact coefficient
  v_u = 1,            # Up-move hedging volume
  v_d = 1,            # Down-move hedging volume
  n = 10,             # Time steps
  option_type = "call"
)

# Arithmetic Asian bounds with tight path-specific bound
bounds <- arithmetic_asian_bounds(
  S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
  lambda = 0.1, v_u = 1, v_d = 1, n = 10,
  compute_path_specific = TRUE
)
print(bounds)
```

## Rate Convention

**Important:** This package uses **gross rates** following binomial tree convention:
- Use `r = 1.05` for 5% per period (NOT `r = 0.05`)
- Benchmark functions use continuously compounded rates where standard

## System Requirements

- R ≥ 4.0.0
- Rcpp ≥ 1.0.0
- C++11 compiler

## Testing

The package includes comprehensive tests covering:
- Mathematical properties (price relationships, bound ordering)
- No-arbitrage condition enforcement
- Input validation
- Edge cases and error handling
- Numerical accuracy

## References

**Primary reference:**

Tiwari, P., & Majumdar, S. (2024). Asian option valuation under price impact.
*arXiv preprint*. https://doi.org/10.48550/arXiv.2512.07154

**Additional references:**

Cox, J. C., Ross, S. A., & Rubinstein, M. (1979). Option pricing: A simplified
approach. *Journal of Financial Economics*, 7(3), 229-263.

Kyle, A. S. (1985). Continuous auctions and insider trading. *Econometrica*,
53(6), 1315-1335.

Kemna, A. G. Z., & Vorst, A. C. F. (1990). A pricing method for options based
on average asset values. *Journal of Banking and Finance*, 14, 113-129.

## License

GPL (≥ 3)

## Authors

Priyanshu Tiwari <priyanshu.tiwari@duke.edu>

## Bug Reports

Please report issues at: https://github.com/plato-12/AsianOption/issues

---

*This is the initial release implementing core functionality for Asian option
pricing with market price impact. Future releases may include additional features
based on user feedback.*
