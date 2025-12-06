# AsianOption 0.1.0

## New Features (December 2025)

### Monte Carlo Simulation for Large n

- **NEW**: `price_geometric_asian_mc()`: Monte Carlo pricing for large n (> 20)
  - Efficient simulation-based pricing
  - Handles arbitrarily large n (tested up to n=100+)
  - Returns price, standard error, and 95% confidence interval
  - Reproducible results with seed parameter
  - Typical accuracy: < 0.5% error with 100,000 simulations

- **ENHANCED**: `price_geometric_asian()`: Automatic method selection
  - Auto-selects exact enumeration for n ≤ 20
  - Auto-selects Monte Carlo for n > 20
  - Manual override available via `method` parameter
  - Seamless integration with existing code

### Performance Improvements

- **Exact method**: Unchanged, optimal for n ≤ 20
- **Monte Carlo**: Extends practical range to n = 100+ time steps
  - n=30: ~0.1 seconds (vs. 1 billion paths with exact)
  - n=50: ~0.2 seconds (vs. 10^15 paths with exact)
  - n=100: ~0.4 seconds (impossible with exact method)

### Additional Enhancements

- **Put option support**: Both call and put options now fully supported
- **S3 print method**: Pretty printing for Monte Carlo results
- **Enhanced documentation**: Complete Monte Carlo examples and theory
- **43 new tests**: Comprehensive test coverage for MC implementation

## Initial Release (November 2025)

This is the initial development release of AsianOption, implementing binomial tree pricing for Asian options with market price impact from hedging activities.

### Core Features

#### Pricing Functions
- `price_geometric_asian()`: Exact pricing for geometric Asian call options
  - Complete path enumeration algorithm
  - Efficient C++ implementation via Rcpp
  - Handles up to n = 20 time steps comfortably

- `arithmetic_asian_bounds()`: Upper and lower bounds for arithmetic Asian options
  - Lower bound using AM-GM inequality
  - Upper bound via reverse Jensen's inequality
  - Returns comprehensive bound information (lower, upper, ρ*, E^Q[G_n])

#### Utility Functions
- `compute_p_adj()`: Compute adjusted risk-neutral probability with price impact
- `compute_adjusted_factors()`: Calculate modified up/down factors (ũ, d̃)
- `check_no_arbitrage()`: Validate no-arbitrage condition d̃ < r < ũ

#### Price Impact Model
- Incorporates hedging-induced stock price movements: ΔS = λ·v·sign(trade)
- Modifies binomial tree with adjusted factors: ũ = u·e^(λv^u), d̃ = d·e^(-λv^d)
- Adjusts risk-neutral probability: p^adj = (r - d̃)/(ũ - d̃)

### Input Validation

Comprehensive parameter validation including:
- Positivity checks (S0, K, r, u, d must be positive)
- Non-negativity for price impact parameters (λ, v_u, v_d ≥ 0)
- Ordering constraint (u > d)
- **Critical no-arbitrage validation** (d̃ < r < ũ)
- Risk-neutral probability bounds (p^adj ∈ [0,1])
- Performance warnings for large n (> 20)

### Documentation

#### Package Documentation
- Comprehensive package-level documentation in `?AsianOption`
- Mathematical framework with LaTeX formulas
- Price impact mechanism explained
- No-arbitrage condition detailed
- Computational complexity analysis

#### Function Documentation
- Complete Roxygen documentation for all functions
- All parameters documented with @param
- Return values specified with @return
- Mathematical details in @details sections
- Working examples in @examples
- Academic references in @references

#### README
- Quick start guide with examples
- Detailed mathematical explanations
- Usage examples for all functions
- Sensitivity analysis demonstrations
- Error handling examples
- Performance benchmarks

### Implementation Details

#### C++ Core (src/)
- `utils.cpp`: Core utility functions
  - Adjusted factor calculations
  - Geometric and arithmetic mean computations
  - Price path generation

- `geometric_asian.cpp`: Geometric Asian option pricing
  - Recursive binary path enumeration (2^n paths)
  - Risk-neutral valuation with price impact
  - Optimized for performance

- `arithmetic_bounds.cpp`: Arithmetic Asian bounds
  - Jensen's inequality implementation
  - Spread parameter ρ* calculation
  - Expected geometric average computation

#### R Wrapper Layer (R/)
- `validation.R`: Input validation module
- `price_impact_utils.R`: Utility function wrappers
- `geometric_asian.R`: Geometric pricing wrapper
- `arithmetic_asian.R`: Arithmetic bounds wrapper with S3 print method

### Performance

- **Fast**: n ≤ 15 completes in < 1 second
- **Acceptable**: n = 20 takes ~10 seconds (1 million paths)
- **Warning**: Automatic warning issued for n > 20
- **Efficient**: C++11 implementation with Rcpp integration

### Testing

- All core functions tested and validated
- Mathematical properties verified:
  - Price with impact > Price without impact (for calls)
  - Lower bound ≤ Upper bound
  - ρ* ≥ 1 (required by theory)
  - No-arbitrage condition enforcement

- Input validation thoroughly tested:
  - Catches invalid parameters (negative values, u ≤ d, etc.)
  - Detects no-arbitrage violations
  - Issues performance warnings

### S3 Methods

- `print.arithmetic_bounds()`: Pretty printing for bounds objects
  - Formatted table display
  - Shows all key metrics
  - Includes midpoint estimate

### Development Status

**Completed Phases**:
- ✅ Phase 1: Package skeleton and infrastructure
- ✅ Phase 2: Core C++ implementation
- ✅ Phase 3: R wrapper functions with validation
- ✅ Phase 4: Enhanced documentation

**Planned for Future Releases**:
- Phase 5: Comprehensive unit testing with testthat
- Phase 6: Theory and examples vignettes
- Phase 7: CRAN compliance checks
- Phase 8: CRAN submission

### Dependencies

- R (>= 4.0.0)
- Rcpp (>= 1.0.0)

### References

**Primary Model**:
Cox, J. C., Ross, S. A., & Rubinstein, M. (1979). Option pricing: A simplified approach. *Journal of Financial Economics*, 7(3), 229-263.

**Bounds Theory**:
Budimir, I., Dragomir, S. S., & Pečarić, J. (2000). Further reverse results for Jensen's discrete inequality and applications in information theory. *Journal of Inequalities in Pure and Applied Mathematics*, 2(1).

### Notes

- This is a development release implementing core functionality
- Package uses gross rates (r = 1.05 for 5%, not r = 0.05)
- All functions automatically validate no-arbitrage conditions
- C++11 standard required for compilation
- Comprehensive error messages for invalid inputs

### Known Limitations

- **Exact method**: Path enumeration complexity O(2^n) limits practical use to n ≤ 20
  - Memory usage: O(n·2^n) for storing all paths
  - Automatic fallback to Monte Carlo for n > 20
- **Monte Carlo method**: Provides estimates with quantifiable error
  - Standard error typically < 0.5% with default 100,000 simulations
  - Increase n_simulations for higher precision if needed
- Single option type per call (no portfolio pricing yet)

### Acknowledgments

Development supported by comprehensive mathematical analysis and rigorous testing procedures. Implementation follows CRAN best practices for R package development.
