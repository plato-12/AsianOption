# AsianOption 0.4.1

## The arithmetic accumulator grid now tracks its own distribution

`a = int_0^T (S_u / S_0) du` is not a log-scale quantity, but its grid was being
sized as `[0, T * exp(drift + k * sd)]` — the log-return bound, exponentiated.
That inflated the span, while the hard floor `a >= 0` anchored the grid many
standard deviations below where the mass is. At `sigma = 0.10` the result was an
accumulator cell about **2.2 times wider** than the geometric one at the same
`n_R`, making the low-volatility arithmetic quotes the least well resolved in
the table: the arithmetic spread came out *below* the geometric spread, which
the greater variance of the arithmetic average rules out.

The grid is now sized from the moments of the arithmetic average itself — the
discrete counterpart of the Turnbull-Wakeman / Kemna-Vorst arithmetic-Asian
moments, computed with the engine's own quadrature weights so it is exact for a
time-varying `mu`, either `accum_rule`, and discrete monitoring. Its origin
tracks the accumulator's mean path per step, exactly as `grid_drift` already
does for the log-price grid, so the width has only to span the spread rather
than the level. The arithmetic cell now matches the geometric one to within a
few per cent at every volatility, and boundary clamping in the accumulator falls
(3.1% to 2.3% at the base case).

**Arithmetic quotes change.** They are better resolved, not merely different: at
`n_R = 121` the new grid reproduces what the old grid needed `n_R = 265` to
reach. **Geometric quotes are bit-identical** — the geometric branch is
untouched and its origin shift is identically zero.

The new `accum_center` argument (default `TRUE`) switches this off, restoring
the previous grid for reproducing earlier numbers.

# AsianOption 0.4.0

## Indifference engine updated to the revised model specification

The utility-indifference module now implements the revised specification of
Tiwari & Majumdar. Three changes affect the numbers it produces.

**The terminal liquidation charge no longer penalises the dealer impact state.**
It is now `L(q) = ell_1 * |q| + (Gamma_Q / 2) * q^2`: a new linear term, and no
term in `j` at all. The dealer's transient impact enters only through pre-expiry
execution costs. **`Gamma_J` has been removed** from
`price_arithmetic_asian_indiff()`, `price_geometric_asian_indiff()` and the
internal validator; calls that pass it will error. Its replacement, `ell_1`,
defaults to `0`, which gives the purely quadratic charge. At `Gamma_J = 0` the
new engine reproduces the old quotes to machine precision.

**Trading admissibility is now forward-looking.** A trade is admissible only if
the execution price `S + lambda_bar_P * Q + lambda_bar_T * J` clears the floor
`eps_exec` both at the current state and after one step, on both price branches.
`eps_exec` is a new argument defaulting to `1e-6 * S0`. Unlike the previous
condition, this set can be empty; the dealer then stands still, and the number
of such states is reported as `diagnostics$n_infeasible` and raised as a
warning rather than absorbed silently.

**Discrete monitoring is now supported.** `monitoring = "discrete"` with
`n_fixings = M` averages over the contractual fixing dates `t_k = kT/M` instead
of over the whole path, updating the accumulator only on those dates. `M` must
divide `N` so every fixing falls on a time-grid node. The accumulator grid is
sized from the exact variance of the discrete average, which at small `M` is
substantially wider than the continuous one. Quotes converge to the continuous
case at rate `O(1/M)`. `monitoring = "continuous"` remains the default and is
unchanged.

# AsianOption 0.3.0

## Bug fix: Kemna-Vorst benchmarks now return present values

`price_kemna_vorst_geometric()` returned the *undiscounted* expectation
`E[(G_T - K)^+]` for `sigma > 0` but a discounted value in its `sigma == 0`
branch, and every other pricing function in the package returns a time-0
present value. It now discounts in both branches. At the package's own example
parameters (`S0 = K = 100`, `r = 0.05`, `sigma = 0.2`, `T = 1`) the returned
value changes from `5.8312` to `5.5468`.

`price_kemna_vorst_arithmetic()` was affected more subtly: its control variate
added the *undiscounted* analytic geometric price to a correction built from
*discounted* simulated payoffs, so the result was neither convention. The
analytic leg is now discounted on the same basis as the payoffs, which shifts
the estimate by exactly `(1 - e^{-r tau})` times the geometric price and leaves
the Monte Carlo error untouched. At the same parameters the estimate changes
from about `6.05` to about `5.77`.

**Impact on users.** Any code comparing these benchmarks with
`price_*_asian_diffusion()` or `price_*_asian_indiff()` was previously
comparing quantities in different units, which at `r = 0.05` and `T = 1`
manifests as a spurious `4.88%` discrepancy. Code that applied its own
`exp(-r * T)` correction to `price_kemna_vorst_geometric()` should drop it.
The frictionless limit `price_geometric_asian_diffusion(lambda_T = 0)` now
agrees with `price_kemna_vorst_geometric()` to machine precision.

## New: utility-indifference pricing (endogenous regime, rewritten)

`price_arithmetic_asian_indiff()` and `price_geometric_asian_indiff()` replace
the endogenous HJB module with a self-financing exponential-utility
indifference formulation for a dealer with constant absolute risk aversion.
The dealer runs a genuine cash account with continuous compounding, cannot
move the Asian fixing, and quotes are reservation prices obtained from three
value functions:

```
P_ask = (v0 - v_plus) / exp(r T),    P_bid = (v_minus - v0) / exp(r T)
```

This addresses the structural objections to the previous module: there is now
a replication/self-financing account, the bid and ask have an explicit
economic meaning, and the zero-friction limit recovers the frictionless
benchmark (see below).

Also new in this module: put payoffs are implemented (the legacy module warned
and returned call prices), an optional payoff cap `phi_cap`, an option notional
`n_options`, `print`, `summary` and `plot` methods for the new
`"indiff_asian"` class, and a backward sweep parallelised over the price and
impact grid planes with RcppParallel (thread count via `n_threads`; the result
does not depend on it).

## Breaking change in the meaning of two arguments

In the new interface `lambda_bar_T` and `lambda_bar_P` are the **dealer's own
execution-impact coefficients** — the execution price is
`S + lambda_bar_P * Q + lambda_bar_T * J` — and not coefficients in the drift
of `S`. The drift loading is now `lambda_I`. The exogenous impact
mean-reversion rate is `kappa_I` and the decay of the dealer's transient state
is `kappa_J`; these were a single `kappa` before.

The legacy functions keep their old meanings and are unaffected.

## Deprecated

`price_arithmetic_asian_hjb()` and `price_geometric_asian_hjb()` are
deprecated. They emit a notice once per session, keep their behaviour
unchanged so the arXiv v2 numbers stay reproducible, will be made internal in
0.4.0 and removed in 0.5.0.

## Numerical notes

Three discretisation choices in the new engine are worth knowing about,
because two of them were worth several percent of the price:

* **Aligned log-price grid.** By default `n_logS = NULL` lets the engine size
  the grid so that one shock `sigma * sqrt(dt)` spans exactly one cell. An
  unaligned grid spreads every transition over two nodes and inflates the
  effective volatility; at the previously intended default of `n_logS = 41`
  with `N = 25` that cost about 12% on the price.
* **Drift-tracking grid.** The grid origin follows the deterministic drift, so
  the drift is never interpolated. With `lambda_I = 0` the price lattice is
  then exactly recombining.
* **Trapezoidal accumulator.** `accum_rule = "trapezoid"` (default)
  discretises `a_T = integral h(S_u) du` with an O(dt^2) error in the variance
  of the running average, against O(dt) for the left-endpoint rule.

With these in place the frictionless limit matches the Kemna-Vorst price to
better than 1% at `N = 25` with a moderately fine accumulator grid, and the
engine converges to the exact value of its own discrete model to within 0.03%.
`inst/scripts/indiff_convergence.R` reproduces both statements.

When `lambda_I = 0` the value function is exactly constant in the exogenous
impact state, and the engine collapses that dimension, which cuts the state
space by roughly a factor of ten.

**The spread is more grid-sensitive than the price level**, chiefly through
`n_Q`: one time step moves the inventory by only `nu_bar * dt`, and a coarse
inventory grid cannot resolve that. Check `inst/scripts/indiff_convergence.R`
before quoting a spread.

## Comparative statics differ from the legacy model

Trading in the new formulation is hedging, not manipulation, so **the spread
widens in the execution cost** `k_A`, `k_B`, reversing the direction reported
for the legacy module. This is stable across every grid tested. The spread
also widens in `gamma`, with the ask rising and the bid falling, as CARA
reservation prices should.

The signs with respect to `lambda_bar_T`, `lambda_bar_P` and `kappa_J` are
**not** robust at coarse grids and should not be quoted from a single run;
`inst/scripts/indiff_comparative_statics.R` reports them with an explicit
refinement study. Note that a linear inventory-proportional concession
`lambda_bar_P` contributes `lambda_bar_P (Q_T^2 - Q_0^2)/2` to total execution
cash and is therefore costless on any round trip, so it is not a friction in
the sense that `k_A` and `k_B` are.

# AsianOption 0.2.0

CRAN release.
