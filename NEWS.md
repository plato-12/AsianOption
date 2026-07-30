# AsianOption 0.3.0

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
