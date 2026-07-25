# Economic consistency tests (design.md Section 7, points 3-9).  These encode
# the properties the referee report identified as missing from the legacy
# endogenous module, so they are the substantive content of the rewrite.
#
# Only monotonicities that were verified to be stable under grid refinement are
# asserted here.  The signs of the spread with respect to lambda_bar_T,
# lambda_bar_P and kappa_J are NOT stable at grid sizes that fit in a test
# suite; see inst/scripts/indiff_comparative_statics.R, which reports them with
# an explicit refinement study instead of asserting a sign.

# Frictionless, near-risk-neutral configuration used by the limit tests.
frictionless <- function(fun, ..., k = 1e-4, gamma = 1e-3, N = 25L,
                         n_R = 121L) {
  fun(S0 = 100, K = 100, T = 1, N = N, sigma = 0.2, r_cont = 0.05,
      lambda_I = 0, kappa_I = 1, eta = 0, rho = 0,
      lambda_bar_T = 0, lambda_bar_P = 0, kappa_J = 1,
      k_A = k, k_B = k, psi_cost = 1,
      gamma = gamma, Gamma_Q = 1e-4, Gamma_J = 1e-4,
      Q_bar = 10, nu_bar = 10,
      n_I = 5L, n_Q = 7L, n_J = 7L, n_R = n_R, n_controls = 7L,
      ...)
}

# price_kemna_vorst_geometric returns an undiscounted expectation, so the
# quantity comparable with an indifference price is exp(-r T) times its value.
kv_geometric_discounted <- function(S0, K, r, sigma, T) {
  exp(-r * T) * price_kemna_vorst_geometric(S0, K, r, sigma, 0, T)
}

test_that("zero-friction geometric limit recovers Kemna-Vorst", {
  skip_on_cran()
  res <- frictionless(price_geometric_asian_indiff)
  target <- kv_geometric_discounted(100, 100, 0.05, 0.2, 1)

  expect_equal(res$ask_price, target, tolerance = 0.05)
  expect_equal(res$bid_price, target, tolerance = 0.05)
  # With no frictions and negligible risk aversion the quotes must coincide.
  expect_lt(res$spread, 0.02 * target)
})

test_that("zero-friction geometric limit tightens as the grid refines", {
  skip_on_cran()
  target <- kv_geometric_discounted(100, 100, 0.05, 0.2, 1)
  coarse <- frictionless(price_geometric_asian_indiff, n_R = 41L)
  fine   <- frictionless(price_geometric_asian_indiff, n_R = 241L, N = 50L)

  expect_lt(abs(fine$ask_price / target - 1),
            abs(coarse$ask_price / target - 1))
  expect_lt(abs(fine$ask_price / target - 1), 0.02)
})

test_that("zero-friction arithmetic limit recovers Kemna-Vorst", {
  skip_on_cran()
  res <- frictionless(price_arithmetic_asian_indiff)
  target <- price_kemna_vorst_arithmetic(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T0 = 0, T_mat = 1,
    n = 250, M = 200000, seed = 20260725
  )
  expect_equal(res$ask_price, target, tolerance = 0.06)
  expect_equal(res$bid_price, target, tolerance = 0.06)
  expect_lt(res$spread, 0.02 * target)
})

test_that("the spread vanishes as execution costs are switched off", {
  skip_on_cran()
  res <- lapply(c(0.5, 0.05, 0.005, 0), function(k) {
    frictionless(price_geometric_asian_indiff, k = k, gamma = 1e-4,
                 n_R = 61L)
  })
  spreads <- vapply(res, function(z) z$spread, numeric(1))
  asks    <- vapply(res, function(z) z$ask_price, numeric(1))
  # Once frictions and risk aversion are both negligible there is nothing left
  # to charge for, so every spread here is a rounding-level fraction of the
  # price.  The residual differences between them are interpolation noise, not
  # economics, so monotonicity is only asserted up to that noise floor.
  expect_true(all(spreads < 0.005 * asks))
  expect_true(all(diff(spreads) <= 1e-3 * asks[-1]))
  expect_lt(spreads[4], 0.005 * asks[4])
})

test_that("as execution costs explode the quotes approach the no-trade limit", {
  skip_on_cran()
  args <- list(S0 = 100, K = 100, T = 1, N = 10L, sigma = 0.2, r_cont = 0.05,
               gamma = 0.05, Q_bar = 2, nu_bar = 4,
               n_I = 5L, n_Q = 9L, n_J = 9L, n_R = 41L, n_controls = 9L)
  # A dealer who may not trade at all: the pure CARA reservation spread.
  no_trade <- do.call(price_geometric_asian_indiff,
                      c(args, list(control_set = 0)))
  expensive <- do.call(price_geometric_asian_indiff,
                       c(args, list(k_A = 1e4, k_B = 1e4)))
  expect_equal(expensive$spread, no_trade$spread, tolerance = 1e-6)
  expect_equal(expensive$ask_price, no_trade$ask_price, tolerance = 1e-6)
})

test_that("core invariants hold on every run", {
  res <- price_geometric_asian_indiff(
    S0 = 100, K = 100, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
    n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 21L, n_controls = 7L
  )
  # v- >= v0 >= v+ : being long the option is worth more than being flat,
  # which is worth more than being short.
  expect_gte(res$v_minus, res$v0)
  expect_gte(res$v0, res$v_plus)
  # Both quotes non-negative, and bid <= ask (design.md flag F2).
  expect_gte(res$bid_price, -1e-8)
  expect_gte(res$ask_price, -1e-8)
  expect_lte(res$bid_price, res$ask_price + 1e-8)
  expect_equal(res$spread, res$ask_price - res$bid_price)
  expect_equal(res$mid_price, 0.5 * (res$ask_price + res$bid_price))
})

test_that("grid interiority improves when the accumulator grid is widened", {
  narrow <- price_geometric_asian_indiff(
    S0 = 100, K = 100, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
    n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 11L, n_controls = 7L
  )
  wide <- price_geometric_asian_indiff(
    S0 = 100, K = 100, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
    n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 61L, n_controls = 7L,
    accum_sd = 8
  )
  expect_true(narrow$diagnostics$initial_state_interior)
  expect_true(wide$diagnostics$initial_state_interior)
  expect_lte(wide$diagnostics$clamp_fraction$baseline[["R"]],
             narrow$diagnostics$clamp_fraction$baseline[["R"]])
})

test_that("the spread widens in the temporary execution cost", {
  skip_on_cran()
  # Trading here is hedging, not manipulation, so costlier execution must
  # widen the quotes.  This is the one impact-parameter monotonicity that is
  # stable across every grid and time step tried, and it reverses the sign
  # reported for the legacy model.
  run <- function(k) {
    price_geometric_asian_indiff(
      S0 = 100, K = 100, T = 1, N = 10L, sigma = 0.2, r_cont = 0.05,
      gamma = 0.05, k_A = k, k_B = k, Q_bar = 2, nu_bar = 4,
      n_I = 5L, n_Q = 9L, n_J = 9L, n_R = 41L, n_controls = 9L)$spread
  }
  sp <- vapply(c(0.005, 0.05, 0.2, 1.0), run, numeric(1))
  expect_true(all(diff(sp) > 0))
  expect_gt(sp[4] / sp[1], 2)          # and the effect is economically large
})

test_that("the ask rises and the bid falls with risk aversion", {
  skip_on_cran()
  run <- function(g) {
    price_geometric_asian_indiff(
      S0 = 100, K = 100, T = 1, N = 10L, sigma = 0.2, r_cont = 0.05,
      gamma = g, Q_bar = 2, nu_bar = 4,
      n_I = 5L, n_Q = 9L, n_J = 9L, n_R = 41L, n_controls = 9L)
  }
  res <- lapply(c(0.005, 0.02, 0.08), run)
  asks <- vapply(res, function(z) z$ask_price, numeric(1))
  bids <- vapply(res, function(z) z$bid_price, numeric(1))
  expect_true(all(diff(asks) > 0))
  expect_true(all(diff(bids) < 0))
  expect_true(all(diff(asks - bids) > 0))
})

test_that("call prices rise in S0 and fall in K", {
  skip_on_cran()
  run <- function(S0, K) {
    price_geometric_asian_indiff(
      S0 = S0, K = K, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
      n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 41L, n_controls = 7L)$mid_price
  }
  s <- vapply(c(90, 100, 110), function(v) run(v, 100), numeric(1))
  expect_true(all(diff(s) > 0))
  k <- vapply(c(90, 100, 110), function(v) run(100, v), numeric(1))
  expect_true(all(diff(k) < 0))
})

test_that("capping the payoff lowers the call quotes and puts run uncapped", {
  skip_on_cran()
  args <- list(S0 = 100, K = 100, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
               n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 41L, n_controls = 7L)
  uncapped <- do.call(price_geometric_asian_indiff, args)
  capped   <- do.call(price_geometric_asian_indiff, c(args, list(phi_cap = 3)))
  expect_lt(capped$ask_price, uncapped$ask_price)
  expect_lte(capped$bid_price, uncapped$bid_price + 1e-10)

  # Puts are bounded by K, so they are finite with no cap at all.
  put <- do.call(price_geometric_asian_indiff,
                 c(args, list(option_type = "put")))
  expect_true(is.finite(put$ask_price) && is.finite(put$bid_price))
  expect_gte(put$bid_price, -1e-8)
  expect_lte(put$bid_price, put$ask_price + 1e-8)
})

test_that("put quotes rise with the strike", {
  skip_on_cran()
  run <- function(K) {
    price_geometric_asian_indiff(
      S0 = 100, K = K, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
      option_type = "put",
      n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 41L, n_controls = 7L)$mid_price
  }
  p <- vapply(c(90, 100, 110), run, numeric(1))
  expect_true(all(diff(p) > 0))
})

test_that("quotes are per unit of notional and rise with position size", {
  skip_on_cran()
  args <- list(S0 = 100, K = 100, T = 1, N = 8L, sigma = 0.2, r_cont = 0.05,
               gamma = 1e-4, lambda_bar_T = 0, lambda_bar_P = 0,
               k_A = 1e-4, k_B = 1e-4, Gamma_Q = 1e-4, Gamma_J = 1e-4,
               Q_bar = 10, nu_bar = 10,
               n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 41L, n_controls = 7L)
  one <- do.call(price_geometric_asian_indiff, args)
  ten <- do.call(price_geometric_asian_indiff, c(args, list(n_options = 10)))
  # Per-unit quotes barely move in the risk-neutral, frictionless limit, and a
  # CARA dealer never sells the larger position more cheaply per unit.
  expect_equal(ten$ask_price, one$ask_price, tolerance = 0.02)
  expect_gte(ten$ask_price, one$ask_price - 1e-6)
  expect_lte(ten$bid_price, one$bid_price + 1e-6)
})

test_that("the trapezoidal accumulator beats the left-endpoint rule", {
  skip_on_cran()
  args <- list(S0 = 100, K = 100, T = 1, N = 10L, sigma = 0.2, r_cont = 0.05,
               gamma = 1e-3, lambda_bar_T = 0, lambda_bar_P = 0, eta = 0,
               k_A = 1e-4, k_B = 1e-4, Gamma_Q = 1e-4, Gamma_J = 1e-4,
               Q_bar = 10, nu_bar = 10,
               n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 121L, n_controls = 7L)
  target <- kv_geometric_discounted(100, 100, 0.05, 0.2, 1)
  trap <- do.call(price_geometric_asian_indiff, args)
  left <- do.call(price_geometric_asian_indiff,
                  c(args, list(accum_rule = "left")))
  # The left rule understates the variance of the running average by O(dt),
  # so at N = 10 it must be visibly further from the frictionless benchmark.
  expect_lt(abs(trap$ask_price / target - 1),
            abs(left$ask_price / target - 1))
})

test_that("an unaligned log-price grid is reported and inflates the price", {
  skip_on_cran()
  args <- list(S0 = 100, K = 100, T = 1, N = 25L, sigma = 0.2, r_cont = 0.05,
               gamma = 1e-3, lambda_bar_T = 0, lambda_bar_P = 0, eta = 0,
               k_A = 1e-4, k_B = 1e-4, Gamma_Q = 1e-4, Gamma_J = 1e-4,
               Q_bar = 10, nu_bar = 10,
               n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 121L, n_controls = 7L)
  target <- kv_geometric_discounted(100, 100, 0.05, 0.2, 1)
  auto <- do.call(price_geometric_asian_indiff, args)
  # 41 nodes cannot be aligned with one shock over the required margin.  An
  # unaligned grid can distort the three value functions enough to trip the
  # bid <= ask invariant, which is the warning being suppressed here and is
  # itself part of the point of this test.
  bad <- suppressWarnings(
    do.call(price_geometric_asian_indiff, c(args, list(n_logS = 41L))))

  expect_true(auto$diagnostics$grid_aligned)
  expect_false(bad$diagnostics$grid_aligned)
  expect_lte(auto$bid_price, auto$ask_price + 1e-8)
  # Interpolating the shock spreads mass over two nodes and adds variance.
  expect_gt(bad$ask_price, auto$ask_price)
  expect_lt(abs(auto$ask_price / target - 1), abs(bad$ask_price / target - 1))
})
