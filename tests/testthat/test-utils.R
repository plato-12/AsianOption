test_that("compute_p_adj returns valid probability", {
  p_adj <- compute_p_adj(1.05, 1.2, 0.8, 0.1, 1, 1)

  expect_true(is.numeric(p_adj))
  expect_length(p_adj, 1)
  expect_true(p_adj >= 0 && p_adj <= 1)
  expect_false(is.na(p_adj))
})

test_that("compute_p_adj handles zero price impact", {

  p_adj <- compute_p_adj(1.05, 1.2, 0.8, 0, 0, 0)

  p_standard <- (1.05 - 0.8)/(1.2 - 0.8)

  expect_equal(p_adj, p_standard, tolerance = 1e-10)
})

test_that("compute_p_adj changes with price impact", {
  p_adj_no_impact <- compute_p_adj(1.05, 1.2, 0.8, 0, 0, 0)
  p_adj_with_impact <- compute_p_adj(1.05, 1.2, 0.8, 0.1, 1, 1)

  expect_false(p_adj_no_impact == p_adj_with_impact)
})

test_that("compute_adjusted_factors returns correct structure", {
  factors <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)

  expect_type(factors, "list")
  expect_named(factors, c("u_tilde", "d_tilde"))
  expect_length(factors$u_tilde, 1)
  expect_length(factors$d_tilde, 1)
  expect_true(is.numeric(factors$u_tilde))
  expect_true(is.numeric(factors$d_tilde))
})

test_that("Adjusted factors satisfy ordering", {
  factors <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)

  expect_true(factors$u_tilde > factors$d_tilde)
})

test_that("Adjusted up factor increases with price impact", {
  factors_no_impact <- compute_adjusted_factors(1.2, 0.8, 0, 0, 0)
  factors_with_impact <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)

  expect_true(factors_with_impact$u_tilde > factors_no_impact$u_tilde)
  expect_equal(factors_no_impact$u_tilde, 1.2, tolerance = 1e-10)
})

test_that("Adjusted down factor decreases with price impact", {
  factors_no_impact <- compute_adjusted_factors(1.2, 0.8, 0, 0, 0)
  factors_with_impact <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)

  expect_true(factors_with_impact$d_tilde < factors_no_impact$d_tilde)
  expect_equal(factors_no_impact$d_tilde, 0.8, tolerance = 1e-10)
})

test_that("Adjusted factors match formula", {
  u <- 1.2
  d <- 0.8
  lambda <- 0.1
  v_u <- 1
  v_d <- 1

  factors <- compute_adjusted_factors(u, d, lambda, v_u, v_d)

  u_tilde_expected <- u * exp(lambda * v_u)
  d_tilde_expected <- d * exp(-lambda * v_d)

  expect_equal(factors$u_tilde, u_tilde_expected, tolerance = 1e-10)
  expect_equal(factors$d_tilde, d_tilde_expected, tolerance = 1e-10)
})

test_that("check_no_arbitrage correctly identifies valid cases", {

  result <- check_no_arbitrage(1.05, 1.2, 0.8, 0.1, 1, 1)

  expect_true(is.logical(result))
  expect_length(result, 1)
  expect_true(result)
})

test_that("check_no_arbitrage detects r too high", {

  result <- check_no_arbitrage(2.0, 1.2, 0.8, 0.1, 1, 1)

  expect_false(result)
})

test_that("check_no_arbitrage detects r too low", {

  result <- check_no_arbitrage(0.5, 1.2, 0.8, 0.1, 1, 1)

  expect_false(result)
})

test_that("check_no_arbitrage works with zero price impact", {

  result <- check_no_arbitrage(1.05, 1.2, 0.8, 0, 0, 0)

  expect_true(result)
})

test_that("check_no_arbitrage handles edge cases", {

  u_tilde <- 1.2 * exp(0.1 * 1)

  result <- check_no_arbitrage(u_tilde, 1.2, 0.8, 0.1, 1, 1)

  expect_false(result)
})

test_that("Utility functions work together consistently", {
  r <- 1.05
  u <- 1.2
  d <- 0.8
  lambda <- 0.1
  v_u <- 1
  v_d <- 1

  factors <- compute_adjusted_factors(u, d, lambda, v_u, v_d)

  p_adj <- compute_p_adj(r, u, d, lambda, v_u, v_d)

  p_adj_manual <- (r - factors$d_tilde) / (factors$u_tilde - factors$d_tilde)

  expect_equal(p_adj, p_adj_manual, tolerance = 1e-10)

  no_arb <- check_no_arbitrage(r, u, d, lambda, v_u, v_d)

  expect_true(no_arb)

  expect_true(p_adj >= 0)
  expect_true(p_adj <= 1)
})

test_that("Increasing lambda affects factors appropriately", {
  lambdas <- c(0, 0.1, 0.2, 0.3)
  u_tildes <- numeric(length(lambdas))
  d_tildes <- numeric(length(lambdas))

  for (i in seq_along(lambdas)) {
    factors <- compute_adjusted_factors(1.2, 0.8, lambdas[i], 1, 1)
    u_tildes[i] <- factors$u_tilde
    d_tildes[i] <- factors$d_tilde
  }

  expect_true(all(diff(u_tildes) > 0))

  expect_true(all(diff(d_tildes) < 0))
})

test_that("Asymmetric volumes handled correctly", {

  factors <- compute_adjusted_factors(1.2, 0.8, 0.1, 0.5, 1.5)

  expect_true(is.numeric(factors$u_tilde))
  expect_true(is.numeric(factors$d_tilde))
  expect_true(factors$u_tilde > factors$d_tilde)
})

test_that("Very small price impact behaves correctly", {

  factors <- compute_adjusted_factors(1.2, 0.8, 0.001, 1, 1)

  expect_equal(factors$u_tilde, 1.2, tolerance = 0.01)
  expect_equal(factors$d_tilde, 0.8, tolerance = 0.01)
})

test_that("compute_p_adj is consistent with manual calculation", {
  r <- 1.05
  u <- 1.2
  d <- 0.8
  lambda <- 0.15
  v_u <- 0.8
  v_d <- 1.2

  p_adj_func <- compute_p_adj(r, u, d, lambda, v_u, v_d)

  u_tilde <- u * exp(lambda * v_u)
  d_tilde <- d * exp(-lambda * v_d)
  p_adj_manual <- (r - d_tilde) / (u_tilde - d_tilde)

  expect_equal(p_adj_func, p_adj_manual, tolerance = 1e-12)
})

test_that("Results are reproducible", {

  p1 <- compute_p_adj(1.05, 1.2, 0.8, 0.1, 1, 1)
  p2 <- compute_p_adj(1.05, 1.2, 0.8, 0.1, 1, 1)
  expect_equal(p1, p2)

  f1 <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)
  f2 <- compute_adjusted_factors(1.2, 0.8, 0.1, 1, 1)
  expect_equal(f1, f2)

  c1 <- check_no_arbitrage(1.05, 1.2, 0.8, 0.1, 1, 1)
  c2 <- check_no_arbitrage(1.05, 1.2, 0.8, 0.1, 1, 1)
  expect_equal(c1, c2)
})
