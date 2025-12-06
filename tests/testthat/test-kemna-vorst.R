# Test suite for Kemna-Vorst implementation

test_that("Kemna-Vorst geometric average gives positive prices", {
  price <- price_kemna_vorst_geometric(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2,
    T0 = 0, T = 1, option_type = "call"
  )

  expect_true(is.numeric(price))
  expect_true(price > 0)
  expect_true(is.finite(price))
})

test_that("Kemna-Vorst geometric: call price increases with volatility", {
  price_low_vol <- price_kemna_vorst_geometric(
    100, 100, 0.05, 0.1, 0, 1, "call"
  )
  price_high_vol <- price_kemna_vorst_geometric(
    100, 100, 0.05, 0.3, 0, 1, "call"
  )

  expect_true(price_high_vol > price_low_vol)
})

test_that("Kemna-Vorst geometric: call price decreases with strike", {
  price_low_K <- price_kemna_vorst_geometric(
    100, 90, 0.05, 0.2, 0, 1, "call"
  )
  price_high_K <- price_kemna_vorst_geometric(
    100, 110, 0.05, 0.2, 0, 1, "call"
  )

  expect_true(price_low_K > price_high_K)
})

test_that("Kemna-Vorst geometric: put price increases with strike", {
  price_low_K <- price_kemna_vorst_geometric(
    100, 90, 0.05, 0.2, 0, 1, "put"
  )
  price_high_K <- price_kemna_vorst_geometric(
    100, 110, 0.05, 0.2, 0, 1, "put"
  )

  expect_true(price_high_K > price_low_K)
})

test_that("Kemna-Vorst geometric: zero volatility gives intrinsic value", {

  price_call <- price_kemna_vorst_geometric(
    100, 100, 0.05, 0, 0, 1, "call"
  )
  price_put <- price_kemna_vorst_geometric(
    100, 100, 0.05, 0, 0, 1, "put"
  )

  G_T <- 100 * exp(0.05 * 1 / 2)
  expected_call <- max(0, G_T - 100) * exp(-0.05)
  expected_put <- max(0, 100 - G_T) * exp(-0.05)

  expect_equal(price_call, expected_call, tolerance = 1e-6)
  expect_equal(price_put, expected_put, tolerance = 1e-6)
})

test_that("Kemna-Vorst geometric: deep ITM call approaches forward price", {

  price <- price_kemna_vorst_geometric(
    100, 50, 0.05, 0.2, 0, 1, "call"
  )

  expect_true(price > 40)
})

test_that("Kemna-Vorst geometric: input validation", {
  expect_error(
    price_kemna_vorst_geometric(-100, 100, 0.05, 0.2, 0, 1, "call"),
    "S0 must be a positive number"
  )
  expect_error(
    price_kemna_vorst_geometric(100, -100, 0.05, 0.2, 0, 1, "call"),
    "K must be a positive number"
  )
  expect_error(
    price_kemna_vorst_geometric(100, 100, 0.05, -0.2, 0, 1, "call"),
    "sigma must be a non-negative number"
  )
  expect_error(
    price_kemna_vorst_geometric(100, 100, 0.05, 0.2, 1, 0, "call"),
    "T must be greater than T0"
  )
  expect_error(
    price_kemna_vorst_geometric(100, 100, 0.05, 0.2, 0, 1, "invalid"),
    "'arg' should be one of"
  )
})

test_that("Kemna-Vorst geometric binomial version works", {
  price <- price_kemna_vorst_geometric_binomial(
    S0 = 100, K = 100, r = 1.05,
    u = 1.2, d = 0.8, n = 10
  )

  expect_true(is.numeric(price))
  expect_true(price > 0)
  expect_true(is.finite(price))
})

test_that("Kemna-Vorst geometric binomial: validation", {
  expect_error(
    price_kemna_vorst_geometric_binomial(100, 100, 1.05, 0.9, 0.8, 10),
    "u must be greater than 1"
  )
  expect_error(
    price_kemna_vorst_geometric_binomial(100, 100, 1.05, 1.2, 1.1, 10),
    "d must be less than 1"
  )
})

test_that("Kemna-Vorst arithmetic Monte Carlo works", {
  result <- price_kemna_vorst_arithmetic(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2,
    T0 = 0, T = 1, n = 10, M = 1000,
    seed = 123
  )

  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(is.finite(result))
})

test_that("Kemna-Vorst arithmetic with diagnostics returns list", {
  result <- price_kemna_vorst_arithmetic(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2,
    T0 = 0, T = 1, n = 10, M = 1000,
    seed = 123, return_diagnostics = TRUE
  )

  expect_type(result, "list")
  expect_true("price" %in% names(result))
  expect_true("std_error" %in% names(result))
  expect_true("lower_ci" %in% names(result))
  expect_true("upper_ci" %in% names(result))
  expect_true("geometric_price" %in% names(result))
  expect_true("correlation" %in% names(result))
  expect_true("variance_reduction_factor" %in% names(result))
})

test_that("Kemna-Vorst arithmetic: control variate reduces variance", {
  set.seed(123)
  result_with <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 10, 5000,
    use_control_variate = TRUE, return_diagnostics = TRUE, seed = 123
  )

  set.seed(123)
  result_without <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 10, 5000,
    use_control_variate = FALSE, return_diagnostics = TRUE, seed = 123
  )

  expect_true(result_with$std_error < result_without$std_error)

  expect_true(result_with$variance_reduction_factor < 1)
  expect_true(result_with$variance_reduction_factor > 0)
})

test_that("Kemna-Vorst arithmetic: correlation is high", {
  result <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 50, 10000,
    return_diagnostics = TRUE, seed = 123
  )

  expect_true(result$correlation > 0.9)
})

test_that("Kemna-Vorst arithmetic: bounded by geometric", {

  geom_price <- price_kemna_vorst_geometric(
    100, 100, 0.05, 0.2, 0, 1, "call"
  )

  arith_price <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 50, 10000, seed = 123
  )

  expect_true(arith_price >= geom_price - 0.5)
})

test_that("Kemna-Vorst arithmetic: prices increase with volatility", {
  price_low <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.1, 0, 1, 20, 5000, seed = 123
  )

  price_high <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.3, 0, 1, 20, 5000, seed = 123
  )

  expect_true(price_high > price_low)
})

test_that("Kemna-Vorst arithmetic: call decreases with strike", {
  price_low_K <- price_kemna_vorst_arithmetic(
    100, 90, 0.05, 0.2, 0, 1, 20, 5000, seed = 123
  )

  price_high_K <- price_kemna_vorst_arithmetic(
    100, 110, 0.05, 0.2, 0, 1, 20, 5000, seed = 123
  )

  expect_true(price_low_K > price_high_K)
})

test_that("Kemna-Vorst arithmetic: put increases with strike", {
  price_low_K <- price_kemna_vorst_arithmetic(
    100, 90, 0.05, 0.2, 0, 1, 20, 5000,
    option_type = "put", seed = 123
  )

  price_high_K <- price_kemna_vorst_arithmetic(
    100, 110, 0.05, 0.2, 0, 1, 20, 5000,
    option_type = "put", seed = 123
  )

  expect_true(price_high_K > price_low_K)
})

test_that("Kemna-Vorst arithmetic: input validation", {
  expect_error(
    price_kemna_vorst_arithmetic(-100, 100, 0.05, 0.2, 0, 1, 10, 1000),
    "S0 must be a positive number"
  )
  expect_error(
    price_kemna_vorst_arithmetic(100, -100, 0.05, 0.2, 0, 1, 10, 1000),
    "K must be a positive number"
  )
  expect_error(
    price_kemna_vorst_arithmetic(100, 100, 0.05, -0.2, 0, 1, 10, 1000),
    "sigma must be a non-negative number"
  )
  expect_error(
    price_kemna_vorst_arithmetic(100, 100, 0.05, 0.2, 1, 0, 10, 1000),
    "T must be greater than T0"
  )
  expect_error(
    price_kemna_vorst_arithmetic(100, 100, 0.05, 0.2, 0, 1, -5, 1000),
    "n must be a positive integer"
  )
  expect_error(
    price_kemna_vorst_arithmetic(100, 100, 0.05, 0.2, 0, 1, 10, -100),
    "M must be a positive integer"
  )
})

test_that("Kemna-Vorst arithmetic: warns for small M", {
  expect_warning(
    price_kemna_vorst_arithmetic(100, 100, 0.05, 0.2, 0, 1, 10, 500),
    "M = 500 is very small"
  )
})

test_that("Kemna-Vorst arithmetic binomial version works", {
  result <- price_kemna_vorst_arithmetic_binomial(
    S0 = 100, K = 100, r = 1.05,
    u = 1.2, d = 0.8, n = 10, M = 1000,
    seed = 123
  )

  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(is.finite(result))
})

test_that("Kemna-Vorst arithmetic binomial: validation", {
  expect_error(
    price_kemna_vorst_arithmetic_binomial(100, 100, 1.05, 0.9, 0.8, 10, 1000),
    "u must be greater than 1"
  )
  expect_error(
    price_kemna_vorst_arithmetic_binomial(100, 100, 1.05, 1.2, 1.1, 10, 1000),
    "d must be less than 1"
  )
})

test_that("Kemna-Vorst print and summary methods work", {
  result <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 10, 1000,
    return_diagnostics = TRUE, seed = 123
  )

  expect_output(print(result), "Kemna-Vorst Arithmetic Asian Option")
  expect_output(print(result), "Estimated Price")
  expect_output(print(result), "Standard Error")
  expect_output(print(result), "95% CI")

  expect_output(summary(result), "Kemna-Vorst Arithmetic Asian Option")
})

test_that("Kemna-Vorst: reproducibility with seed", {
  result1 <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 10, 1000, seed = 456
  )

  result2 <- price_kemna_vorst_arithmetic(
    100, 100, 0.05, 0.2, 0, 1, 10, 1000, seed = 456
  )

  expect_equal(result1, result2)
})

test_that("Kemna-Vorst: comparison with example from paper", {

  S0 <- 40
  K <- 40
  r_continuous <- log(1.05)
  sigma <- 0.2
  T0 <- 0
  T <- 88 / 365

  geom_price <- price_kemna_vorst_geometric(
    S0, K, r_continuous, sigma, T0, T, "call"
  )

  expect_true(geom_price > 1.0)
  expect_true(geom_price < 2.0)
})
