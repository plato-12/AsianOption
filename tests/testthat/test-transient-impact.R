test_that("Transient impact functions validate inputs correctly", {
  # Valid inputs
  expect_silent(
    validate_transient_inputs(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = 0.05, lambda_T = 0.05,
      alpha = 0.5, psi = 1,
      volumes = rep(1, 10)
    )
  )

  # Invalid S0
  expect_error(
    validate_transient_inputs(
      S0 = -100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = 0.05, lambda_T = 0.05,
      alpha = 0.5, psi = 1,
      volumes = rep(1, 10)
    ),
    "S0 must be positive"
  )

  # Invalid alpha (>= 1)
  expect_error(
    validate_transient_inputs(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = 0.05, lambda_T = 0.05,
      alpha = 1.0, psi = 1,
      volumes = rep(1, 10)
    ),
    "alpha .* must be in \\[0, 1\\)"
  )

  # Invalid psi (<= 0)
  expect_error(
    validate_transient_inputs(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = 0.05, lambda_T = 0.05,
      alpha = 0.5, psi = 0,
      volumes = rep(1, 10)
    ),
    "psi .* must be positive"
  )

  # Negative volumes
  expect_error(
    validate_transient_inputs(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = 0.05, lambda_T = 0.05,
      alpha = 0.5, psi = 1,
      volumes = c(1, -1, 1)
    ),
    "All volumes must be non-negative"
  )

  # Invalid lambda_P
  expect_error(
    validate_transient_inputs(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda_P = -0.1, lambda_T = 0.05,
      alpha = 0.5, psi = 1,
      volumes = rep(1, 10)
    ),
    "lambda_P .* must be non-negative"
  )
})


test_that("Geometric Asian pricing with transient impact works", {
  # Test with constant volumes
  volumes <- rep(1, 5)

  price <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes,
    option_type = "call"
  )

  expect_type(price, "double")
  expect_true(is.finite(price))
  expect_true(price >= 0)

  # Put option should also work
  put_price <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes,
    option_type = "put"
  )

  expect_type(put_price, "double")
  expect_true(is.finite(put_price))
  expect_true(put_price >= 0)
})


test_that("Transient impact reduces to permanent impact when lambda_T = 0", {
  # When lambda_T = 0, there is no transient impact component
  # This should match permanent impact with lambda = lambda_P

  volumes <- rep(1, 8)

  # Transient model with lambda_T = 0 (no transient component)
  price_transient <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.1, lambda_T = 0,
    alpha = 0.5, psi = 1,  # alpha doesn't matter when lambda_T = 0
    volumes = volumes,
    option_type = "call"
  )

  # Permanent model with lambda = lambda_P
  price_permanent <- price_geometric_asian(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1,  # Same as lambda_P above
    v_u = 1, v_d = 1,
    n = 8,
    option_type = "call"
  )

  # Should match exactly (or very close)
  expect_equal(price_transient, price_permanent, tolerance = 1e-6)
})


test_that("Transient impact price increases with lambda_T", {
  volumes <- rep(1, 6)

  # Lower transient impact
  price_low <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.01,
    alpha = 0.5, psi = 1,
    volumes = volumes
  )

  # Higher transient impact
  price_high <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.1,
    alpha = 0.5, psi = 1,
    volumes = volumes
  )

  # Higher impact should generally increase call option prices
  expect_true(price_high >= price_low)
})


test_that("Transient impact with varying volumes works", {
  # Time-varying volumes
  volumes <- seq(0.5, 1.5, length.out = 6)

  price <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes
  )

  expect_type(price, "double")
  expect_true(is.finite(price))
  expect_true(price >= 0)
})


test_that("Arithmetic bounds with transient impact work", {
  volumes <- rep(1, 6)

  bounds <- arithmetic_asian_bounds_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes
  )

  expect_s3_class(bounds, "arithmetic_bounds_transient")
  expect_true(bounds$lower_bound >= 0)
  expect_true(bounds$upper_bound_global >= bounds$lower_bound)
  expect_true(bounds$rho_star >= 1)
  expect_true(is.na(bounds$upper_bound_path_specific))
})


test_that("Path-specific arithmetic bounds work", {
  volumes <- rep(1, 5)

  bounds <- arithmetic_asian_bounds_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes,
    compute_path_specific = TRUE
  )

  expect_false(is.na(bounds$upper_bound_path_specific))
  expect_true(bounds$upper_bound_path_specific >= bounds$lower_bound)
  expect_true(bounds$upper_bound_path_specific <= bounds$upper_bound_global)
  expect_equal(bounds$n_paths_used, 2^5)
})


test_that("Different alpha values affect prices correctly", {
  volumes <- rep(1, 6)

  # Low decay (short memory)
  price_low_alpha <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.1, psi = 1,
    volumes = volumes
  )

  # High decay (long memory)
  price_high_alpha <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.9, psi = 1,
    volumes = volumes
  )

  # Both should be valid prices
  expect_true(is.finite(price_low_alpha))
  expect_true(is.finite(price_high_alpha))

  # Higher alpha (longer memory) should generally give different prices
  expect_true(abs(price_high_alpha - price_low_alpha) > 0.001)
})


test_that("Different psi values affect prices correctly", {
  # Use volumes != 1 so that v^psi differs for different psi
  volumes <- rep(2, 5)

  # Square-root impact
  price_sqrt <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 0.5,
    volumes = volumes
  )

  # Linear impact
  price_linear <- price_geometric_asian_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1.0,
    volumes = volumes
  )

  expect_true(is.finite(price_sqrt))
  expect_true(is.finite(price_linear))
  # For volumes > 1, linear impact (psi=1) should give larger effect than sqrt (psi=0.5)
  expect_true(abs(price_sqrt - price_linear) > 0.01)
})


test_that("Print methods work for transient impact", {
  volumes <- rep(1, 5)

  # Arithmetic bounds
  bounds <- arithmetic_asian_bounds_transient(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda_P = 0.05, lambda_T = 0.05,
    alpha = 0.5, psi = 1,
    volumes = volumes
  )

  expect_output(print(bounds), "Arithmetic Asian Option Bounds")
  expect_output(print(bounds), "Transient Impact")
})


test_that("No-arbitrage violations are caught", {
  volumes <- rep(10, 5)  # Very large volumes

  expect_error(
    price_geometric_asian_transient(
      S0 = 100, K = 100, r = 1.05, u = 1.1, d = 0.95,
      lambda_P = 1.0, lambda_T = 1.0,  # Large impact coefficients
      alpha = 0.9, psi = 1,
      volumes = volumes
    ),
    "risk-neutral probability"
  )
})
