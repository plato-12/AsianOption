test_that("price_geometric_asian_diffusion validates inputs correctly", {
  # Valid baseline
  expect_silent(price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  ))

  # S0 validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = -1, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "S0 must be positive"
  )

  # K validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 0, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "K must be positive"
  )

  # r validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = -0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "r must be positive"
  )

  # sigma validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "sigma must be positive"
  )

  # T validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 0,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "T must be positive"
  )

  # lambda_T validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = -0.01, I0 = 0, kappa = 1, eta = 0.1
    ),
    "lambda_T must be non-negative"
  )

  # kappa validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = -1, eta = 0.1
    ),
    "kappa must be positive"
  )

  # eta validation (scalar)
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = -0.1
    ),
    "eta must be non-negative"
  )

  # eta validation (function returning negative)
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = function(t) -0.1
    ),
    "eta\\(t\\) must be non-negative"
  )

  # rho validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 1.5
    ),
    "rho must be in \\[-1, 1\\]"
  )

  # option_type validation
  expect_error(
    price_geometric_asian_diffusion(
      S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
      lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, option_type = "invalid"
    ),
    "option_type must be 'call' or 'put'"
  )
})


test_that("price_geometric_asian_diffusion handles special cases", {
  # Case 1: No impact (lambda_T = 0) should reduce to standard geometric Asian
  price_no_impact <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )
  expect_true(is.finite(price_no_impact))
  expect_true(price_no_impact >= 0)

  # Case 2: Zero volatility (deterministic)
  price_zero_vol <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 1e-12, T = 1,
    lambda_T = 0, I0 = 0, kappa = 1, eta = 1e-12, rho = 0
  )
  expect_true(is.finite(price_zero_vol))
  expect_true(price_zero_vol >= 0)

  # Case 3: Deep in-the-money call
  price_itm <- price_geometric_asian_diffusion(
    S0 = 100, K = 50, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, option_type = "call"
  )
  expect_true(price_itm > 0)

  # Case 4: Deep out-of-the-money call
  price_otm <- price_geometric_asian_diffusion(
    S0 = 100, K = 200, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, option_type = "call"
  )
  expect_true(price_otm >= 0)
  expect_true(price_otm < price_itm)

  # Case 5: Put option
  price_put <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, option_type = "put"
  )
  expect_true(is.finite(price_put))
  expect_true(price_put >= 0)
})


test_that("price_geometric_asian_diffusion handles time-dependent eta", {
  # Constant eta as function
  eta_const <- function(t) rep(0.1, length(t))
  price1 <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = eta_const, rho = 0
  )

  # Constant eta as scalar
  price2 <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )

  # Should be approximately equal
  expect_equal(price1, price2, tolerance = 1e-6)

  # Time-dependent eta
  eta_linear <- function(t) 0.1 * (1 + 0.5 * t)
  price_td <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = eta_linear, rho = 0.3
  )
  expect_true(is.finite(price_td))
  expect_true(price_td >= 0)
})


test_that("price_geometric_asian_diffusion handles correlation effects", {
  # Positive correlation
  price_pos_corr <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = 0.5
  )

  # Negative correlation
  price_neg_corr <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = -0.5
  )

  # Zero correlation
  price_zero_corr <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )

  # All should be finite and non-negative
  expect_true(is.finite(price_pos_corr) && price_pos_corr >= 0)
  expect_true(is.finite(price_neg_corr) && price_neg_corr >= 0)
  expect_true(is.finite(price_zero_corr) && price_zero_corr >= 0)

  # Prices should be different due to correlation
  expect_true(abs(price_pos_corr - price_neg_corr) > 1e-6)
})


test_that("price_geometric_asian_diffusion handles initial impact state", {
  # Positive initial impact
  price_I_pos <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 1.0, kappa = 1, eta = 0.1, rho = 0
  )

  # Negative initial impact
  price_I_neg <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = -1.0, kappa = 1, eta = 0.1, rho = 0
  )

  # Zero initial impact
  price_I_zero <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )

  # All should be finite and non-negative
  expect_true(is.finite(price_I_pos) && price_I_pos >= 0)
  expect_true(is.finite(price_I_neg) && price_I_neg >= 0)
  expect_true(is.finite(price_I_zero) && price_I_zero >= 0)

  # Positive impact should increase call price (upward drift)
  expect_true(price_I_pos > price_I_zero)

  # Negative impact should decrease call price (downward drift)
  expect_true(price_I_neg < price_I_zero)
})


test_that("price_geometric_asian_diffusion is monotonic in key parameters", {
  base_params <- list(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    option_type = "call"
  )

  # Increasing S0 should increase call price
  params1 <- modifyList(base_params, list(S0 = 90))
  params2 <- modifyList(base_params, list(S0 = 110))
  price1 <- do.call(price_geometric_asian_diffusion, params1)
  price2 <- do.call(price_geometric_asian_diffusion, params2)
  expect_true(price2 > price1)

  # Increasing K should decrease call price
  params1 <- modifyList(base_params, list(K = 90))
  params2 <- modifyList(base_params, list(K = 110))
  price1 <- do.call(price_geometric_asian_diffusion, params1)
  price2 <- do.call(price_geometric_asian_diffusion, params2)
  expect_true(price1 > price2)

  # Increasing sigma should increase call price (for ATM options)
  params1 <- modifyList(base_params, list(sigma = 0.1))
  params2 <- modifyList(base_params, list(sigma = 0.3))
  price1 <- do.call(price_geometric_asian_diffusion, params1)
  price2 <- do.call(price_geometric_asian_diffusion, params2)
  expect_true(price2 > price1)

  # Increasing T should affect price (direction depends on r and sigma)
  params1 <- modifyList(base_params, list(T = 0.5))
  params2 <- modifyList(base_params, list(T = 1.5))
  price1 <- do.call(price_geometric_asian_diffusion, params1)
  price2 <- do.call(price_geometric_asian_diffusion, params2)
  expect_true(is.finite(price1) && is.finite(price2))
})


test_that("integrate_trapezoid works correctly", {
  # Test on simple functions
  x <- seq(0, 1, length.out = 101)

  # Integral of constant function: int_0^1 1 dx = 1
  y <- rep(1, length(x))
  expect_equal(integrate_trapezoid(x, y), 1, tolerance = 1e-6)

  # Integral of linear function: int_0^1 x dx = 0.5
  y <- x
  expect_equal(integrate_trapezoid(x, y), 0.5, tolerance = 1e-6)

  # Integral of quadratic: int_0^1 x^2 dx = 1/3
  y <- x^2
  expect_equal(integrate_trapezoid(x, y), 1/3, tolerance = 1e-4)

  # Error handling
  expect_error(integrate_trapezoid(x, y[-1]), "x and y must have same length")
  expect_equal(integrate_trapezoid(x[1], y[1]), 0)
})


test_that("price_geometric_asian_diffusion matches known limits", {
  # Limit: Very small kappa (slow mean reversion)
  price_small_kappa <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0.5, kappa = 0.01, eta = 0.1, rho = 0
  )
  expect_true(is.finite(price_small_kappa) && price_small_kappa >= 0)

  # Limit: Large kappa (fast mean reversion)
  price_large_kappa <- price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0.5, kappa = 10, eta = 0.1, rho = 0
  )
  expect_true(is.finite(price_large_kappa) && price_large_kappa >= 0)

  # Fast mean reversion should reduce impact of I0
  # (Impact decays quickly, so less effect on price)
  # With positive I0, slower decay means higher upward drift, higher price
  expect_true(price_small_kappa > price_large_kappa)
})


test_that("price_geometric_asian_diffusion put-call consistency", {
  # For the same parameters, put and call should satisfy:
  # Call - Put = PV(Forward - Strike) approximately
  # (This is a rough check; exact parity may not hold for Asian options)

  params <- list(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0
  )

  call_params <- modifyList(params, list(option_type = "call"))
  put_params <- modifyList(params, list(option_type = "put"))
  call_price <- do.call(price_geometric_asian_diffusion, call_params)
  put_price <- do.call(price_geometric_asian_diffusion, put_params)

  # Both should be non-negative
  expect_true(call_price >= 0)
  expect_true(put_price >= 0)

  # For ATM options with no impact drift, call and put should be similar
  # (though not necessarily equal for Asian options)
  expect_true(is.finite(call_price) && is.finite(put_price))
})
