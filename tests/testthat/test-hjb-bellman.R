test_that("validate_hjb_inputs catches invalid parameters", {
  base <- list(
    S0 = 100, K = 100, T = 1, N = 10, sigma = 0.2,
    r_cont = 0.05, kappa = 1, lambda_bar_T = 0.1,
    lambda_bar_P = 0.05, k_A = 0.01, k_B = 0.01,
    psi_cost = 1, eta = 1.0, p = 0.5, I0 = 0
  )

  expect_silent(do.call(validate_hjb_inputs, base))

  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(S0 = -1))),
    "S0 must be positive"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(K = 0))),
    "K must be positive"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(T = -1))),
    "T.*must be positive"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(N = 0))),
    "N must be a positive integer"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(sigma = 0))),
    "sigma must be positive"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(kappa = -1))),
    "kappa must be non-negative"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(lambda_bar_T = -0.1))),
    "lambda_bar_T must be non-negative"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(k_A = -1))),
    "k_A must be non-negative"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(psi_cost = 0))),
    "psi_cost must be in"
  )
  expect_error(
    do.call(validate_hjb_inputs,
            modifyList(base, list(p = 0))),
    "p.*must be in"
  )
})

test_that("validate_hjb_inputs warns for large kappa*dt", {
  expect_warning(
    validate_hjb_inputs(
      S0 = 100, K = 100, T = 1, N = 5, sigma = 0.2,
      r_cont = 0.05, kappa = 10, lambda_bar_T = 0,
      lambda_bar_P = 0, k_A = 0.01, k_B = 0.01,
      psi_cost = 1, eta = 1, p = 0.5, I0 = 0
    ),
    "alpha_m"
  )
})

test_that("arithmetic HJB returns bid, ask, and volumes", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 11
  )

  expect_s3_class(result, "hjb_asian")
  expect_true(is.numeric(result$ask_price))
  expect_true(is.numeric(result$bid_price))
  expect_true(result$ask_price >= 0)
  expect_true(result$bid_price >= 0)
  # In endogenous model, spread can be negative (manipulation surplus)
  expect_true(is.numeric(result$spread))

  # Optimal volumes for all N periods
  expect_length(result$optimal_nu, 10)
  expect_length(result$optimal_volumes, 10)
  expect_length(result$optimal_nu_buyer, 10)
  expect_length(result$optimal_volumes_buyer, 10)
  expect_true(all(is.numeric(result$optimal_nu)))
  expect_true(all(is.numeric(result$optimal_volumes)))

  expect_equal(result$asian_type, "arithmetic")
})

test_that("geometric HJB returns bid, ask, and volumes", {
  result <- price_geometric_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Z = 21
  )

  expect_s3_class(result, "hjb_asian")
  expect_true(result$ask_price >= 0)
  expect_true(result$bid_price >= 0)
  expect_length(result$optimal_nu, 10)
  expect_length(result$optimal_volumes, 10)
  expect_equal(result$asian_type, "geometric")
})

test_that("volumes equal nu times dt", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 11
  )

  dt <- 1 / 10
  expect_equal(
    result$optimal_volumes,
    result$optimal_nu * dt,
    tolerance = 1e-12
  )
  expect_equal(
    result$optimal_volumes_buyer,
    result$optimal_nu_buyer * dt,
    tolerance = 1e-12
  )
})

test_that("zero impact gives zero spread", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 0, lambda_bar_T = 0, lambda_bar_P = 0,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 21, n_logS = 21
  )

  # With no price impact, both choose nu=0 and spread = 0
  expect_equal(result$spread, 0, tolerance = 1e-10)
  expect_true(all(result$optimal_nu == 0))
  expect_true(all(result$optimal_nu_buyer == 0))
})

test_that("zero impact with high cost approximates standard", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 0, lambda_bar_T = 0, lambda_bar_P = 0,
    k_A = 100, k_B = 100, psi_cost = 1,
    n_I = 11, n_Y = 31, n_logS = 31,
    nu_min = -1, nu_max = 1, n_controls = 11
  )

  # Optimal nu should be near 0 due to high cost
  expect_true(all(abs(result$optimal_nu) <= 0.5))
  # Value should be reasonable for ATM Asian call
  expect_true(result$ask_price > 0)
  expect_true(result$ask_price < 50)
})

test_that("increasing cost increases spread", {
  low_cost <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.001, k_B = 0.001, psi_cost = 1,
    n_I = 11, n_Y = 21, n_logS = 21,
    nu_min = -3, nu_max = 3, n_controls = 21
  )

  high_cost <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 1.0, k_B = 1.0, psi_cost = 1,
    n_I = 11, n_Y = 21, n_logS = 21,
    nu_min = -3, nu_max = 3, n_controls = 21
  )

  # Higher cost => less manipulation => spread closer to zero
  # (spread is negative in endogenous model; higher cost makes it less negative)
  expect_true(
    high_cost$spread > low_cost$spread - 0.01
  )
})

test_that("value decreases as K increases for calls", {
  val_low_K <- price_arithmetic_asian_hjb(
    S0 = 100, K = 90, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 21
  )

  val_high_K <- price_arithmetic_asian_hjb(
    S0 = 100, K = 110, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 21
  )

  expect_true(val_low_K$ask_price > val_high_K$ask_price)
})

test_that("eta can be a vector of length N", {
  eta_vec <- seq(0.5, 1.5, length.out = 10)
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    eta = eta_vec, n_I = 11, n_Y = 11
  )

  expect_true(result$ask_price >= 0)
  expect_length(result$optimal_nu, 10)
})

test_that("custom control_set works", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 10,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    control_set = c(-2, -1, 0, 1, 2),
    n_I = 11, n_Y = 11
  )

  expect_equal(result$grid_sizes$n_controls, 5)
  expect_true(result$ask_price >= 0)
})

test_that("print.hjb_asian produces output", {
  result <- price_arithmetic_asian_hjb(
    S0 = 100, K = 100, T = 1, N = 5,
    sigma = 0.2, r_cont = 0.05,
    kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
    k_A = 0.01, k_B = 0.01, psi_cost = 1,
    n_I = 11, n_Y = 11
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("HJB Bellman Pricing", output)))
  expect_true(any(grepl("Ask price", output)))
  expect_true(any(grepl("Bid price", output)))
  expect_true(any(grepl("Spread", output)))
  expect_true(any(grepl("volume", output)))
})

test_that("eta length mismatch throws error", {
  expect_error(
    price_arithmetic_asian_hjb(
      S0 = 100, K = 100, T = 1, N = 10,
      sigma = 0.2, r_cont = 0.05,
      kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
      k_A = 0.01, k_B = 0.01, psi_cost = 1,
      eta = c(1, 2, 3),
      n_I = 11, n_Y = 11
    ),
    "eta must be a scalar or a vector of length N"
  )
})
