# Tests for Monte Carlo pricing implementation

test_that("Monte Carlo produces valid output structure", {
  result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10,
    n_simulations = 10000, seed = 42
  )

  expect_type(result, "list")
  expect_s3_class(result, "geometric_asian_mc")
  expect_named(result, c("price", "std_error", "n_simulations",
                        "confidence_interval", "method"))

  expect_type(result$price, "double")
  expect_type(result$std_error, "double")
  expect_gt(result$price, 0)
  expect_gt(result$std_error, 0)

  expect_length(result$confidence_interval, 2)
  expect_named(result$confidence_interval, c("lower", "upper"))
  expect_lt(result$confidence_interval["lower"], result$price)
  expect_gt(result$confidence_interval["upper"], result$price)
})

test_that("Monte Carlo converges to exact solution for small n", {
  exact <- price_geometric_asian(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10, method = "exact"
  )

  mc <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10,
    n_simulations = 100000, seed = 42
  )

  expect_lt(abs(mc$price - exact), 3 * mc$std_error)
})

test_that("Monte Carlo seed produces reproducible results", {
  params <- list(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 15,
    n_simulations = 10000, seed = 123
  )

  result1 <- do.call(price_geometric_asian_mc, params)
  result2 <- do.call(price_geometric_asian_mc, params)

  expect_equal(result1$price, result2$price)
  expect_equal(result1$std_error, result2$std_error)
})

test_that("Monte Carlo without seed produces different results", {
  params <- list(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 15,
    n_simulations = 10000, seed = NULL
  )

  result1 <- do.call(price_geometric_asian_mc, params)
  result2 <- do.call(price_geometric_asian_mc, params)

  expect_false(result1$price == result2$price)
})

test_that("Monte Carlo standard error decreases with more simulations", {
  params <- list(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 15
  )

  mc_small <- do.call(price_geometric_asian_mc,
                      c(params, list(n_simulations = 10000, seed = 42)))

  mc_large <- do.call(price_geometric_asian_mc,
                      c(params, list(n_simulations = 40000, seed = 42)))

  ratio <- mc_small$std_error / mc_large$std_error
  expect_gt(ratio, 1.5)
  expect_lt(ratio, 2.5)
})

test_that("Monte Carlo works for very large n", {

  suppressWarnings({
    result <- price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 50,
      n_simulations = 10000, seed = 42
    )
  })

  expect_gt(result$price, 0)
  expect_gt(result$std_error, 0)
  expect_type(result$price, "double")
})

test_that("Monte Carlo handles put options correctly", {
  call_result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 15,
    n_simulations = 50000, option_type = "call", seed = 42
  )

  put_result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 15,
    n_simulations = 50000, option_type = "put", seed = 42
  )

  expect_false(call_result$price == put_result$price)

  expect_gt(call_result$price, 0)
  expect_gt(put_result$price, 0)
})

test_that("Monte Carlo respects price impact", {

  no_impact <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0, v_u = 0, v_d = 0, n = 15,
    n_simulations = 50000, seed = 42
  )

  with_impact <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.2, v_u = 1, v_d = 1, n = 15,
    n_simulations = 50000, seed = 42
  )

  expect_false(no_impact$price == with_impact$price)

  expect_gt(with_impact$price, no_impact$price - 3 * with_impact$std_error)
})

test_that("Monte Carlo validation rejects invalid inputs", {

  expect_error(
    price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 10,
      n_simulations = -1000
    ),
    "n_simulations must be a positive integer"
  )

  expect_error(
    price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 10,
      n_simulations = 0
    ),
    "n_simulations must be a positive integer"
  )

  expect_error(
    price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 10,
      n_simulations = 1000.5
    ),
    "n_simulations must be a positive integer"
  )

  expect_error(
    price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 10,
      seed = -5
    ),
    "seed must be NULL or a non-negative integer"
  )

  expect_error(
    price_geometric_asian_mc(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 10,
      option_type = "invalid"
    ),
    "'arg' should be one of"
  )
})

test_that("Main function auto-selects method correctly", {

  small_n <- price_geometric_asian(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10
  )
  expect_type(small_n, "double")

  suppressWarnings({
    expect_message(
      large_n <- price_geometric_asian(
        S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
        lambda = 0.1, v_u = 1, v_d = 1, n = 25
      ),
      "Using Monte Carlo method"
    )
  })
  expect_type(large_n, "double")
})

test_that("Main function respects method parameter", {

  expect_warning(
    exact <- price_geometric_asian(
      S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
      lambda = 0.1, v_u = 1, v_d = 1, n = 22, method = "exact"
    ),
    "will enumerate"
  )

  mc <- price_geometric_asian(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10,
    method = "mc", n_simulations = 10000, seed = 42
  )

  expect_type(mc, "double")
})

test_that("Print method for MC results works", {
  result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10,
    n_simulations = 10000, seed = 42
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("Geometric Asian Option Price", output)))
  expect_true(any(grepl("Price:", output)))
  expect_true(any(grepl("Std Error:", output)))
  expect_true(any(grepl("95% CI:", output)))
  expect_true(any(grepl("Simulations:", output)))
})

test_that("MC confidence interval has correct width", {
  result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 1, v_d = 1, n = 10,
    n_simulations = 10000, seed = 42
  )

  expected_margin <- 1.96 * result$std_error
  actual_margin <- as.numeric((result$confidence_interval["upper"] -
                               result$confidence_interval["lower"]) / 2)

  expect_equal(actual_margin, expected_margin, tolerance = 1e-10)
})

test_that("MC works with asymmetric hedging volumes", {

  result <- price_geometric_asian_mc(
    S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
    lambda = 0.1, v_u = 2, v_d = 0.5, n = 15,
    n_simulations = 20000, seed = 42
  )

  expect_gt(result$price, 0)
  expect_type(result$price, "double")
})
