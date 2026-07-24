test_that("Input validation catches invalid parameters", {

  expect_error(
    price_geometric_asian(-100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3),
    "S0 must be positive"
  )

  expect_error(
    price_geometric_asian(100, -100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3),
    "K must be positive"
  )

  expect_error(
    price_geometric_asian(100, 100, -1, 1.2, 0.8, 0.1, 1, 1, 3),
    "r must be positive"
  )

  expect_error(
    price_geometric_asian(100, 100, 0, 1.2, 0.8, 0.1, 1, 1, 3),
    "r must be positive"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, -1.2, 0.8, 0.1, 1, 1, 3),
    "u must be positive"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, -0.8, 0.1, 1, 1, 3),
    "d must be positive"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 0.8, 1.2, 0.1, 1, 1, 3),
    "Up factor u must be greater than down factor d"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.0, 1.0, 0.1, 1, 1, 3),
    "Up factor u must be greater than down factor d"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, -0.1, 1, 1, 3),
    "lambda must be non-negative"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, -1, 1, 3),
    "v_u must be non-negative"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, -1, 3),
    "v_d must be non-negative"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3.5),
    "n must be a positive integer"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, -3),
    "n must be a positive integer"
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 0),
    "n must be a positive integer"
  )
})

test_that("No-arbitrage violation is detected", {

  expect_error(
    price_geometric_asian(100, 100, 2.0, 1.2, 0.8, 0.1, 1, 1, 3),
    "No-arbitrage condition violated.*r.*>=.*u_tilde"
  )

  expect_error(
    price_geometric_asian(100, 100, 0.5, 1.2, 0.8, 0.1, 1, 1, 3),
    "No-arbitrage condition violated.*d_tilde.*>=.*r"
  )
})

test_that("Warning issued for large n", {
  expect_warning(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 21),
    "2\\^21"
  )

  expect_warning(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 25),
    "2\\^25.*(33554432|3\\.355.*e\\+07)"
  )

  expect_warning(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 20),
    NA
  )
})

test_that("Validation can be disabled", {
  result <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
    validate = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("Arithmetic bounds validation works", {

  expect_error(
    arithmetic_asian_bounds(-100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3),
    "S0 must be positive"
  )

  expect_error(
    arithmetic_asian_bounds(100, 100, 1.05, 0.8, 1.2, 0.1, 1, 1, 3),
    "Up factor u must be greater than down factor d"
  )

  expect_error(
    arithmetic_asian_bounds(100, 100, 2.0, 1.2, 0.8, 0.1, 1, 1, 3),
    "No-arbitrage condition violated"
  )

  expect_warning(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 22),
    "2\\^22"
  )
})

test_that("Edge cases in validation", {

  result <- price_geometric_asian(
    0.01, 0.01, 1.01, 1.1, 0.9, 0.01, 0.1, 0.1, 2
  )
  expect_true(is.numeric(result))

  result <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 3
  )
  expect_true(is.numeric(result))

  result <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1
  )
  expect_true(is.numeric(result))
})
