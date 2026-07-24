test_that("price_european default is call", {
  price_default <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_call <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "call")

  expect_equal(price_default, price_call)
})

test_that("price_european put increases with strike", {
  price_K90 <- price_european(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                               option_type = "put")
  price_K100 <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "put")
  price_K110 <- price_european(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "put")

  expect_true(price_K90 < price_K100)
  expect_true(price_K100 < price_K110)
})

test_that("price_european put decreases with stock price", {
  price_S90 <- price_european(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                               option_type = "put")
  price_S100 <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "put")
  price_S110 <- price_european(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "put")

  expect_true(price_S90 > price_S100)
  expect_true(price_S100 > price_S110)
})

test_that("price_european option_type validation works", {
  expect_no_error(
    price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                   option_type = "call")
  )
  expect_no_error(
    price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                   option_type = "put")
  )

  expect_error(
    price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                   option_type = "invalid"),
    "should be one of"
  )
})

test_that("price_european both call and put are positive for ATM", {
  call_price <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                option_type = "call")
  put_price <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                               option_type = "put")

  expect_true(call_price > 0)
  expect_true(put_price > 0)
})

test_that("price_european deep ITM put has high value", {

  price <- price_european(100, 500, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                          option_type = "put")

  expect_true(price > 200)
})

test_that("price_european deep OTM put has low value", {

  price <- price_european(100, 1, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                          option_type = "put")

  expect_true(price >= 0)
  expect_true(price < 0.01)
})

test_that("price_european put n=1 matches manual calculation", {
  S0 <- 100
  K <- 100
  r <- 1.05
  u <- 1.2
  d <- 0.8
  lambda <- 0.1
  v_u <- 1
  v_d <- 1

  u_tilde <- u * exp(lambda * v_u)
  d_tilde <- d * exp(-lambda * v_d)
  p_adj <- (r - d_tilde) / (u_tilde - d_tilde)

  S_u <- S0 * u_tilde
  S_d <- S0 * d_tilde

  payoff_u <- max(0, K - S_u)
  payoff_d <- max(0, K - S_d)

  expected_price <- (p_adj * payoff_u + (1 - p_adj) * payoff_d) / r

  computed_price <- price_european(S0, K, r, u, d, lambda, v_u, v_d, 1,
                                    option_type = "put")

  expect_equal(computed_price, expected_price, tolerance = 1e-10)
})
