test_that("Geometric Asian option has correct properties", {

  price <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(is.numeric(price))
  expect_true(price >= 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("Price decreases as strike increases", {

  price_K90 <- price_geometric_asian(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_K100 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_K110 <- price_geometric_asian(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(price_K90 > price_K100)
  expect_true(price_K100 > price_K110)
})

test_that("Price increases with initial stock price", {

  price_S90 <- price_geometric_asian(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_S100 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_S110 <- price_geometric_asian(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(price_S90 < price_S100)
  expect_true(price_S100 < price_S110)
})

test_that("Price impact increases option value for calls", {

  price_no_impact <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 3
  )

  price_with_impact <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3
  )

  expect_true(price_with_impact >= price_no_impact)
})

test_that("Price increases with price impact coefficient", {

  price_lambda0 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 3)
  price_lambda01 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_lambda02 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.2, 1, 1, 3)

  expect_true(price_lambda01 >= price_lambda0)
  expect_true(price_lambda02 >= price_lambda01)
})

test_that("n=1 case matches manual calculation", {

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

  G_U <- S0 * sqrt(u_tilde)
  payoff_U <- max(0, G_U - K)

  G_D <- S0 * sqrt(d_tilde)
  payoff_D <- max(0, G_D - K)

  expected_price <- (p_adj * payoff_U + (1 - p_adj) * payoff_D) / r

  computed_price <- price_geometric_asian(S0, K, r, u, d, lambda, v_u, v_d, 1)

  expect_equal(computed_price, expected_price, tolerance = 1e-10)
})

test_that("Deep ITM option has large value", {

  price <- price_geometric_asian(100, 1, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(price > 50)
})

test_that("Deep OTM option has near-zero value", {

  price <- price_geometric_asian(100, 500, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(price < 1)
  expect_true(price >= 0)
})

test_that("ATM option has intermediate value", {

  price_atm <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_itm <- price_geometric_asian(100, 80, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_otm <- price_geometric_asian(100, 120, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(price_atm > price_otm)
  expect_true(price_atm < price_itm)
})

test_that("Price increases with number of time steps", {

  price_n1 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1)
  price_n3 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price_n5 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5)

  expect_true(price_n1 > 0)
  expect_true(price_n3 > 0)
  expect_true(price_n5 > 0)
})

test_that("Symmetric volumes give consistent results", {

  price_sym <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  price_diff <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 0.5, 1.5, 3)

  expect_true(is.numeric(price_sym))
  expect_true(is.numeric(price_diff))
  expect_true(price_sym > 0)
  expect_true(price_diff > 0)
})

test_that("Higher volatility increases option value", {

  price_low_vol <- price_geometric_asian(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 3)

  price_high_vol <- price_geometric_asian(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 3)

  expect_true(price_high_vol > price_low_vol)
})

test_that("Zero lambda reduces to standard CRR", {
  price_zero_lambda <- price_geometric_asian(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 3
  )

  expect_true(is.numeric(price_zero_lambda))
  expect_true(price_zero_lambda > 0)
})

test_that("Results are reproducible", {
  price1 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  price2 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_equal(price1, price2)
})

test_that("Geometric Asian put option has correct properties", {

  price <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                  option_type = "put")

  expect_true(is.numeric(price))
  expect_true(price >= 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("Put price increases as strike increases", {

  price_K90 <- price_geometric_asian(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")
  price_K100 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                       option_type = "put")
  price_K110 <- price_geometric_asian(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                       option_type = "put")

  expect_true(price_K90 < price_K100)
  expect_true(price_K100 < price_K110)
})

test_that("Put price decreases with initial stock price", {

  price_S90 <- price_geometric_asian(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")
  price_S100 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                       option_type = "put")
  price_S110 <- price_geometric_asian(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                       option_type = "put")

  expect_true(price_S90 > price_S100)
  expect_true(price_S100 > price_S110)
})

test_that("Put n=1 case matches manual calculation", {

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

  G_U <- S0 * sqrt(u_tilde)
  payoff_U <- max(0, K - G_U)

  G_D <- S0 * sqrt(d_tilde)
  payoff_D <- max(0, K - G_D)

  expected_price <- (p_adj * payoff_U + (1 - p_adj) * payoff_D) / r

  computed_price <- price_geometric_asian(S0, K, r, u, d, lambda, v_u, v_d, 1,
                                           option_type = "put")

  expect_equal(computed_price, expected_price, tolerance = 1e-10)
})

test_that("Deep ITM put has large value", {

  price <- price_geometric_asian(100, 500, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                  option_type = "put")

  expect_true(price > 200)
})

test_that("Deep OTM put has near-zero value", {
  price <- price_geometric_asian(100, 1, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                  option_type = "put")

  expect_true(price < 1)
  expect_true(price >= 0)
})

test_that("ATM put has intermediate value", {

  price_atm <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")
  price_itm <- price_geometric_asian(100, 120, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")
  price_otm <- price_geometric_asian(100, 80, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")

  expect_true(price_atm > price_otm)
  expect_true(price_atm < price_itm)
})

test_that("Call and put satisfy put-call relationship", {
  call_price <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                       option_type = "call")
  put_price <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")

  expect_true(call_price > 0)
  expect_true(put_price > 0)

  expect_true(call_price > 0 && put_price > 0)
})

test_that("Put results are reproducible", {

  price1 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                   option_type = "put")
  price2 <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                   option_type = "put")

  expect_equal(price1, price2)
})

test_that("Higher volatility increases put value", {

  price_low_vol <- price_geometric_asian(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 3,
                                          option_type = "put")

  price_high_vol <- price_geometric_asian(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 3,
                                           option_type = "put")

  expect_true(price_high_vol > price_low_vol)
})

test_that("option_type parameter validation works", {
  expect_no_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                          option_type = "call")
  )
  expect_no_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                          option_type = "put")
  )

  expect_error(
    price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                          option_type = "invalid"),
    "should be one of"
  )
})
