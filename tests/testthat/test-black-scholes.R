test_that("Black-Scholes call option has correct properties", {

  price <- price_black_scholes_call(S0 = 100, K = 100, r = 0.05,
                                     sigma = 0.2, time_to_maturity = 1)

  expect_true(is.numeric(price))
  expect_true(price > 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("BS call price decreases as strike increases", {

  price_K90 <- price_black_scholes_call(100, 90, 0.05, 0.2, 1)
  price_K100 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price_K110 <- price_black_scholes_call(100, 110, 0.05, 0.2, 1)

  expect_true(price_K90 > price_K100)
  expect_true(price_K100 > price_K110)
})

test_that("BS call price increases with initial stock price", {

  price_S90 <- price_black_scholes_call(90, 100, 0.05, 0.2, 1)
  price_S100 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price_S110 <- price_black_scholes_call(110, 100, 0.05, 0.2, 1)

  expect_true(price_S90 < price_S100)
  expect_true(price_S100 < price_S110)
})

test_that("BS call price increases with volatility", {

  price_low_vol <- price_black_scholes_call(100, 100, 0.05, 0.1, 1)
  price_mid_vol <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price_high_vol <- price_black_scholes_call(100, 100, 0.05, 0.3, 1)

  expect_true(price_low_vol < price_mid_vol)
  expect_true(price_mid_vol < price_high_vol)
})

test_that("BS call price increases with time to maturity", {

  price_T025 <- price_black_scholes_call(100, 100, 0.05, 0.2, 0.25)
  price_T05 <- price_black_scholes_call(100, 100, 0.05, 0.2, 0.5)
  price_T1 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1.0)

  expect_true(price_T025 < price_T05)
  expect_true(price_T05 < price_T1)
})

test_that("BS call price increases with interest rate", {

  price_r001 <- price_black_scholes_call(100, 100, 0.01, 0.2, 1)
  price_r005 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price_r010 <- price_black_scholes_call(100, 100, 0.10, 0.2, 1)

  expect_true(price_r001 < price_r005)
  expect_true(price_r005 < price_r010)
})

test_that("BS call with zero volatility gives discounted intrinsic value", {

  S0 <- 100
  K <- 100
  r <- 0.05
  T <- 1

  price <- price_black_scholes_call(S0, K, r, sigma = 0, T)

  S_T <- S0 * exp(r * T)
  expected <- max(0, S_T - K) * exp(-r * T)

  expect_equal(price, expected, tolerance = 1e-10)
})

test_that("BS call deep ITM approaches intrinsic value", {

  S0 <- 100
  K <- 50
  price <- price_black_scholes_call(S0, K, 0.05, 0.2, 1)

  intrinsic <- S0 - K * exp(-0.05 * 1)

  expect_true(price > intrinsic)
  expect_true(price < S0)
  expect_true(price > 40)
})

test_that("BS call deep OTM has small value", {

  price <- price_black_scholes_call(100, 200, 0.05, 0.2, 1)

  expect_true(price < 1)
  expect_true(price >= 0)
})

test_that("BS call ATM has intermediate value", {

  price_itm <- price_black_scholes_call(100, 80, 0.05, 0.2, 1)
  price_atm <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price_otm <- price_black_scholes_call(100, 120, 0.05, 0.2, 1)

  expect_true(price_atm > price_otm)
  expect_true(price_atm < price_itm)
})

test_that("BS call input validation works", {

  expect_error(
    price_black_scholes_call(-100, 100, 0.05, 0.2, 1),
    "S0 must be a positive number"
  )

  expect_error(
    price_black_scholes_call(100, -100, 0.05, 0.2, 1),
    "K must be a positive number"
  )

  expect_error(
    price_black_scholes_call(100, 100, 0.05, -0.2, 1),
    "sigma must be a non-negative number"
  )

  expect_error(
    price_black_scholes_call(100, 100, 0.05, 0.2, -1),
    "time_to_maturity must be a positive number"
  )

  expect_error(
    price_black_scholes_call(100, 100, 0.05, 0.2, 0),
    "time_to_maturity must be a positive number"
  )
})

test_that("BS call results are reproducible", {
  price1 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)
  price2 <- price_black_scholes_call(100, 100, 0.05, 0.2, 1)

  expect_equal(price1, price2)
})

test_that("Black-Scholes put option has correct properties", {

  price <- price_black_scholes_put(S0 = 100, K = 100, r = 0.05,
                                    sigma = 0.2, time_to_maturity = 1)

  expect_true(is.numeric(price))
  expect_true(price > 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("BS put price increases as strike increases", {

  price_K90 <- price_black_scholes_put(100, 90, 0.05, 0.2, 1)
  price_K100 <- price_black_scholes_put(100, 100, 0.05, 0.2, 1)
  price_K110 <- price_black_scholes_put(100, 110, 0.05, 0.2, 1)

  expect_true(price_K90 < price_K100)
  expect_true(price_K100 < price_K110)
})

test_that("BS put price decreases with initial stock price", {

  price_S90 <- price_black_scholes_put(90, 100, 0.05, 0.2, 1)
  price_S100 <- price_black_scholes_put(100, 100, 0.05, 0.2, 1)
  price_S110 <- price_black_scholes_put(110, 100, 0.05, 0.2, 1)

  expect_true(price_S90 > price_S100)
  expect_true(price_S100 > price_S110)
})

test_that("BS put price increases with volatility", {

  price_low_vol <- price_black_scholes_put(100, 100, 0.05, 0.1, 1)
  price_mid_vol <- price_black_scholes_put(100, 100, 0.05, 0.2, 1)
  price_high_vol <- price_black_scholes_put(100, 100, 0.05, 0.3, 1)

  expect_true(price_low_vol < price_mid_vol)
  expect_true(price_mid_vol < price_high_vol)
})

test_that("BS put with zero volatility gives discounted intrinsic value", {

  S0 <- 100
  K <- 100
  r <- 0.05
  T <- 1

  price <- price_black_scholes_put(S0, K, r, sigma = 0, T)

  S_T <- S0 * exp(r * T)
  expected <- max(0, K - S_T) * exp(-r * T)

  expect_equal(price, expected, tolerance = 1e-10)
})

test_that("BS put deep ITM has large value", {

  price <- price_black_scholes_put(100, 150, 0.05, 0.2, 1)

  max_value <- 150 * exp(-0.05 * 1)

  expect_true(price > 30)
  expect_true(price < max_value)
})

test_that("BS put deep OTM has small value", {

  price <- price_black_scholes_put(100, 50, 0.05, 0.2, 1)

  expect_true(price < 1)
  expect_true(price >= 0)
})

test_that("BS put input validation works", {

  expect_error(
    price_black_scholes_put(-100, 100, 0.05, 0.2, 1),
    "S0 must be a positive number"
  )

  expect_error(
    price_black_scholes_put(100, -100, 0.05, 0.2, 1),
    "K must be a positive number"
  )

  expect_error(
    price_black_scholes_put(100, 100, 0.05, -0.2, 1),
    "sigma must be a non-negative number"
  )

  expect_error(
    price_black_scholes_put(100, 100, 0.05, 0.2, -1),
    "time_to_maturity must be a positive number"
  )
})

test_that("BS put results are reproducible", {
  price1 <- price_black_scholes_put(100, 100, 0.05, 0.2, 1)
  price2 <- price_black_scholes_put(100, 100, 0.05, 0.2, 1)

  expect_equal(price1, price2)
})

test_that("Black-Scholes put-call parity holds exactly", {

  S0 <- 100
  K <- 100
  r <- 0.05
  sigma <- 0.2
  T <- 1

  call_price <- price_black_scholes_call(S0, K, r, sigma, T)
  put_price <- price_black_scholes_put(S0, K, r, sigma, T)

  parity_lhs <- call_price - put_price
  parity_rhs <- S0 - K * exp(-r * T)

  expect_equal(parity_lhs, parity_rhs, tolerance = 1e-10)
})

test_that("Put-call parity holds for various parameter combinations", {

  scenarios <- list(
    list(S0 = 100, K = 100, r = 0.05, sigma = 0.2, time_to_maturity = 1),
    list(S0 = 50, K = 60, r = 0.03, sigma = 0.3, time_to_maturity = 0.5),
    list(S0 = 150, K = 140, r = 0.08, sigma = 0.15, time_to_maturity = 2),
    list(S0 = 80, K = 100, r = 0.02, sigma = 0.4, time_to_maturity = 0.25)
  )

  for (scenario in scenarios) {
    call <- price_black_scholes_call(scenario$S0, scenario$K, scenario$r,
                                      scenario$sigma, scenario$time_to_maturity)
    put <- price_black_scholes_put(scenario$S0, scenario$K, scenario$r,
                                    scenario$sigma, scenario$time_to_maturity)

    parity_lhs <- call - put
    parity_rhs <- scenario$S0 - scenario$K * exp(-scenario$r * scenario$time_to_maturity)

    expect_equal(parity_lhs, parity_rhs, tolerance = 1e-10,
                 info = paste("Failed for S0 =", scenario$S0, "K =", scenario$K))
  }
})

test_that("Put-call parity holds with zero volatility", {

  S0 <- 100
  K <- 100
  r <- 0.05
  T <- 1

  call_price <- price_black_scholes_call(S0, K, r, 0, T)
  put_price <- price_black_scholes_put(S0, K, r, 0, T)

  parity_lhs <- call_price - put_price
  parity_rhs <- S0 - K * exp(-r * T)

  expect_equal(parity_lhs, parity_rhs, tolerance = 1e-10)
})

test_that("BS handles very short time to maturity", {

  S0 <- 105
  K <- 100
  T_small <- 1e-6

  call_price <- price_black_scholes_call(S0, K, 0.05, 0.2, T_small)
  put_price <- price_black_scholes_put(S0, K, 0.05, 0.2, T_small)

  expect_true(abs(call_price - (S0 - K)) < 0.1)
  expect_true(put_price < 0.1)
})

test_that("BS handles very high volatility", {

  call_price <- price_black_scholes_call(100, 100, 0.05, 2.0, 1)
  put_price <- price_black_scholes_put(100, 100, 0.05, 2.0, 1)

  expect_true(call_price > 20)
  expect_true(put_price > 20)
})

test_that("BS handles very low volatility", {

  call_price <- price_black_scholes_call(100, 100, 0.05, 0.01, 1)
  put_price <- price_black_scholes_put(100, 100, 0.05, 0.01, 1)

  call_zero <- price_black_scholes_call(100, 100, 0.05, 0, 1)
  put_zero <- price_black_scholes_put(100, 100, 0.05, 0, 1)

  expect_true(abs(call_price - call_zero) < 0.5)
  expect_true(abs(put_price - put_zero) < 0.5)
})

test_that("BS handles extreme strikes", {
  S0 <- 100

  call_low_K <- price_black_scholes_call(S0, 1, 0.05, 0.2, 1)
  expect_true(call_low_K > S0 * 0.9)

  call_high_K <- price_black_scholes_call(S0, 1000, 0.05, 0.2, 1)
  expect_true(call_high_K < 0.01)

  put_low_K <- price_black_scholes_put(S0, 1, 0.05, 0.2, 1)
  expect_true(put_low_K < 0.01)

  put_high_K <- price_black_scholes_put(S0, 500, 0.05, 0.2, 1)
  expect_true(put_high_K > 300)
})

test_that("BS handles negative interest rates", {

  call_price <- price_black_scholes_call(100, 100, -0.01, 0.2, 1)
  put_price <- price_black_scholes_put(100, 100, -0.01, 0.2, 1)

  expect_true(is.numeric(call_price))
  expect_true(is.numeric(put_price))
  expect_true(call_price > 0)
  expect_true(put_price > 0)

  parity_lhs <- call_price - put_price
  parity_rhs <- 100 - 100 * exp(0.01 * 1)
  expect_equal(parity_lhs, parity_rhs, tolerance = 1e-10)
})
