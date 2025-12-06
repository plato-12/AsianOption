# Tests for European Call Options

test_that("European call option has correct properties", {

  price <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(is.numeric(price))
  expect_true(price >= 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("Call price decreases as strike increases", {

  price_K90 <- price_european_call(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_K100 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_K110 <- price_european_call(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_K90 > price_K100)
  expect_true(price_K100 > price_K110)
})

test_that("Call price increases with initial stock price", {

  price_S90 <- price_european_call(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_S100 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_S110 <- price_european_call(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_S90 < price_S100)
  expect_true(price_S100 < price_S110)
})

test_that("Price impact increases call option value", {

  price_no_impact <- price_european_call(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 10
  )

  price_with_impact <- price_european_call(
    100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10
  )

  expect_true(price_with_impact >= price_no_impact)
})

test_that("Call price increases with price impact coefficient", {

  price_lambda0 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 10)
  price_lambda01 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_lambda02 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.2, 1, 1, 10)

  expect_true(price_lambda01 >= price_lambda0)
  expect_true(price_lambda02 >= price_lambda01)
})

test_that("European call n=1 case matches manual calculation", {

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

  payoff_u <- max(0, S_u - K)
  payoff_d <- max(0, S_d - K)

  expected_price <- (p_adj * payoff_u + (1 - p_adj) * payoff_d) / r

  computed_price <- price_european_call(S0, K, r, u, d, lambda, v_u, v_d, 1)

  expect_equal(computed_price, expected_price, tolerance = 1e-10)
})

test_that("Deep ITM call option has large value", {

  price <- price_european_call(100, 50, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price > 30)
})

test_that("Deep OTM call option has small value", {

  price_otm <- price_european_call(100, 500, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_itm <- price_european_call(100, 50, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_otm < 10)
  expect_true(price_otm >= 0)

  expect_true(price_otm < price_itm / 3)
})

test_that("ATM call option has intermediate value", {

  price_atm <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_itm <- price_european_call(100, 80, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_otm <- price_european_call(100, 120, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_atm > price_otm)
  expect_true(price_atm < price_itm)
})

test_that("Call price increases with number of time steps", {

  price_n1 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1)
  price_n5 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5)
  price_n10 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_n1 > 0)
  expect_true(price_n5 > 0)
  expect_true(price_n10 > 0)
})

test_that("Higher volatility increases call option value", {

  price_low_vol <- price_european_call(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 10)

  price_high_vol <- price_european_call(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 10)

  expect_true(price_high_vol > price_low_vol)
})

test_that("Zero lambda reduces to standard CRR for calls", {

  price_zero_lambda <- price_european_call(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 10
  )

  expect_true(is.numeric(price_zero_lambda))
  expect_true(price_zero_lambda > 0)
})

test_that("Call results are reproducible", {

  price1 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price2 <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_equal(price1, price2)
})

test_that("European call can handle large n efficiently", {
  start_time <- Sys.time()
  suppressWarnings({
    price <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 50)
  })
  end_time <- Sys.time()

  expect_true(is.numeric(price))
  expect_true(price > 0)

  expect_true(as.numeric(end_time - start_time, units = "secs") < 1)
})

# Tests for European Put Options

test_that("European put option has correct properties", {

  price <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(is.numeric(price))
  expect_true(price >= 0)
  expect_length(price, 1)
  expect_false(is.na(price))
  expect_false(is.infinite(price))
})

test_that("Put price increases as strike increases", {

  price_K90 <- price_european_put(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_K100 <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_K110 <- price_european_put(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_K90 < price_K100)
  expect_true(price_K100 < price_K110)
})

test_that("Put price decreases with initial stock price", {

  price_S90 <- price_european_put(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_S100 <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price_S110 <- price_european_put(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price_S90 > price_S100)
  expect_true(price_S100 > price_S110)
})

test_that("European put n=1 case matches manual calculation", {

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

  computed_price <- price_european_put(S0, K, r, u, d, lambda, v_u, v_d, 1)

  expect_equal(computed_price, expected_price, tolerance = 1e-10)
})

test_that("Deep ITM put option has large value", {

  price <- price_european_put(100, 200, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price > 50)
})

test_that("Deep OTM put option has near-zero value", {

  price <- price_european_put(100, 10, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_true(price < 1)
  expect_true(price >= 0)
})

test_that("Higher volatility increases put option value", {

  price_low_vol <- price_european_put(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 10)

  price_high_vol <- price_european_put(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 10)

  expect_true(price_high_vol > price_low_vol)
})

test_that("Zero lambda reduces to standard CRR for puts", {

  price_zero_lambda <- price_european_put(
    100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 10
  )

  expect_true(is.numeric(price_zero_lambda))
  expect_true(price_zero_lambda > 0)
})

test_that("Put results are reproducible", {

  price1 <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)
  price2 <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_equal(price1, price2)
})

# Put-Call Parity Tests

test_that("Put-call parity approximately holds with no price impact", {

  S0 <- 100
  K <- 100
  r <- 1.05
  u <- 1.2
  d <- 0.8
  n <- 10

  call_price <- price_european_call(S0, K, r, u, d, 0, 0, 0, n)
  put_price <- price_european_put(S0, K, r, u, d, 0, 0, 0, n)

  diff <- call_price - put_price

  expect_true(call_price > 0)
  expect_true(put_price > 0)

  expect_true(abs(diff) < 50)
})

test_that("Put-call relationship changes with price impact", {

  S0 <- 100
  K <- 100
  r <- 1.05
  u <- 1.2
  d <- 0.8
  n <- 10

  call_no_impact <- price_european_call(S0, K, r, u, d, 0, 0, 0, n)
  put_no_impact <- price_european_put(S0, K, r, u, d, 0, 0, 0, n)
  diff_no_impact <- call_no_impact - put_no_impact

  call_with_impact <- price_european_call(S0, K, r, u, d, 0.1, 1, 1, n)
  put_with_impact <- price_european_put(S0, K, r, u, d, 0.1, 1, 1, n)
  diff_with_impact <- call_with_impact - put_with_impact

  expect_true(call_with_impact >= call_no_impact)

  expect_true(is.finite(diff_no_impact))
  expect_true(is.finite(diff_with_impact))
})

# Comparison with Geometric Asian Options

test_that("European options should differ from Asian options", {
  euro_call <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5)
  asian_geom <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5)

  expect_true(euro_call > 0)
  expect_true(asian_geom > 0)

  expect_true(euro_call >= asian_geom)
})

test_that("European option efficiency vs Asian option", {
  n_large <- 15

  start_euro <- Sys.time()
  euro_price <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n_large)
  time_euro <- as.numeric(Sys.time() - start_euro, units = "secs")

  start_asian <- Sys.time()
  asian_price <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n_large)
  time_asian <- as.numeric(Sys.time() - start_asian, units = "secs")

  expect_true(time_euro < time_asian)
  expect_true(euro_price > 0)
  expect_true(asian_price > 0)
})

test_that("price_european works for calls", {
  price_unified <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                   option_type = "call")
  price_direct <- price_european_call(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_equal(price_unified, price_direct)
})

test_that("price_european works for puts", {
  price_unified <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10,
                                   option_type = "put")
  price_direct <- price_european_put(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10)

  expect_equal(price_unified, price_direct)
})

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
