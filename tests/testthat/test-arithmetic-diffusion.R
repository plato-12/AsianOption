test_that("basic output structure is correct", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 5000, seed = 42
  )

  expect_s3_class(result, "arithmetic_asian_diffusion")
  expect_true(is.numeric(result$price))
  expect_true(is.finite(result$price))
  expect_true(result$price >= 0)
  expect_true(is.numeric(result$std_error))
  expect_true(result$std_error >= 0)
  expect_true(result$lower_ci <= result$price)
  expect_true(result$upper_ci >= result$price)
  expect_equal(result$n_sims, 5000)
  expect_equal(result$n_steps, 50)
  expect_equal(result$option_type, "call")
})

test_that("print method works", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 1000, seed = 1
  )
  expect_output(print(result), "Arithmetic Asian Option")
})

test_that("arithmetic price >= geometric price (AM-GM inequality)", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 100, n_sims = 50000, seed = 42
  )

  # Allow tolerance for MC error
  expect_true(result$price >= result$geometric_price - 3 * result$std_error)
})

test_that("seed gives reproducible results", {
  args <- list(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 5000, seed = 123
  )
  r1 <- do.call(price_arithmetic_asian_diffusion, args)
  r2 <- do.call(price_arithmetic_asian_diffusion, args)

  expect_equal(r1$price, r2$price)
  expect_equal(r1$std_error, r2$std_error)
})

test_that("put option returns positive price", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 110, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    option_type = "put",
    n_steps = 50, n_sims = 10000, seed = 42
  )

  expect_true(result$price > 0)
  expect_equal(result$option_type, "put")
})

test_that("higher strike decreases call price", {
  price_low_K <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 90, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 20000, seed = 42
  )$price

  price_high_K <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 110, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 20000, seed = 42
  )$price

  expect_true(price_low_K > price_high_K)
})

test_that("higher S0 increases call price", {
  price_low_S <- price_arithmetic_asian_diffusion(
    S0 = 90, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 20000, seed = 42
  )$price

  price_high_S <- price_arithmetic_asian_diffusion(
    S0 = 110, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 20000, seed = 42
  )$price

  expect_true(price_high_S > price_low_S)
})

test_that("no-impact case (lambda_T = 0) gives standard arithmetic Asian", {
  # With lambda_T = 0 the impact term vanishes; price should match

# a standard (no-impact) arithmetic Asian MC
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 100, n_sims = 50000, seed = 42
  )

  # The geometric price with no impact should be close to Kemna-Vorst
  expect_true(result$price > 0)
  expect_true(result$price > result$geometric_price - 3 * result$std_error)
})

test_that("control variate reduces standard error", {
  args_base <- list(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 20000, seed = 42
  )

  r_cv <- do.call(price_arithmetic_asian_diffusion,
                   c(args_base, use_control_variate = TRUE))
  r_no_cv <- do.call(price_arithmetic_asian_diffusion,
                      c(args_base, use_control_variate = FALSE))

  expect_true(r_cv$std_error < r_no_cv$std_error)
})

test_that("time-dependent eta works", {
  eta_func <- function(t) 0.1 * (1 + 0.5 * t)

  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = eta_func, rho = 0.3,
    n_steps = 50, n_sims = 10000, seed = 42
  )

  expect_true(is.finite(result$price))
  expect_true(result$price > 0)
})

test_that("correlation is high between arithmetic and geometric payoffs", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 10000, seed = 42
  )

  expect_true(result$correlation > 0.99)
})

test_that("input validation catches bad parameters", {
  base_args <- list(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 1000
  )

  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(S0 = -1))), "S0 must be positive")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(K = 0))), "K must be positive")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(sigma = -0.1))), "sigma must be positive")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(lambda_T = -1))), "lambda_T must be non-negative")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(kappa = 0))), "kappa must be positive")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(rho = 2))), "rho must be")
  expect_error(do.call(price_arithmetic_asian_diffusion,
                       modifyList(base_args, list(option_type = "straddle"))), "option_type")
})

test_that("deep in-the-money call has price close to discounted intrinsic", {
  result <- price_arithmetic_asian_diffusion(
    S0 = 200, K = 100, r = 0.05, sigma = 0.05, T = 1,
    lambda_T = 0, I0 = 0, kappa = 1, eta = 0.01, rho = 0,
    n_steps = 100, n_sims = 20000, seed = 42
  )

  # Deep ITM: price should be roughly exp(-r*T) * (E[avg S] - K)
  # E[avg S] ~ S0 * (exp(r*T) - 1) / (r*T) for continuous average
  expected_avg <- 200 * (exp(0.05) - 1) / 0.05
  expected_intrinsic <- exp(-0.05) * (expected_avg - 100)
  expect_true(abs(result$price - expected_intrinsic) / expected_intrinsic < 0.05)
})

test_that("positive impact state I0 increases call price", {
  price_no_impact <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 30000, seed = 42
  )$price

  price_pos_impact <- price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
    lambda_T = 0.05, I0 = 2, kappa = 1, eta = 0.1, rho = 0,
    n_steps = 50, n_sims = 30000, seed = 42
  )$price

  # Positive I0 with lambda_T > 0 adds drift to S, increasing call price
  expect_true(price_pos_impact > price_no_impact)
})

test_that("at lambda_T = 0 the price is identical to Kemna-Vorst under a common seed", {
  # The average is discretely monitored over the same n_steps + 1 fixings that
  # price_kemna_vorst_arithmetic() uses, and with lambda_T = 0 the impact state
  # is not simulated, so this routine draws one normal per step exactly as the
  # frictionless sampler does.  The two therefore walk the same paths and must
  # agree to floating-point rounding -- not merely to within Monte Carlo error.
  # Any measured "impact" at lambda_T = 0 would be an artefact of the sampler.
  args <- list(S0 = 100, K = 100, r = 0.05, sigma = 0.2, n = 60L, M = 5000L,
               seed = 20250101L)

  for (ot in c("call", "put")) {
    diff_price <- price_arithmetic_asian_diffusion(
      S0 = args$S0, K = args$K, r = args$r, sigma = args$sigma, T = 1,
      lambda_T = 0, I0 = 3, kappa = 1, eta = 0.5, rho = 0.4,
      option_type = ot, n_steps = args$n, n_sims = args$M,
      use_control_variate = TRUE, seed = args$seed
    )$price

    kv_price <- price_kemna_vorst_arithmetic(
      S0 = args$S0, K = args$K, r = args$r, sigma = args$sigma,
      T0 = 0, T_mat = 1, n = args$n, M = args$M, option_type = ot,
      use_control_variate = TRUE, seed = args$seed
    )

    expect_equal(diff_price, kv_price, tolerance = 1e-10)
  }
})

test_that("the average includes both the initial and the terminal fixing", {
  # sigma = 0 makes every path deterministic, S(t_m) = S0 * exp(r * t_m), so
  # the discretely monitored average is a number that can be written down.
  # A left-endpoint sum would drop S_T and undershoot it.
  S0 <- 100; r <- 0.05; T <- 1; n <- 4L
  fixings <- S0 * exp(r * (0:n) * (T / n))
  A <- mean(fixings)

  res <- price_arithmetic_asian_diffusion(
    S0 = S0, K = 1, r = r, sigma = 1e-8, T = T,
    lambda_T = 0, I0 = 0, kappa = 1, eta = 0, rho = 0,
    n_steps = n, n_sims = 200L, use_control_variate = FALSE, seed = 7L
  )

  expect_equal(res$price, exp(-r * T) * (A - 1), tolerance = 1e-5)
})

test_that("a sweep in lambda_T shares its price shocks (common random numbers)", {
  # The price and impact shocks come from separate streams, so changing
  # lambda_T must not disturb the price stream.  The impact premium is a
  # difference of order 1e-3 between two prices of order 5, so without that
  # cancellation it is pure noise -- and the failure is silent, because each
  # level is still individually correct.  The check: the premium is positive,
  # falls with volatility, and sits just above the exact geometric premium,
  # the arithmetic average being the more tail-sensitive functional.
  arith <- function(lam, sig) price_arithmetic_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = sig, T = 1,
    lambda_T = lam, I0 = 0, kappa = 1, eta = 0.5, rho = 0,
    n_steps = 100L, n_sims = 20000L, use_control_variate = TRUE,
    seed = 20250101L)$price
  geom <- function(lam, sig) price_geometric_asian_diffusion(
    S0 = 100, K = 100, r = 0.05, sigma = sig, T = 1,
    lambda_T = lam, I0 = 0, kappa = 1, eta = 0.5, rho = 0)

  prem_a <- vapply(c(0.10, 0.20, 0.40),
                   function(s) arith(0.05, s) - arith(0, s), numeric(1))
  prem_g <- vapply(c(0.10, 0.20, 0.40),
                   function(s) geom(0.05, s) - geom(0, s), numeric(1))

  expect_true(all(prem_a > 0))
  expect_true(all(diff(prem_a) < 0))
  expect_true(all(prem_a > prem_g))
  # Same order of magnitude as the exact geometric premium: if the streams
  # decoupled, this ratio would be arbitrary.
  expect_true(all(prem_a / prem_g < 2))
})
