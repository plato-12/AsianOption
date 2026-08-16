# Validation, S3 structure and reproducibility (design.md Section 7, point 10).

base_args <- list(
  S0 = 100, K = 100, T = 1, N = 6L, sigma = 0.2, r_cont = 0.05,
  n_I = 5L, n_Q = 7L, n_J = 7L, n_R = 11L, n_controls = 5L
)
quick <- function(...) do.call(price_geometric_asian_indiff,
                               utils::modifyList(base_args, list(...)))

valid_validator_args <- list(
  S0 = 100, K = 100, T = 1, N = 10, sigma = 0.2, r_cont = 0.05, mu = 0.05,
  lambda_I = 0, kappa_I = 1, eta = 0.5, rho = 0,
  lambda_bar_T = 0.05, lambda_bar_P = 0.025, kappa_J = 1,
  k_A = 0.05, k_B = 0.05, psi_cost = 1,
  gamma = 0.05, Gamma_Q = 1, ell_1 = 0,
  Q_bar = 2, nu_bar = 4, eps_exec = 1e-4, phi_cap = NULL, n_options = 1,
  I0 = 0, Q0 = 0, J0 = 0, control_set = c(-1, 0, 1),
  n_logS = NULL, n_I = 21, n_Q = 21, n_J = 15, n_R = 121, accum_sd = 5,
  monitoring = "continuous", n_fixings = NULL
)
.validator <- AsianOption:::validate_indiff_inputs
bad <- function(...) {
  do.call(.validator, utils::modifyList(valid_validator_args, list(...)))
}

test_that("the validator accepts a well-formed parameter set", {
  expect_silent(do.call(.validator, valid_validator_args))
})

test_that("the validator rejects out-of-range contract parameters", {
  expect_error(bad(S0 = -1), "S0 must be positive")
  expect_error(bad(K = 0), "K must be positive")
  expect_error(bad(T = 0), "T \\(maturity\\) must be positive")
  expect_error(bad(N = 0), "N must be a positive integer")
  expect_error(bad(N = 2.5), "N must be a positive integer")
  expect_error(bad(sigma = 0), "sigma must be positive")
})

test_that("the validator rejects out-of-range model parameters", {
  expect_error(bad(rho = 1.5), "rho must be in \\[-1, 1\\]")
  expect_error(bad(rho = -1.5), "rho must be in \\[-1, 1\\]")
  expect_error(bad(gamma = 0), "gamma \\(risk aversion\\) must be positive")
  expect_error(bad(gamma = -1), "gamma \\(risk aversion\\) must be positive")
  expect_error(bad(kappa_I = -1), "kappa_I must be non-negative")
  expect_error(bad(kappa_J = -1), "kappa_J must be non-negative")
  expect_error(bad(lambda_bar_T = -1), "lambda_bar_T must be non-negative")
  expect_error(bad(lambda_bar_P = -1), "lambda_bar_P must be non-negative")
  expect_error(bad(k_A = -1), "k_A must be non-negative")
  expect_error(bad(k_B = -1), "k_B must be non-negative")
  expect_error(bad(Gamma_Q = -1), "Gamma_Q must be non-negative")
  expect_error(bad(ell_1 = -1), "ell_1 must be non-negative")
  expect_error(bad(eps_exec = -1), "eps_exec must be non-negative")
  expect_error(bad(psi_cost = 0), "psi_cost must be in \\(0, 2\\]")
  expect_error(bad(psi_cost = 3), "psi_cost must be in \\(0, 2\\]")
  expect_warning(bad(psi_cost = 1.5), "outside the range")
})

test_that("the validator enforces the dealer's admissibility conditions", {
  expect_error(bad(Q_bar = 0), "Q_bar must be positive")
  expect_error(bad(nu_bar = -1), "nu_bar must be non-negative")
  expect_error(bad(n_options = 0), "n_options must be positive")
  expect_error(bad(Q0 = 5), "abs\\(Q0\\) = 5.0000 must not exceed Q_bar")
  expect_error(bad(control_set = c(-1, 1)), "control_set must contain 0")
  expect_error(bad(phi_cap = 0), "phi_cap must be a positive number or NULL")
  expect_error(bad(phi_cap = -3), "phi_cap must be a positive number or NULL")
  # Initial execution price s + lambda_bar_P q + lambda_bar_T j must clear
  # eps_exec, the floor note_v2 (8) puts on the admissible set.
  expect_error(bad(J0 = -1e5, lambda_bar_T = 1),
               "Initial execution price .* must be at least eps_exec")
  expect_error(bad(eps_exec = 200),
               "Initial execution price .* must be at least eps_exec")
})

test_that("the validator enforces the discrete-monitoring fixing schedule", {
  expect_error(bad(monitoring = "discrete"),
               "n_fixings is required when monitoring")
  expect_error(bad(monitoring = "discrete", n_fixings = 0),
               "n_fixings must be a positive integer")
  expect_error(bad(monitoring = "discrete", n_fixings = 2.5),
               "n_fixings must be a positive integer")
  expect_error(bad(monitoring = "discrete", n_fixings = 20, N = 10),
               "n_fixings = 20 must not exceed N = 10")
  # Fixings must land on time-grid nodes rather than be snapped to them.
  expect_error(bad(monitoring = "discrete", n_fixings = 3, N = 10),
               "n_fixings = 3 must divide N = 10 exactly")
  expect_silent(bad(monitoring = "discrete", n_fixings = 5, N = 10))
})

test_that("the validator enforces minimum grid sizes", {
  expect_error(bad(n_I = 4), "n_I must be an integer of at least 5")
  expect_error(bad(n_Q = 3), "n_Q must be an integer of at least 5")
  expect_error(bad(n_J = 1), "n_J must be an integer of at least 5")
  expect_error(bad(n_R = 4), "n_R must be an integer of at least 5")
  expect_error(bad(n_logS = 4), "n_logS must be an integer of at least 5")
  expect_error(bad(accum_sd = 0), "accum_sd must be positive")
  expect_error(bad(accum_center = NA), "accum_center must be TRUE or FALSE")
  expect_error(bad(accum_center = 1), "accum_center must be TRUE or FALSE")
})

test_that("the validator warns about unstable Euler updates", {
  expect_warning(bad(kappa_I = 20, N = 10), "kappa_I \\* dt = 2.0000 exceeds 1")
  expect_warning(bad(kappa_J = 20, N = 10), "kappa_J \\* dt = 2.0000 exceeds 1")
})

test_that("the validator warns when risk aversion dominates the grid (F1)", {
  expect_warning(bad(gamma = 1, Q_bar = 2),
                 "gamma \\* Q_bar \\* S0 = 200.0 is large")
  # A payoff cap is the documented remedy, so it silences the warning.
  expect_silent(bad(gamma = 1, Q_bar = 2, phi_cap = 20))
})

test_that("the control set is symmetric and contains an exact zero", {
  cs <- AsianOption:::.indiff_control_set(5, 15L)
  expect_length(cs, 15L)
  expect_true(any(cs == 0))
  expect_equal(cs, -rev(cs))
  expect_equal(max(cs), 5)
  expect_equal(min(cs), -5)
  expect_warning(cs2 <- AsianOption:::.indiff_control_set(4, 8L),
                 "n_controls must be odd")
  expect_length(cs2, 9L)
  expect_true(any(cs2 == 0))
  expect_error(AsianOption:::.indiff_control_set(0, 9L),
               "nu_bar must be positive")
  expect_error(AsianOption:::.indiff_control_set(4, 2L),
               "n_controls must be an integer of at least 3")
})

test_that("time-varying mu and eta are accepted as vectors and functions", {
  a <- quick(eta = 0.4)
  b <- quick(eta = rep(0.4, 6L))
  d <- quick(eta = function(t) rep(0.4, length(t)))
  expect_equal(a$ask_price, b$ask_price)
  expect_equal(a$ask_price, d$ask_price)

  ramp <- quick(eta = function(t) 0.2 + 0.4 * t)
  expect_true(is.finite(ramp$ask_price))
  expect_error(quick(eta = c(0.1, 0.2)),
               "eta must be a scalar, a vector of length N, or a function")
  expect_error(quick(eta = -0.5), "eta must be a non-negative")

  m <- quick(mu = function(t) 0.05 + 0 * t)
  expect_equal(m$ask_price, quick(mu = 0.05)$ask_price)
})

test_that("the returned object has the documented S3 structure", {
  res <- quick(store_policy = TRUE)
  expect_s3_class(res, "indiff_asian")
  for (nm in c("ask_price", "bid_price", "mid_price", "spread",
               "v0", "v_plus", "v_minus", "asian_type", "option_type",
               "diagnostics", "params", "grid_sizes")) {
    expect_true(nm %in% names(res), info = nm)
  }
  expect_identical(res$asian_type, "geometric")
  expect_identical(res$option_type, "call")

  expect_length(res$optimal_nu_seller, 6L)
  expect_length(res$Q_path_seller, 7L)
  expect_length(res$J_path_seller, 7L)
  expect_length(res$S_path, 7L)
  expect_equal(res$optimal_volumes_seller, res$optimal_nu_seller * (1 / 6))
  # The simulated inventory must respect its own bound and start at Q0.
  expect_equal(res$Q_path_seller[1], 0)
  expect_true(all(abs(res$Q_path_seller) <= res$params$Q_bar + 1e-9))
  expect_true(all(abs(res$optimal_nu_seller) <= res$params$nu_bar + 1e-9))

  d <- res$diagnostics
  expect_true(all(c("runtime_sec", "clamp_fraction", "grid_aligned",
                    "collapsed_I", "parallel_backend", "n_threads",
                    "grids") %in% names(d)))
  expect_true(is.character(d$parallel_backend))
  expect_true(d$parallel_backend %in% c("tbb", "tinythread", "serial"))
  expect_true(is.numeric(d$n_threads) && d$n_threads >= 1)
})

test_that("the arithmetic wrapper reports its own type", {
  res <- do.call(price_arithmetic_asian_indiff, base_args)
  expect_s3_class(res, "indiff_asian")
  expect_identical(res$asian_type, "arithmetic")
})

test_that("n_Z aliases n_R in the geometric interface", {
  a <- quick(n_R = 21L)
  b <- quick(n_R = 11L, n_Z = 21L)
  expect_equal(a$ask_price, b$ask_price)
  expect_equal(a$grid_sizes$n_R, b$grid_sizes$n_R)
})

test_that("print, summary and plot methods work", {
  res <- quick(store_policy = TRUE)
  expect_output(print(res), "Utility-Indifference Pricing")
  expect_output(print(res), "Bid price")
  expect_output(summary(res), "Diagnostics")
  expect_output(summary(res), "Log-price grid")
  expect_invisible(print(res))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_invisible(plot(res))
  expect_invisible(plot(res, which = "nu"))

  no_policy <- quick(store_policy = FALSE)
  expect_error(plot(no_policy), "store_policy = TRUE")
})

test_that("results are reproducible and thread-count independent", {
  a <- quick(n_threads = 1)
  b <- quick(n_threads = 4)
  d <- quick(n_threads = 1)
  expect_identical(a$ask_price, d$ask_price)
  expect_equal(a$ask_price, b$ask_price)
  expect_equal(a$bid_price, b$bid_price)
})

test_that("the reference engine agrees with the cached engine end to end", {
  a <- quick(engine_mode = "cached")
  b <- quick(engine_mode = "reference")
  expect_equal(a$ask_price, b$ask_price, tolerance = 1e-10)
  expect_equal(a$bid_price, b$bid_price, tolerance = 1e-10)
})

test_that("validate = FALSE skips validation", {
  expect_silent(quick(kappa_J = 100, validate = FALSE))
})

test_that("a user-supplied control set is respected", {
  res <- quick(control_set = c(-2, -1, 0, 1, 2))
  expect_equal(res$grid_sizes$n_controls, 5L)
  expect_equal(res$params$nu_bar, 2)
  # A dealer forbidden to trade is a legitimate baseline.
  nt <- quick(control_set = 0)
  expect_equal(nt$grid_sizes$n_controls, 1L)
  expect_true(is.finite(nt$spread))
})

test_that("the legacy HJB functions warn once about deprecation", {
  # The notice fires at most once per session, so only assert that whichever
  # call comes first in this file's run is capable of raising it.
  expect_true(is.function(price_arithmetic_asian_hjb))
  warn_fn <- AsianOption:::.warn_hjb_deprecated
  state <- AsianOption:::.hjb_deprecation_state
  expect_true(is.function(warn_fn))

  state$warned <- NULL
  expect_warning(warn_fn("price_x"), "deprecated")
  expect_silent(warn_fn("price_x"))
  state$warned <- NULL
})
