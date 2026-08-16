# Unit tests of the C++ engine internals (design.md Section 7, points 1, 2, 6
# and the M2 acceptance criterion).

test_that("branch probabilities have the right moments", {
  for (rho in c(-1, -0.7, -0.25, 0, 0.25, 0.7, 1)) {
    p <- indiff_branch_probs_cpp(rho)
    expect_length(p, 4L)
    expect_true(all(p >= 0))
    expect_equal(sum(p), 1)

    # Branch order is (+,+), (+,-), (-,+), (-,-).
    xi   <- c(1, 1, -1, -1)
    zeta <- c(1, -1, 1, -1)
    expect_equal(sum(p * xi), 0)
    expect_equal(sum(p * zeta), 0)
    expect_equal(sum(p * xi * zeta), rho)
    expect_equal(sum(p * xi^2), 1)
    expect_equal(sum(p * zeta^2), 1)
  }
})

test_that("branch probabilities reject |rho| > 1", {
  expect_error(indiff_branch_probs_cpp(1.0001), "rho must be in")
  expect_error(indiff_branch_probs_cpp(-1.0001), "rho must be in")
})

test_that("cash-discount identity beta_m delta_r = beta_{m+1}(e^{r dt}-1)/r", {
  T <- 1.3
  N <- 7L
  for (r in c(0, 1e-8, 0.05, 0.2)) {
    h <- indiff_discount_helpers_cpp(r, T, N)
    dt <- h$dt
    expect_equal(dt, T / N)

    # delta_r is the integral of exp(-r u) over [0, dt].  The reference is
    # written with expm1 because the algebraically equivalent
    # (1 - exp(-r dt))/r loses all but a handful of digits once r dt is tiny.
    ref_delta <- if (abs(r) < 1e-12) dt else -expm1(-r * dt) / r
    expect_equal(h$delta_r, ref_delta, tolerance = 1e-14)

    expect_equal(h$beta, exp(r * (T - (0:N) * dt)))

    rhs <- if (abs(r) < 1e-12) {
      h$beta[2:(N + 1)] * dt
    } else {
      h$beta[2:(N + 1)] * expm1(r * dt) / r
    }
    expect_equal(h$beta_delta, rhs, tolerance = 1e-14)
  }
})

test_that("delta_r stays accurate where the naive formula cancels", {
  T <- 1.3; N <- 7L; dt <- T / N
  # For ordinary rates the naive expression is fine and must agree.
  for (r in c(0.05, 0.2)) {
    h <- indiff_discount_helpers_cpp(r, T, N)
    expect_equal(h$delta_r, (1 - exp(-r * dt)) / r, tolerance = 1e-10)
  }
  # For r dt ~ 1e-9 it is not: delta_r = dt (1 - r dt/2 + O((r dt)^2)), and the
  # engine must track that series rather than the cancelling difference.
  r <- 1e-8
  h <- indiff_discount_helpers_cpp(r, T, N)
  expect_equal(h$delta_r, dt * (1 - r * dt / 2 + (r * dt)^2 / 6),
               tolerance = 1e-15)
})

test_that("r -> 0 limit of the discount helpers is continuous", {
  T <- 1; N <- 10L
  h0 <- indiff_discount_helpers_cpp(0, T, N)
  he <- indiff_discount_helpers_cpp(1e-10, T, N)
  expect_equal(h0$delta_r, he$delta_r, tolerance = 1e-12)
  expect_equal(h0$beta_delta, he$beta_delta, tolerance = 1e-10)
})

test_that("terminal payoff matches the closed form and honours the cap", {
  S0 <- 100; K <- 100; T <- 1
  # Arithmetic: average is S0 * a / T.
  expect_equal(indiff_payoff_cpp(1.2, S0, K, T, 0L, 0L, -1), 20)
  expect_equal(indiff_payoff_cpp(0.8, S0, K, T, 0L, 0L, -1), 0)
  expect_equal(indiff_payoff_cpp(0.8, S0, K, T, 0L, 1L, -1), 20)   # put
  expect_equal(indiff_payoff_cpp(1.2, S0, K, T, 0L, 0L, 5), 5)     # capped

  # Geometric: average is S0 * exp(a / T).
  expect_equal(indiff_payoff_cpp(0.1, S0, K, T, 1L, 0L, -1),
               S0 * exp(0.1) - K)
  expect_equal(indiff_payoff_cpp(-0.1, S0, K, T, 1L, 0L, -1), 0)
  expect_equal(indiff_payoff_cpp(-0.1, S0, K, T, 1L, 1L, -1),
               K - S0 * exp(-0.1))
})

# --- engine-level checks -----------------------------------------------------

tiny <- function(theta, mode = 0L, lambda_I = 0, n_I = 5L, n_threads = 0L,
                 gamma = 0.05, rho = 0.3, ell_1 = 0, eps_exec = 0,
                 monitor_mode = 0L, fix_w = rep(0, 5), ctrl = c(-2, -1, 0, 1, 2),
                 Q_bar = 3, lambda_bar_P = 0.025, lambda_bar_T = 0.05) {
  indiff_bellman_engine_cpp(
    S0 = 100, K = 100, T_mat = 1, N = 5L,
    mu_vec = rep(0.05, 5), sigma = 0.2, r_cont = 0.05,
    lambda_I = lambda_I, kappa_I = 1, eta_vec = rep(0.3, 5), rho = rho,
    lambda_bar_T = lambda_bar_T, lambda_bar_P = lambda_bar_P, kappa_J = 1,
    k_A = 0.4, k_B = 0.4, psi_cost = 1,
    gamma_ra = gamma, Gamma_Q = 1, ell_1 = ell_1,
    Q_bar = Q_bar, eps_exec = eps_exec, phi_cap = -1, n_opt = 1,
    I0 = 0, Q0 = 0, J0 = 0, control_set = ctrl,
    n_logS = 0L, n_I = n_I, n_Q = 7L, n_J = 7L, n_R = 11L,
    asian_type = 1L, option_type = 0L, theta = theta,
    accum_rule = 1L, monitor_mode = monitor_mode, fix_w = fix_w,
    accum_sd = 5, grid_drift = 1L, accum_center = 1L,
    store_policy = FALSE, n_threads = n_threads, engine_mode = mode,
    verbose = FALSE
  )
}

test_that("the cached path reproduces the reference path (M2 acceptance)", {
  for (theta in c(-1L, 0L, 1L)) {
    a <- tiny(theta, mode = 1L)
    b <- tiny(theta, mode = 0L)
    # The two paths perform the same operations in the same order, so they
    # agree to a few ulps; exact bit-identity is not guaranteed because the
    # compiler may contract multiply-adds differently in the two loop shapes.
    expect_equal(a$value, b$value, tolerance = 1e-12)
    expect_equal(a$logS_grid, b$logS_grid)
    expect_equal(a$R_grid, b$R_grid)
  }
})

test_that("the cached path reproduces the reference path with lambda_I != 0", {
  for (theta in c(-1L, 1L)) {
    a <- tiny(theta, mode = 1L, lambda_I = 0.3)
    b <- tiny(theta, mode = 0L, lambda_I = 0.3)
    expect_equal(a$value, b$value, tolerance = 1e-12)
  }
})

test_that("the two paths agree under each note_v2 feature", {
  # The M2 acceptance criterion, re-run over the features note_v2 adds: the
  # linear terminal charge, the forward-looking execution-price floor, and
  # discrete monitoring.  A change to one code path that is not mirrored in the
  # other shows up here.
  variants <- list(
    ell_1     = list(ell_1 = 0.5),
    eps_bind  = list(eps_exec = 101, lambda_bar_P = 1.5, Q_bar = 3),
    discrete  = list(monitor_mode = 1L,
                     fix_w = c(0, 1 / 2, 0, 0, 1 / 2)),
    combined  = list(ell_1 = 0.3, eps_exec = 99, monitor_mode = 1L,
                     fix_w = rep(1 / 5, 5))
  )
  for (nm in names(variants)) {
    for (theta in c(-1L, 0L, 1L)) {
      args <- c(list(theta = theta), variants[[nm]])
      a <- do.call(tiny, c(args, list(mode = 1L)))
      b <- do.call(tiny, c(args, list(mode = 0L)))
      expect_equal(a$value, b$value, tolerance = 1e-12,
                   info = paste(nm, "theta =", theta))
      expect_equal(a$n_infeasible, b$n_infeasible, info = nm)
    }
  }
})

test_that("the linear terminal charge is exactly ell_1 * |Q_T|", {
  # A dealer who cannot trade holds Q0 = 0 to maturity, so ell_1 must not bite.
  frozen <- function(ell_1) {
    tiny(0L, ctrl = 0, ell_1 = ell_1)$value
  }
  expect_equal(frozen(0.7), frozen(0), tolerance = 1e-12)

  # Sitting on a non-zero inventory it must, and by exactly ell_1 * |q|: the
  # charge is deterministic, so the certainty equivalent shifts one for one.
  # theta = 0 with no trading makes the whole problem deterministic in q.
  with_q <- function(ell_1, Q0) {
    indiff_bellman_engine_cpp(
      S0 = 100, K = 100, T_mat = 1, N = 3L,
      mu_vec = rep(0, 3), sigma = 0.2, r_cont = 0,
      lambda_I = 0, kappa_I = 1, eta_vec = rep(0, 3), rho = 0,
      lambda_bar_T = 0, lambda_bar_P = 0, kappa_J = 1,
      k_A = 0, k_B = 0, psi_cost = 1,
      gamma_ra = 1e-8, Gamma_Q = 0, ell_1 = ell_1,
      Q_bar = 3, eps_exec = 0, phi_cap = -1, n_opt = 1,
      I0 = 0, Q0 = Q0, J0 = 0, control_set = 0,
      n_logS = 0L, n_I = 5L, n_Q = 7L, n_J = 5L, n_R = 11L,
      asian_type = 1L, option_type = 0L, theta = 0L,
      accum_rule = 1L, monitor_mode = 0L, fix_w = rep(0, 3),
      accum_sd = 5, grid_drift = 1L, accum_center = 1L, store_policy = FALSE,
      n_threads = 1L, engine_mode = 0L, verbose = FALSE
    )$value
  }
  expect_equal(with_q(0, 2) - with_q(0.25, 2), 0.25 * 2, tolerance = 1e-8)
  expect_equal(with_q(0, -2) - with_q(0.25, -2), 0.25 * 2, tolerance = 1e-8)
})

test_that("the execution-price floor removes controls and is reported", {
  # lambda_bar_P = 1.5 makes the execution price 100 + 1.5q, so a floor just
  # above 100 rules out every q < 0 node and the trades that would reach one.
  loose <- tiny(1L, eps_exec = 0,   lambda_bar_P = 1.5)
  tight <- tiny(1L, eps_exec = 101, lambda_bar_P = 1.5)

  expect_equal(loose$n_infeasible, 0)
  expect_gt(tight$n_infeasible, 0)
  expect_false(isTRUE(all.equal(loose$value, tight$value)))

  # The count is a property of the problem, not of the schedule.
  expect_equal(tiny(1L, eps_exec = 101, lambda_bar_P = 1.5,
                    n_threads = 1L)$n_infeasible,
               tight$n_infeasible)
})

test_that("results do not depend on the thread count", {
  base <- tiny(1L, n_threads = 1L)
  for (thr in c(2L, 4L)) {
    expect_equal(tiny(1L, n_threads = thr)$value, base$value)
  }
})

test_that("collapsing the impact dimension when lambda_I = 0 is exact", {
  # lambda_I = 0 triggers the collapse; a tiny non-zero value does not, so the
  # two runs solve the same problem on very different grids.
  a <- tiny(1L, lambda_I = 0,     n_I = 21L)
  b <- tiny(1L, lambda_I = 1e-14, n_I = 21L)
  expect_equal(a$n_I, 2L)
  expect_equal(b$n_I, 21L)
  expect_equal(a$value, b$value, tolerance = 1e-10)
})

test_that("the log-price grid is aligned and reports its diagnostics", {
  res <- tiny(0L)
  expect_true(res$grid_aligned)
  expect_equal(res$shock_cells, 1L)
  expect_true(res$initial_state_interior)
  # One shock must be exactly one grid cell.
  dx <- diff(res$logS_grid)[1]
  expect_equal(dx, 0.2 * sqrt(1 / 5), tolerance = 1e-12)
})

test_that("a = 0 and the initial state stay on the grid", {
  res <- tiny(0L)
  expect_true(any(abs(res$R_grid) < 1e-12))       # geometric: 0 is a node
  expect_true(any(abs(res$Q_grid) < 1e-12))
  expect_true(any(abs(res$J_grid) < 1e-12))
  expect_equal(res$n_logS %% 2, 1)                # log S0 is the centre node
})

test_that("the engine rejects malformed arguments", {
  bad <- function(...) {
    args <- list(
      S0 = 100, K = 100, T_mat = 1, N = 5L, mu_vec = rep(0.05, 5),
      sigma = 0.2, r_cont = 0.05, lambda_I = 0, kappa_I = 1,
      eta_vec = rep(0.3, 5), rho = 0, lambda_bar_T = 0, lambda_bar_P = 0,
      kappa_J = 1, k_A = 0, k_B = 0, psi_cost = 1, gamma_ra = 0.1,
      Gamma_Q = 0, ell_1 = 0, Q_bar = 3, eps_exec = 0, phi_cap = -1,
      n_opt = 1, I0 = 0, Q0 = 0, J0 = 0, control_set = c(-1, 0, 1),
      n_logS = 0L, n_I = 5L, n_Q = 5L, n_J = 5L, n_R = 11L,
      asian_type = 1L, option_type = 0L, theta = 0L, accum_rule = 1L,
      monitor_mode = 0L, fix_w = rep(0, 5), accum_sd = 5, grid_drift = 1L,
      accum_center = 1L,
      store_policy = FALSE, n_threads = 0L, engine_mode = 0L, verbose = FALSE
    )
    do.call(indiff_bellman_engine_cpp, utils::modifyList(args, list(...)))
  }

  expect_error(bad(mu_vec = rep(0.05, 3)), "mu_vec must have length N")
  expect_error(bad(control_set = c(-1, 1)), "control_set must contain 0")
  expect_error(bad(fix_w = rep(0, 3)), "fix_w must have length N")
})

test_that("with no option and no inventory the baseline value is exactly zero", {
  # With Q0 = J0 = 0 and costly, risky trading, standing still is optimal and
  # terminal wealth is identically zero, so v^0 = 0 to the last bit.
  expect_equal(tiny(0L)$value, 0)
})

# --- accumulator moments and the mean-tracking accumulator grid -------------

test_that("accumulator moments match the continuous Turnbull-Wakeman form", {
  # a_T = int_0^T (S_u/S0) du under dS/S = mu dt + sigma dW has
  #   E[a_T]   = (e^{mu T} - 1)/mu
  #   E[a_T^2] = (2/b)[(e^{c T} - 1)/c - (e^{mu T} - 1)/mu],
  # with b = mu + sigma^2 and c = 2 mu + sigma^2.  The engine's discrete
  # trapezoidal moments must converge to these as N grows.
  T <- 1; mu <- 0.05
  for (sigma in c(0.10, 0.20, 0.40)) {
    b <- mu + sigma^2
    cc <- 2 * mu + sigma^2
    m1 <- (exp(mu * T) - 1) / mu
    m2 <- (2 / b) * ((exp(cc * T) - 1) / cc - (exp(mu * T) - 1) / mu)
    sd_cont <- sqrt(m2 - m1^2)

    N <- 200L
    got <- indiff_accum_moments_cpp(N = N, dt = T / N, mu = rep(mu, N),
                                    sigma = sigma, accum_rule = 1L,
                                    monitor_mode = 0L, fix_w = rep(0, N))
    expect_equal(got$mean[N + 1L], m1, tolerance = 1e-5)
    expect_equal(got$sd, sd_cont, tolerance = 1e-4)
  }
})

test_that("the accumulator mean path starts at zero and uses the engine's rule", {
  N <- 8L; T <- 1; dt <- T / N; mu <- 0.05
  for (rule in c(0L, 1L)) {
    got <- indiff_accum_moments_cpp(N = N, dt = dt, mu = rep(mu, N),
                                    sigma = 0.2, accum_rule = rule,
                                    monitor_mode = 0L, fix_w = rep(0, N))
    # a_0 = 0 exactly: the engine starts the accumulator there, so the grid
    # origin must too.
    expect_equal(got$mean[1], 0)
    expect_length(got$mean, N + 1L)
    expect_true(all(diff(got$mean) > 0))

    # The mean path is the recursion accum_next() runs, not a partial sum of
    # the terminal weights: under the trapezoid rule the first step advances
    # by dt/2 * (E X_0 + E X_1), not by the terminal weight dt.
    EX <- exp(mu * (0:N) * dt)
    step1 <- if (rule == 0L) EX[1] * dt else 0.5 * (EX[1] + EX[2]) * dt
    expect_equal(got$mean[2], step1)

    # At maturity the mean path does agree with the terminal weights: left
    # endpoint puts dt on nodes 0..N-1, trapezoid puts dt/2 on both ends.
    w <- if (rule == 0L) c(rep(dt, N), 0) else c(dt / 2, rep(dt, N - 1L), dt / 2)
    expect_equal(got$weights, w)
    expect_equal(got$mean[N + 1L], sum(w * EX))
  }
})

test_that("accum_center narrows the arithmetic accumulator grid, not the geometric", {
  args <- list(S0 = 100, K = 100, T = 1, N = 10, sigma = 0.10, r_cont = 0.05,
               lambda_I = 0, n_I = 5, n_Q = 7, n_J = 5, n_R = 41,
               n_controls = 5, n_threads = 1)
  ar_on  <- do.call(price_arithmetic_asian_indiff, c(args, accum_center = TRUE))
  ar_off <- do.call(price_arithmetic_asian_indiff, c(args, accum_center = FALSE))
  ge_on  <- do.call(price_geometric_asian_indiff,  c(args, accum_center = TRUE))
  ge_off <- do.call(price_geometric_asian_indiff,  c(args, accum_center = FALSE))

  R_on  <- ar_on$diagnostics$grids$R
  R_off <- ar_off$diagnostics$grids$R
  sh    <- ar_on$diagnostics$grids$R_shift

  # a_0 = 0 sits on the time-0 origin, and the origin ends at E[a_T] > 0.
  expect_equal(sh[1], 0)
  expect_gt(sh[length(sh)], 0)
  expect_equal(R_on[1], -R_on[length(R_on)])      # offsets about the mean path

  # The whole point: a finer cell at the same n_R.
  expect_lt(diff(R_on)[1], diff(R_off)[1])

  # The geometric payoff is untouched -- bit-identical, not merely close.
  expect_identical(ge_on$diagnostics$grids$R, ge_off$diagnostics$grids$R)
  expect_true(all(ge_on$diagnostics$grids$R_shift == 0))
  expect_identical(ge_on$bid_price, ge_off$bid_price)
  expect_identical(ge_on$ask_price, ge_off$ask_price)
})

test_that("the mean-tracking accumulator restores the spread ordering at low sigma", {
  # The arithmetic average is the more variable of the two, so hedging it must
  # be the wider-spread side.  On the pre-0.4.1 grid the arithmetic cell was
  # about twice the geometric one at sigma = 0.10 and the ordering inverted;
  # this is the regression test for that.
  args <- list(S0 = 100, K = 100, T = 1, N = 25, sigma = 0.10, r_cont = 0.05,
               lambda_I = 0, n_I = 5, n_Q = 11, n_J = 5, n_R = 121,
               n_controls = 5, n_threads = 0)
  ar <- do.call(price_arithmetic_asian_indiff, c(args, accum_center = TRUE))
  ge <- do.call(price_geometric_asian_indiff,  c(args, accum_center = TRUE))
  expect_gt(ar$spread, ge$spread)
})
