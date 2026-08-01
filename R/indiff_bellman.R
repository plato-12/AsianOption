# Utility-indifference pricing for Asian options under price impact.
# The model solved here is specified in Section 2 of design.md; the C++ engine
# in src/indiff_bellman.cpp is the implementation of that specification.

# Expand a scalar, length-N vector, or function of t onto the left-endpoint
# time grid, as the exogenous-diffusion functions already do.
.indiff_time_vector <- function(x, T, N, name) {
  dt <- T / N
  t_grid <- seq(0, T - dt, length.out = N)
  if (is.function(x)) {
    v <- x(t_grid)
    if (length(v) != N) {
      stop(sprintf("%s(t) must return one value per time step", name))
    }
  } else if (length(x) == 1) {
    v <- rep(x, N)
  } else if (length(x) == N) {
    v <- as.numeric(x)
  } else {
    stop(sprintf("%s must be a scalar, a vector of length N, or a function",
                 name))
  }
  as.numeric(v)
}

# Symmetric control grid with an exact zero at the centre.  seq() would only
# reproduce the zero by luck, and the engine requires nu = 0 to be admissible.
.indiff_control_set <- function(nu_bar, n_controls) {
  if (!is.numeric(nu_bar) || length(nu_bar) != 1 || nu_bar <= 0) {
    stop("nu_bar must be positive")
  }
  if (!is.numeric(n_controls) || length(n_controls) != 1 ||
      n_controls != as.integer(n_controls) || n_controls < 3) {
    stop("n_controls must be an integer of at least 3")
  }
  if (n_controls %% 2 == 0) {
    n_controls <- n_controls + 1L
    warning(sprintf(
      "n_controls must be odd so that 0 is a control; using %d.", n_controls
    ))
  }
  half <- (n_controls - 1L) %/% 2L
  pos <- seq(nu_bar / half, nu_bar, length.out = half)
  c(-rev(pos), 0, pos)
}

# Thread count the engine will actually use, for reporting only.  Mirrors
# RcppParallel's own resolution order: an explicit n_threads wins, then
# RCPP_PARALLEL_NUM_THREADS (which is what setThreadOptions() sets), then the
# RcppParallel default.
.indiff_resolve_threads <- function(n_threads) {
  if (isTRUE(n_threads > 0)) return(as.integer(n_threads))
  env <- suppressWarnings(
    as.integer(Sys.getenv("RCPP_PARALLEL_NUM_THREADS", unset = ""))
  )
  if (!is.na(env) && env > 0) return(env)
  defaultNumThreads()
}

.indiff_core <- function(asian_type,
                         S0, K, T, N, mu, sigma, r_cont,
                         lambda_I, kappa_I, eta, rho,
                         lambda_bar_T, lambda_bar_P, kappa_J,
                         k_A, k_B, psi_cost,
                         gamma, Gamma_Q, Gamma_J,
                         Q_bar, nu_bar, phi_cap, n_options,
                         I0, Q0, J0,
                         control_set, n_controls,
                         n_logS, n_I, n_Q, n_J, n_R,
                         option_type, accum_rule, accum_sd, grid_drift,
                         store_policy, n_threads, engine_mode, verbose,
                         validate) {

  option_type <- match.arg(option_type, c("call", "put"))
  accum_rule  <- match.arg(accum_rule, c("trapezoid", "left"))
  engine_mode <- match.arg(engine_mode, c("cached", "reference"))

  if (is.null(control_set)) {
    control_set <- .indiff_control_set(nu_bar, n_controls)
  } else {
    control_set <- as.numeric(control_set)
    nu_bar <- max(abs(control_set))
  }

  if (validate) {
    validate_indiff_inputs(
      S0 = S0, K = K, T = T, N = N, sigma = sigma, r_cont = r_cont, mu = mu,
      lambda_I = lambda_I, kappa_I = kappa_I, eta = eta, rho = rho,
      lambda_bar_T = lambda_bar_T, lambda_bar_P = lambda_bar_P,
      kappa_J = kappa_J, k_A = k_A, k_B = k_B, psi_cost = psi_cost,
      gamma = gamma, Gamma_Q = Gamma_Q, Gamma_J = Gamma_J,
      Q_bar = Q_bar, nu_bar = nu_bar, phi_cap = phi_cap,
      n_options = n_options, I0 = I0, Q0 = Q0, J0 = J0,
      control_set = control_set,
      n_logS = n_logS, n_I = n_I, n_Q = n_Q, n_J = n_J, n_R = n_R,
      accum_sd = accum_sd
    )
  }

  N <- as.integer(N)
  mu_vec  <- .indiff_time_vector(mu, T, N, "mu")
  eta_vec <- .indiff_time_vector(eta, T, N, "eta")
  if (any(eta_vec < 0)) stop("eta must be non-negative for all t in [0, T]")

  run <- function(theta, store) {
    indiff_bellman_engine_cpp(
      S0 = S0, K = K, T_mat = T, N = N,
      mu_vec = mu_vec, sigma = sigma, r_cont = r_cont,
      lambda_I = lambda_I, kappa_I = kappa_I,
      eta_vec = eta_vec, rho = rho,
      lambda_bar_T = lambda_bar_T, lambda_bar_P = lambda_bar_P,
      kappa_J = kappa_J,
      k_A = k_A, k_B = k_B, psi_cost = psi_cost,
      gamma_ra = gamma, Gamma_Q = Gamma_Q, Gamma_J = Gamma_J,
      Q_bar = Q_bar,
      phi_cap = if (is.null(phi_cap)) -1 else phi_cap,
      n_opt = n_options,
      I0 = I0, Q0 = Q0, J0 = J0,
      control_set = control_set,
      n_logS = if (is.null(n_logS)) 0L else as.integer(n_logS),
      n_I = as.integer(n_I), n_Q = as.integer(n_Q),
      n_J = as.integer(n_J), n_R = as.integer(n_R),
      asian_type = if (asian_type == "arithmetic") 0L else 1L,
      option_type = if (option_type == "call") 0L else 1L,
      theta = as.integer(theta),
      accum_rule = if (accum_rule == "trapezoid") 1L else 0L,
      accum_sd = accum_sd,
      grid_drift = if (isTRUE(grid_drift)) 1L else 0L,
      store_policy = isTRUE(store),
      n_threads = as.integer(n_threads),
      engine_mode = if (engine_mode == "cached") 0L else 1L,
      verbose = isTRUE(verbose)
    )
  }

  t_start <- proc.time()[["elapsed"]]
  res0 <- run(0L, FALSE)              # baseline: no option
  resP <- run(1L, store_policy)       # theta = +1: dealer is short the option
  resM <- run(-1L, store_policy)      # theta = -1: dealer is long the option
  runtime <- proc.time()[["elapsed"]] - t_start

  beta0 <- exp(r_cont * T)
  v0 <- res0$value; vp <- resP$value; vm <- resM$value

  ask <- (v0 - vp) / beta0 / n_options
  bid <- (vm - v0) / beta0 / n_options

  # Flag F2 of design.md: bid <= ask is expected but unproven, so it is treated
  # as a numerical invariant rather than assumed.
  tol <- 1e-8 * max(1, abs(ask), abs(bid))
  if (bid > ask + tol) {
    warning(sprintf(
      paste0("bid (%.6f) exceeds ask (%.6f) by more than interpolation ",
             "tolerance; refine the grids."),
      bid, ask
    ))
  }
  if (ask < -tol || bid < -tol) {
    warning(sprintf(
      "Negative indifference price (bid = %.6f, ask = %.6f); refine the grids.",
      bid, ask
    ))
  }

  clamp_frac <- function(res) {
    tot <- res$n_bracket
    ifelse(tot > 0, res$n_clamped / pmax(tot, 1), 0)
  }

  result <- list(
    ask_price = ask,
    bid_price = bid,
    mid_price = 0.5 * (ask + bid),
    spread    = ask - bid,
    v0        = v0,
    v_plus    = vp,
    v_minus   = vm,
    asian_type  = asian_type,
    option_type = option_type,
    diagnostics = list(
      runtime_sec = runtime,
      clamp_fraction = list(baseline = clamp_frac(res0),
                            short    = clamp_frac(resP),
                            long     = clamp_frac(resM)),
      n_clamped = list(baseline = res0$n_clamped,
                       short    = resP$n_clamped,
                       long     = resM$n_clamped),
      initial_state_interior = res0$initial_state_interior,
      grid_aligned = res0$grid_aligned,
      shock_cells  = res0$shock_cells,
      collapsed_I  = res0$collapsed_I,
      margin  = res0$margin,
      I_bound = res0$I_bound,
      J_bound = res0$J_bound,
      parallel_backend = indiff_parallel_backend_cpp(),
      n_threads = .indiff_resolve_threads(n_threads),
      grids = list(logS = res0$logS_grid, logS_shift = res0$logS_shift,
                   I = res0$I_grid, Q = res0$Q_grid,
                   J = res0$J_grid, R = res0$R_grid)
    ),
    params = list(
      S0 = S0, K = K, T = T, N = N, sigma = sigma, r_cont = r_cont,
      mu = mu, lambda_I = lambda_I, kappa_I = kappa_I, eta = eta, rho = rho,
      lambda_bar_T = lambda_bar_T, lambda_bar_P = lambda_bar_P,
      kappa_J = kappa_J, k_A = k_A, k_B = k_B, psi_cost = psi_cost,
      gamma = gamma, Gamma_Q = Gamma_Q, Gamma_J = Gamma_J,
      Q_bar = Q_bar, nu_bar = nu_bar, phi_cap = phi_cap,
      n_options = n_options, I0 = I0, Q0 = Q0, J0 = J0,
      accum_rule = accum_rule, accum_sd = accum_sd, grid_drift = grid_drift,
      engine_mode = engine_mode
    ),
    grid_sizes = list(
      n_logS = res0$n_logS, n_I = res0$n_I, n_Q = as.integer(n_Q),
      n_J = as.integer(n_J), n_R = res0$n_R,
      n_controls = length(control_set),
      n_states = res0$n_logS * res0$n_I * n_Q * n_J * res0$n_R
    )
  )

  if (store_policy) {
    dt <- T / N
    result$optimal_nu_seller <- resP$nu_path
    result$optimal_nu_buyer  <- resM$nu_path
    result$optimal_volumes_seller <- resP$nu_path * dt
    result$optimal_volumes_buyer  <- resM$nu_path * dt
    result$Q_path_seller <- resP$Q_path
    result$Q_path_buyer  <- resM$Q_path
    result$J_path_seller <- resP$J_path
    result$J_path_buyer  <- resM$J_path
    result$S_path <- resP$S_path
    result$I_path <- resP$I_path
    result$A_path <- resP$A_path
  }

  class(result) <- c("indiff_asian", "list")
  result
}


#' Utility-Indifference Bid and Ask for an Arithmetic Asian Option
#'
#' Prices an arithmetic Asian option by exponential-utility indifference for a
#' CARA dealer who hedges in a market with price impact. Unlike the legacy
#' \code{\link{price_arithmetic_asian_hjb}} module, the dealer runs a genuine
#' self-financing cash account, cannot move the Asian fixing, and quotes are
#' reservation prices obtained from three value functions.
#'
#' @details
#' Write \eqn{\Delta t = T/N}, \eqn{\delta_r = (1 - e^{-r\Delta t})/r} and
#' \eqn{\beta_m = e^{r(T - t_m)}}. The state is \eqn{(s, i, q, j, a)}: price,
#' exogenous impact, dealer inventory, dealer transient impact, and the running
#' accumulator. With shocks \eqn{(\xi, \zeta) \in \{-1, +1\}^2} drawn with
#' probabilities \eqn{p_{\xi\zeta} = (1 + \rho\xi\zeta)/4},
#'
#' \deqn{s' = s \exp[(\mu + \lambda_I i - \sigma^2/2)\Delta t
#'                    + \sigma\sqrt{\Delta t}\,\xi]}
#' \deqn{i' = i - \kappa_I i \Delta t + \eta \sqrt{\Delta t}\,\zeta}
#' \deqn{q' = q + \nu \Delta t, \qquad j' = j + (-\kappa_J j + \nu)\Delta t}
#'
#' and executing at rate \eqn{\nu} costs
#' \deqn{g = (s + \bar\lambda_P q + \bar\lambda_T j)\nu
#'           + k_A (\nu^+)^{1+\psi} + k_B (\nu^-)^{1+\psi}.}
#'
#' For \eqn{\theta \in \{-1, 0, +1\}} the terminal value is
#' \deqn{v^\theta_N = q s - \tfrac{\Gamma_Q}{2}q^2 - \tfrac{\Gamma_J}{2}j^2
#'                    - \theta\, n\, \Phi(a),}
#' and the backward recursion maximises
#' \deqn{-\beta_m \delta_r g - \frac{1}{\gamma}\log \sum_{\xi\zeta}
#'        p_{\xi\zeta} \exp(-\gamma v^\theta_{m+1}(s', i', q', j', a'))}
#' over admissible \eqn{\nu}. The quotes are
#' \eqn{P_{ask} = (v^0_0 - v^+_0)/e^{rT}} and
#' \eqn{P_{bid} = (v^-_0 - v^0_0)/e^{rT}}, divided by \code{n_options}.
#'
#' The discrete, compact-grid model is the primitive. Under a continuous state
#' space the CARA utility of \eqn{q_T S_T} has infinite exponential moments, so
#' grid truncation is part of the model rather than an approximation to a
#' continuum limit; \code{phi_cap} is provided for the same reason.
#'
#' Note the change of meaning relative to the legacy module:
#' \code{lambda_bar_T} and \code{lambda_bar_P} are now the dealer's own
#' execution-impact coefficients, not coefficients in the drift of \eqn{S}. The
#' drift loading is \code{lambda_I}.
#'
#' @param S0 Initial stock price (positive).
#' @param K Strike price (positive).
#' @param T Time to maturity in years (positive).
#' @param N Number of time steps.
#' @param mu Drift of the price under the dealer's measure. Scalar, length-N
#'   vector, or function of t. Defaults to \code{r_cont}.
#' @param sigma Volatility (positive).
#' @param r_cont Continuously compounded risk-free rate.
#' @param lambda_I Loading of the exogenous impact state in the price drift.
#' @param kappa_I Mean-reversion rate of the exogenous impact state.
#' @param eta Impact noise amplitude. Scalar, length-N vector, or function of t.
#' @param rho Correlation between price and impact shocks, in \eqn{[-1, 1]}.
#' @param lambda_bar_T Dealer transient execution-impact coefficient.
#' @param lambda_bar_P Inventory-proportional execution concession.
#' @param kappa_J Decay rate of the dealer's transient impact state.
#' @param k_A,k_B Buy- and sell-side temporary execution cost coefficients.
#' @param psi_cost Temporary cost exponent; the model assumes \eqn{(0, 1]}.
#' @param gamma Absolute risk aversion of the dealer (positive).
#' @param Gamma_Q,Gamma_J Terminal liquidation penalties on inventory and on
#'   the dealer impact state.
#' @param Q_bar Inventory bound; the grid is exactly \eqn{[-\bar Q, \bar Q]}.
#' @param nu_bar Trading-rate bound used to build the default control set.
#' @param phi_cap Optional cap on the option payoff; \code{NULL} for no cap.
#'   Irrelevant for puts, whose payoff is already bounded.
#' @param n_options Option notional. Quotes are reported per unit.
#' @param I0,Q0,J0 Initial exogenous impact, inventory, and dealer impact state.
#' @param control_set Optional explicit vector of trading rates; must contain 0.
#'   When \code{NULL} a symmetric grid on \eqn{[-\bar\nu, \bar\nu]} is built.
#' @param n_controls Number of controls in the default set (odd).
#' @param n_logS Log-price grid size. \code{NULL} (default) lets the engine
#'   choose it so that one shock spans exactly one cell, which removes the
#'   dominant source of numerical diffusion.
#' @param n_I,n_Q,n_J Grid sizes for the impact, inventory and dealer-impact
#'   states. When \code{lambda_I = 0} the value function is constant in the
#'   impact state and \code{n_I} is collapsed to 2 exactly. The spread is
#'   noticeably sensitive to \code{n_Q}: a coarse inventory grid cannot resolve
#'   the per-step inventory change \eqn{\bar\nu \Delta t} and overstates the
#'   spread. Check convergence with
#'   \code{inst/scripts/indiff_convergence.R} before quoting a spread.
#' @param n_R Accumulator grid size. This dimension controls most of the
#'   remaining discretisation error in the price level; see
#'   \code{inst/scripts/indiff_convergence.R}.
#' @param option_type \code{"call"} (default) or \code{"put"}.
#' @param accum_rule Quadrature for the running accumulator: \code{"trapezoid"}
#'   (default) or \code{"left"}, the latter matching the plain left-endpoint
#'   rule at the cost of an O(dt) error in the variance of the average.
#' @param accum_sd Accumulator grid half-width in standard deviations of the
#'   running average, capped by the reachable-set bound.
#' @param grid_drift If \code{TRUE} (default) the log-price grid tracks the
#'   deterministic drift so the drift is never interpolated.
#' @param store_policy If \code{TRUE}, return optimal trading-rate and state
#'   paths along the zero-shock trajectory.
#' @param n_threads Number of RcppParallel worker threads used by the backward
#'   sweep; 0 (default) leaves the choice to RcppParallel, and 1 forces a
#'   serial sweep. Results do not depend on this value.
#' @param engine_mode \code{"cached"} (default) or \code{"reference"}. The
#'   reference path is a slow, unoptimised transcription of the recursion kept
#'   as a test oracle.
#' @param verbose Print progress every few time steps.
#' @param validate Whether to validate inputs.
#'
#' @return An object of class \code{"indiff_asian"}: a list with
#'   \code{ask_price}, \code{bid_price}, \code{mid_price}, \code{spread}, the
#'   three initial-state values \code{v0}, \code{v_plus}, \code{v_minus},
#'   \code{diagnostics}, \code{params} and \code{grid_sizes}, plus policy paths
#'   when \code{store_policy = TRUE}.
#'
#' @references
#' Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @seealso \code{\link{price_geometric_asian_indiff}} for the geometric payoff,
#'   \code{\link{price_kemna_vorst_arithmetic}} for the frictionless benchmark.
#'
#' @examples
#' # Tiny grids so the example runs quickly; see below for a realistic setting.
#' price_arithmetic_asian_indiff(
#'   S0 = 100, K = 100, T = 1, N = 5, sigma = 0.2, r_cont = 0.05,
#'   n_I = 5, n_Q = 5, n_J = 5, n_R = 11, n_controls = 5
#' )
#'
#' \donttest{
#' # Realistic resolution (takes on the order of a minute).
#' res <- price_arithmetic_asian_indiff(
#'   S0 = 100, K = 100, T = 1, N = 25, sigma = 0.2, r_cont = 0.05
#' )
#' summary(res)
#' }
#'
#' @export
price_arithmetic_asian_indiff <- function(
    S0, K, T, N = 25,
    mu = r_cont, sigma, r_cont,
    lambda_I = 0, kappa_I = 1, eta = 0.5, rho = 0,
    lambda_bar_T = 0.05, lambda_bar_P = 0.025, kappa_J = 1,
    k_A = 0.05, k_B = 0.05, psi_cost = 1,
    gamma = 0.05,
    Gamma_Q = 1, Gamma_J = 0.1,
    Q_bar = 2, nu_bar = 4,
    phi_cap = NULL, n_options = 1,
    I0 = 0, Q0 = 0, J0 = 0,
    control_set = NULL, n_controls = 15,
    n_logS = NULL, n_I = 21, n_Q = 21, n_J = 15, n_R = 121,
    option_type = c("call", "put"),
    accum_rule = c("trapezoid", "left"),
    accum_sd = 5, grid_drift = TRUE,
    store_policy = FALSE, n_threads = 0,
    engine_mode = c("cached", "reference"),
    verbose = FALSE, validate = TRUE) {

  .indiff_core("arithmetic",
               S0, K, T, N, mu, sigma, r_cont,
               lambda_I, kappa_I, eta, rho,
               lambda_bar_T, lambda_bar_P, kappa_J,
               k_A, k_B, psi_cost,
               gamma, Gamma_Q, Gamma_J,
               Q_bar, nu_bar, phi_cap, n_options,
               I0, Q0, J0,
               control_set, n_controls,
               n_logS, n_I, n_Q, n_J, n_R,
               option_type, accum_rule, accum_sd, grid_drift,
               store_policy, n_threads, engine_mode, verbose, validate)
}


#' Utility-Indifference Bid and Ask for a Geometric Asian Option
#'
#' Geometric-average counterpart of
#' \code{\link{price_arithmetic_asian_indiff}}. The accumulator carries
#' \eqn{a = \int_0^T (\log S_u - \log S_0)\,du} and the payoff is
#' \eqn{\Phi(a) = (S_0 e^{a/T} - K)^+} for a call.
#'
#' @inheritParams price_arithmetic_asian_indiff
#' @param n_Z Alias for \code{n_R}, kept for symmetry with the legacy geometric
#'   interface. When supplied it takes precedence.
#'
#' @return An object of class \code{"indiff_asian"} with
#'   \code{asian_type = "geometric"}; see
#'   \code{\link{price_arithmetic_asian_indiff}}.
#'
#' @seealso \code{\link{price_kemna_vorst_geometric}} for the frictionless
#'   closed form, which is directly comparable: both are time-0 present
#'   values.
#'
#' @examples
#' price_geometric_asian_indiff(
#'   S0 = 100, K = 100, T = 1, N = 5, sigma = 0.2, r_cont = 0.05,
#'   n_I = 5, n_Q = 5, n_J = 5, n_R = 11, n_controls = 5
#' )
#'
#' @export
price_geometric_asian_indiff <- function(
    S0, K, T, N = 25,
    mu = r_cont, sigma, r_cont,
    lambda_I = 0, kappa_I = 1, eta = 0.5, rho = 0,
    lambda_bar_T = 0.05, lambda_bar_P = 0.025, kappa_J = 1,
    k_A = 0.05, k_B = 0.05, psi_cost = 1,
    gamma = 0.05,
    Gamma_Q = 1, Gamma_J = 0.1,
    Q_bar = 2, nu_bar = 4,
    phi_cap = NULL, n_options = 1,
    I0 = 0, Q0 = 0, J0 = 0,
    control_set = NULL, n_controls = 15,
    n_logS = NULL, n_I = 21, n_Q = 21, n_J = 15, n_R = 121, n_Z = NULL,
    option_type = c("call", "put"),
    accum_rule = c("trapezoid", "left"),
    accum_sd = 5, grid_drift = TRUE,
    store_policy = FALSE, n_threads = 0,
    engine_mode = c("cached", "reference"),
    verbose = FALSE, validate = TRUE) {

  if (!is.null(n_Z)) n_R <- n_Z

  .indiff_core("geometric",
               S0, K, T, N, mu, sigma, r_cont,
               lambda_I, kappa_I, eta, rho,
               lambda_bar_T, lambda_bar_P, kappa_J,
               k_A, k_B, psi_cost,
               gamma, Gamma_Q, Gamma_J,
               Q_bar, nu_bar, phi_cap, n_options,
               I0, Q0, J0,
               control_set, n_controls,
               n_logS, n_I, n_Q, n_J, n_R,
               option_type, accum_rule, accum_sd, grid_drift,
               store_policy, n_threads, engine_mode, verbose, validate)
}


#' Print Method for Utility-Indifference Asian Option Results
#'
#' @param x Object of class \code{"indiff_asian"}.
#' @param ... Additional arguments (unused).
#' @return Invisibly returns \code{x}.
#' @export
print.indiff_asian <- function(x, ...) {
  cap <- paste0(toupper(substring(x$asian_type, 1, 1)),
                substring(x$asian_type, 2))
  cat("Utility-Indifference Pricing (", cap, " Asian ", x$option_type, ")\n",
      sep = "")
  cat("=========================================================\n")
  cat(sprintf("  Bid price:   %.6f\n", x$bid_price))
  cat(sprintf("  Ask price:   %.6f\n", x$ask_price))
  cat(sprintf("  Mid price:   %.6f\n", x$mid_price))
  cat(sprintf("  Spread:      %.6f\n", x$spread))
  cat(sprintf("  (v- = %.4f  >=  v0 = %.4f  >=  v+ = %.4f)\n",
              x$v_minus, x$v0, x$v_plus))
  cat("\n")
  p <- x$params
  cat(sprintf("Parameters:  S0=%.2f  K=%.2f  T=%.2f  N=%d  sigma=%.3f  r=%.4f\n",
              p$S0, p$K, p$T, p$N, p$sigma, p$r_cont))
  cat(sprintf("             gamma=%.4g  Gamma_Q=%.3g  Gamma_J=%.3g  Q_bar=%.2f  nu_bar=%.2f\n",
              p$gamma, p$Gamma_Q, p$Gamma_J, p$Q_bar, p$nu_bar))
  cat(sprintf("             lambda_I=%.4g  lambda_T=%.4g  lambda_P=%.4g  kappa_J=%.3g\n",
              p$lambda_I, p$lambda_bar_T, p$lambda_bar_P, p$kappa_J))
  cat(sprintf("             k_A=%.3g  k_B=%.3g  psi=%.2f  rho=%.2f\n",
              p$k_A, p$k_B, p$psi_cost, p$rho))
  g <- x$grid_sizes
  cat(sprintf("Grid sizes:  n_logS=%d  n_I=%d  n_Q=%d  n_J=%d  n_R=%d  n_controls=%d  (%.2fM states)\n",
              g$n_logS, g$n_I, g$n_Q, g$n_J, g$n_R, g$n_controls,
              g$n_states / 1e6))
  invisible(x)
}


#' Summary Method for Utility-Indifference Asian Option Results
#'
#' Prints the quotes together with the numerical diagnostics that indicate
#' whether the grids were adequate.
#'
#' @param object Object of class \code{"indiff_asian"}.
#' @param ... Additional arguments (unused).
#' @return Invisibly returns \code{object}.
#' @export
summary.indiff_asian <- function(object, ...) {
  print(object)
  d <- object$diagnostics
  cat("\nDiagnostics:\n")
  cat(sprintf("  Runtime:              %.2f s (3 value functions)\n",
              d$runtime_sec))
  cat(sprintf("  Parallel backend:     %s (%d thread(s))\n",
              d$parallel_backend, d$n_threads))
  cat(sprintf("  Log-price grid:       %s (shock spans %d cell(s))\n",
              if (isTRUE(d$grid_aligned)) "aligned" else "NOT aligned",
              d$shock_cells))
  if (isTRUE(d$collapsed_I)) {
    cat("  Impact dimension:     collapsed (lambda_I = 0, exact)\n")
  }
  cat(sprintf("  Initial state interior: %s\n",
              if (isTRUE(d$initial_state_interior)) "yes" else "NO"))
  cf <- d$clamp_fraction$baseline
  cat("  Clamped bracket fraction by dimension:\n")
  cat(sprintf("    logS %.2e   I %.2e   Q %.2e   J %.2e   R %.2e\n",
              cf[["logS"]], cf[["I"]], cf[["Q"]], cf[["J"]], cf[["R"]]))
  if (!isTRUE(d$grid_aligned)) {
    cat("  NOTE: an unaligned log-price grid inflates the effective ",
        "volatility.\n", sep = "")
    cat("        Leave n_logS = NULL to let the engine size it.\n")
  }
  invisible(object)
}


#' Plot Optimal Hedging Paths from Utility-Indifference Pricing
#'
#' Plots the optimal trading rate and inventory along the zero-shock trajectory
#' for the dealer's short and long positions. Requires the object to have been
#' produced with \code{store_policy = TRUE}.
#'
#' @param x Object of class \code{"indiff_asian"}.
#' @param which One of \code{"nu"} (trading rate), \code{"Q"} (inventory),
#'   \code{"J"} (dealer impact state), or \code{"all"}.
#' @param ... Passed to \code{\link[graphics]{matplot}}.
#' @return Invisibly returns \code{x}.
#' @export
plot.indiff_asian <- function(x, which = c("all", "nu", "Q", "J"), ...) {
  which <- match.arg(which)
  if (is.null(x$optimal_nu_seller)) {
    stop("No policy paths stored; re-run with store_policy = TRUE")
  }
  N <- x$params$N
  dt <- x$params$T / N

  panels <- if (which == "all") c("nu", "Q", "J") else which
  old <- graphics::par(mfrow = c(length(panels), 1),
                       mar = c(4, 4, 2, 1))
  on.exit(graphics::par(old), add = TRUE)

  # lty/col are given plot-appropriate defaults but may be overridden via ...;
  # pulling them out avoids "formal argument matched by multiple actual
  # arguments" and keeps the legend consistent with whatever was used to plot.
  dots <- list(...)
  lty <- if (!is.null(dots$lty)) dots$lty else c(1, 2)
  col <- if (!is.null(dots$col)) dots$col else c(1, 2)
  dots$lty <- NULL
  dots$col <- NULL

  for (p in panels) {
    if (p == "nu") {
      tt <- seq(0, x$params$T - dt, length.out = N)
      m <- cbind(seller = x$optimal_nu_seller, buyer = x$optimal_nu_buyer)
      ylab <- "trading rate nu"
    } else if (p == "Q") {
      tt <- seq(0, x$params$T, length.out = N + 1)
      m <- cbind(seller = x$Q_path_seller, buyer = x$Q_path_buyer)
      ylab <- "inventory Q"
    } else {
      tt <- seq(0, x$params$T, length.out = N + 1)
      m <- cbind(seller = x$J_path_seller, buyer = x$J_path_buyer)
      ylab <- "dealer impact J"
    }
    do.call(graphics::matplot,
            c(list(tt, m, type = "l", lty = lty, col = col,
                   xlab = "t", ylab = ylab), dots))
    graphics::abline(h = 0, col = "grey70", lty = 3)
    graphics::legend("topright", legend = c("short (ask)", "long (bid)"),
                     lty = lty, col = col, bty = "n", cex = 0.8)
  }
  invisible(x)
}
