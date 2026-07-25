#' Validate Input Parameters for Utility-Indifference Pricing
#'
#' Internal validator shared by \code{\link{price_arithmetic_asian_indiff}} and
#' \code{\link{price_geometric_asian_indiff}}. Beyond range checks it enforces
#' the admissibility conditions of the dealer problem: the initial inventory
#' must respect its bound, the initial execution price must be positive, and the
#' control set must allow the dealer to stand still.
#'
#' @param S0 Initial stock price.
#' @param K Strike price.
#' @param T Maturity in years.
#' @param N Number of time steps.
#' @param sigma Volatility.
#' @param r_cont Continuously compounded risk-free rate.
#' @param mu Drift (scalar, length-N vector, or function of time).
#' @param lambda_I Loading of the exogenous impact state in the price drift.
#' @param kappa_I Mean-reversion rate of the exogenous impact state.
#' @param eta Impact noise amplitude (scalar, length-N vector, or function).
#' @param rho Correlation between the price and impact shocks.
#' @param lambda_bar_T Dealer transient execution-impact coefficient.
#' @param lambda_bar_P Inventory-proportional execution concession.
#' @param kappa_J Decay rate of the dealer transient state.
#' @param k_A,k_B Buy- and sell-side temporary cost coefficients.
#' @param psi_cost Temporary cost exponent.
#' @param gamma Absolute risk aversion of the dealer.
#' @param Gamma_Q,Gamma_J Terminal liquidation penalties.
#' @param Q_bar Inventory bound.
#' @param nu_bar Trading-rate bound.
#' @param phi_cap Optional payoff cap.
#' @param n_options Option notional.
#' @param I0,Q0,J0 Initial states.
#' @param control_set Numeric vector of admissible trading rates.
#' @param n_logS,n_I,n_Q,n_J,n_R Grid sizes.
#' @param accum_sd Accumulator grid half-width in standard deviations.
#'
#' @return \code{NULL}, invisibly. Throws an error if validation fails.
#' @keywords internal
validate_indiff_inputs <- function(S0, K, T, N, sigma, r_cont, mu,
                                   lambda_I, kappa_I, eta, rho,
                                   lambda_bar_T, lambda_bar_P, kappa_J,
                                   k_A, k_B, psi_cost,
                                   gamma, Gamma_Q, Gamma_J,
                                   Q_bar, nu_bar, phi_cap, n_options,
                                   I0, Q0, J0, control_set,
                                   n_logS, n_I, n_Q, n_J, n_R, accum_sd) {

  if (!is.numeric(S0) || length(S0) != 1 || S0 <= 0) stop("S0 must be positive")
  if (!is.numeric(K) || length(K) != 1 || K <= 0) stop("K must be positive")
  if (!is.numeric(T) || length(T) != 1 || T <= 0) {
    stop("T (maturity) must be positive")
  }
  if (!is.numeric(N) || length(N) != 1 || N != as.integer(N) || N <= 0) {
    stop("N must be a positive integer")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be positive")
  }
  if (!is.numeric(r_cont) || length(r_cont) != 1) {
    stop("r_cont must be a number")
  }

  if (!is.numeric(rho) || length(rho) != 1 || abs(rho) > 1) {
    stop("rho must be in [-1, 1]")
  }
  if (!is.numeric(gamma) || length(gamma) != 1 || gamma <= 0) {
    stop("gamma (risk aversion) must be positive")
  }

  if (!is.numeric(lambda_I) || length(lambda_I) != 1) {
    stop("lambda_I must be a number")
  }
  if (!is.numeric(kappa_I) || length(kappa_I) != 1 || kappa_I < 0) {
    stop("kappa_I must be non-negative")
  }
  if (!is.numeric(kappa_J) || length(kappa_J) != 1 || kappa_J < 0) {
    stop("kappa_J must be non-negative")
  }
  if (!is.numeric(lambda_bar_T) || length(lambda_bar_T) != 1 ||
      lambda_bar_T < 0) {
    stop("lambda_bar_T must be non-negative")
  }
  if (!is.numeric(lambda_bar_P) || length(lambda_bar_P) != 1 ||
      lambda_bar_P < 0) {
    stop("lambda_bar_P must be non-negative")
  }
  if (!is.numeric(k_A) || length(k_A) != 1 || k_A < 0) {
    stop("k_A must be non-negative")
  }
  if (!is.numeric(k_B) || length(k_B) != 1 || k_B < 0) {
    stop("k_B must be non-negative")
  }

  if (!is.numeric(psi_cost) || length(psi_cost) != 1 ||
      psi_cost <= 0 || psi_cost > 2) {
    stop("psi_cost must be in (0, 2]")
  }
  if (psi_cost > 1) {
    warning(sprintf(
      "psi_cost = %.3f is above 1, outside the range (0, 1] assumed by the model.",
      psi_cost
    ))
  }

  if (!is.numeric(Gamma_Q) || length(Gamma_Q) != 1 || Gamma_Q < 0) {
    stop("Gamma_Q must be non-negative")
  }
  if (!is.numeric(Gamma_J) || length(Gamma_J) != 1 || Gamma_J < 0) {
    stop("Gamma_J must be non-negative")
  }

  if (!is.numeric(Q_bar) || length(Q_bar) != 1 || Q_bar <= 0) {
    stop("Q_bar must be positive")
  }
  # nu_bar is derived from control_set when the caller supplies one, and a
  # control set of {0} (a dealer forbidden to trade) is a legitimate baseline.
  if (!is.numeric(nu_bar) || length(nu_bar) != 1 || nu_bar < 0) {
    stop("nu_bar must be non-negative")
  }
  if (!is.numeric(n_options) || length(n_options) != 1 || n_options <= 0) {
    stop("n_options must be positive")
  }

  if (!is.null(phi_cap)) {
    if (!is.numeric(phi_cap) || length(phi_cap) != 1 || phi_cap <= 0) {
      stop("phi_cap must be a positive number or NULL")
    }
  }

  if (!is.numeric(I0) || length(I0) != 1) stop("I0 must be a number")
  if (!is.numeric(Q0) || length(Q0) != 1) stop("Q0 must be a number")
  if (!is.numeric(J0) || length(J0) != 1) stop("J0 must be a number")

  if (abs(Q0) > Q_bar) {
    stop(sprintf("abs(Q0) = %.4f must not exceed Q_bar = %.4f", abs(Q0), Q_bar))
  }

  exec_price0 <- S0 + lambda_bar_P * Q0 + lambda_bar_T * J0
  if (exec_price0 <= 0) {
    stop(sprintf(
      paste0("Initial execution price S0 + lambda_bar_P*Q0 + lambda_bar_T*J0 ",
             "= %.4f must be positive"),
      exec_price0
    ))
  }

  if (!is.numeric(control_set) || length(control_set) < 1) {
    stop("control_set must be a non-empty numeric vector")
  }
  if (!any(control_set == 0)) {
    stop("control_set must contain 0")
  }

  grid_sizes <- c(n_I = n_I, n_Q = n_Q, n_J = n_J, n_R = n_R)
  if (!is.null(n_logS)) grid_sizes <- c(grid_sizes, n_logS = n_logS)
  for (nm in names(grid_sizes)) {
    v <- grid_sizes[[nm]]
    if (!is.numeric(v) || length(v) != 1 || v != as.integer(v) || v < 5) {
      stop(sprintf("%s must be an integer of at least 5", nm))
    }
  }

  if (!is.numeric(accum_sd) || length(accum_sd) != 1 || accum_sd <= 0) {
    stop("accum_sd must be positive")
  }

  if (!is.function(eta)) {
    if (!is.numeric(eta) || any(eta < 0)) {
      stop("eta must be a non-negative number, vector, or function of t")
    }
  }
  if (!is.function(mu) && !is.numeric(mu)) {
    stop("mu must be a number, vector, or function of t")
  }

  dt <- T / N
  if (kappa_I * dt > 1) {
    warning(sprintf(
      paste0("kappa_I * dt = %.4f exceeds 1; the Euler update of the impact ",
             "state is unstable. Increase N."),
      kappa_I * dt
    ))
  }
  if (kappa_J * dt > 1) {
    warning(sprintf(
      paste0("kappa_J * dt = %.4f exceeds 1; the Euler update of the dealer ",
             "impact state is unstable. Increase N."),
      kappa_J * dt
    ))
  }

  # Flag F1 of design.md: on a compact grid the CARA certainty equivalent is
  # always finite, but once gamma times the inventory mark-to-market scale is
  # large the quotes are governed by the grid corners rather than by the
  # economics, and the truncation stops being innocuous.
  if (is.null(phi_cap) && gamma * Q_bar * S0 > 50) {
    warning(sprintf(
      paste0("gamma * Q_bar * S0 = %.1f is large; the certainty equivalent is ",
             "then dominated by the grid boundary. Consider a smaller gamma, ",
             "a smaller Q_bar, or setting phi_cap."),
      gamma * Q_bar * S0
    ))
  }

  invisible(NULL)
}
