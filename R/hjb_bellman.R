#' Price Arithmetic Asian Option via HJB Bellman Scheme (Endogenous Impact)
#'
#' Computes bid and ask prices for an arithmetic Asian option under
#' transient price impact using a Bellman (HJB) scheme.
#'
#' Bid and ask are defined via value-function differences:
#' a baseline problem (no option), a long-option problem, and a short-option
#' problem are solved internally in C++.
#'
#' @inheritParams price_geometric_asian_hjb
#'
#' @return A list with S3 class \code{"hjb_asian"} containing:
#' \describe{
#'   \item{ask_price}{Ask (seller’s indifference) price at t=0}
#'   \item{bid_price}{Bid (buyer’s indifference) price at t=0}
#'   \item{mid_price}{Mid price}
#'   \item{spread}{Ask minus bid}
#'   \item{asian_type}{Type of Asian option ("arithmetic")}
#'   \item{option_type}{Option type}
#'   \item{params}{List of input parameters}
#'   \item{grid_sizes}{Grid sizes used in the computation}
#' }
#'
#' @export
price_arithmetic_asian_hjb <- function(
    S0, K, T, N, sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta = 1.0, p = 0.5, I0 = 0,
    control_set = NULL,
    nu_min = -5, nu_max = 5,
    n_controls = 31,
    n_logS = NULL, n_I = 51, n_Y = 51,
    option_type = "call",
    validate = TRUE
) {

  if (validate) {
    validate_hjb_inputs(
      S0, K, T, N, sigma, r_cont,
      kappa, lambda_bar_T, lambda_bar_P,
      k_A, k_B, psi_cost, eta, p, I0
    )
  }

  option_type <- match.arg(option_type, c("call", "put"))

  if (is.null(n_logS)) {
    n_logS <- 2L * as.integer(N) + 1L
  }

  eta_vec <- if (length(eta) == 1) rep(eta, N) else eta
  if (length(eta_vec) != N) {
    stop("eta must be a scalar or a vector of length N")
  }

  if (is.null(control_set)) {
    control_set <- seq(nu_min, nu_max, length.out = n_controls)
  }

  quotes <- hjb_arithmetic_quotes_cpp(
    S0, K, T, as.integer(N),
    sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta_vec, p, I0,
    control_set,
    as.integer(n_logS), as.integer(n_I), as.integer(n_Y)
  )

  if (option_type == "put") {
    warning("Put pricing not yet implemented; returning call prices.")
  }

  result <- list(
    ask_price = quotes$ask,
    bid_price = quotes$bid,
    mid_price = quotes$mid,
    spread = quotes$ask - quotes$bid,
    seller_indiff = quotes$seller_indiff,
    buyer_indiff = quotes$buyer_indiff,
    option_type = option_type,
    asian_type = "arithmetic",
    params = list(
      S0 = S0, K = K, T = T, N = N,
      sigma = sigma, r_cont = r_cont,
      kappa = kappa,
      lambda_bar_T = lambda_bar_T,
      lambda_bar_P = lambda_bar_P,
      k_A = k_A, k_B = k_B,
      psi_cost = psi_cost,
      p = p, I0 = I0
    ),
    grid_sizes = list(
      n_logS = n_logS,
      n_I = n_I,
      n_Y = n_Y,
      n_controls = length(control_set)
    )
  )

  class(result) <- c("hjb_asian", "list")
  result
}

#' Price Geometric Asian Option via HJB Bellman Scheme (Endogenous Impact)
#'
#' Computes bid and ask prices for a geometric Asian option under
#' transient price impact using a Bellman (HJB) scheme. Same interface as
#' \code{\link{price_arithmetic_asian_hjb}}; the running average is geometric
#' (average of log-price, then exp).
#'
#' @param S0 Initial stock price (positive).
#' @param K Strike price (positive).
#' @param T Time to maturity (positive).
#' @param N Number of time steps (positive integer).
#' @param sigma Volatility (positive).
#' @param r_cont Continuous risk-free rate.
#' @param kappa Mean reversion rate for impact (non-negative).
#' @param lambda_bar_T Transient impact coefficient (non-negative).
#' @param lambda_bar_P Permanent impact coefficient (non-negative).
#' @param k_A,k_B Cost coefficients (non-negative).
#' @param psi_cost Cost exponent (in (0, 2]).
#' @param eta Noise trader intensity (scalar or length-N vector).
#' @param p Probability of up move (in (0, 1)).
#' @param I0 Initial impact state.
#' @param control_set Optional numeric vector of controls; otherwise built from \code{nu_min}, \code{nu_max}, \code{n_controls}.
#' @param nu_min,nu_max,n_controls Control grid (used if \code{control_set} is NULL).
#' @param n_logS,n_I Grid sizes for log-price and impact state.
#' @param n_Y Grid size for running state (integral of log S). Ignored if \code{n_Z} is provided.
#' @param n_Z Grid size for running state (alias for geometric case). If provided, used instead of \code{n_Y}.
#' @param option_type \code{"call"} or \code{"put"}.
#' @param validate Whether to validate inputs.
#' @return List with S3 class \code{"hjb_asian"} (same structure as arithmetic), with \code{asian_type = "geometric"}.
#' @export
price_geometric_asian_hjb <- function(
    S0, K, T, N, sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta = 1.0, p = 0.5, I0 = 0,
    control_set = NULL,
    nu_min = -5, nu_max = 5,
    n_controls = 31,
    n_logS = NULL, n_I = 51, n_Y = 51,
    n_Z = NULL,
    option_type = "call",
    validate = TRUE
) {

  if (validate) {
    validate_hjb_inputs(
      S0, K, T, N, sigma, r_cont,
      kappa, lambda_bar_T, lambda_bar_P,
      k_A, k_B, psi_cost, eta, p, I0
    )
  }

  option_type <- match.arg(option_type, c("call", "put"))

  if (is.null(n_logS)) {
    n_logS <- 2L * as.integer(N) + 1L
  }

  n_state <- if (!is.null(n_Z)) as.integer(n_Z) else as.integer(n_Y)

  eta_vec <- if (length(eta) == 1) rep(eta, N) else eta
  if (length(eta_vec) != N) {
    stop("eta must be a scalar or a vector of length N")
  }

  if (is.null(control_set)) {
    control_set <- seq(nu_min, nu_max, length.out = n_controls)
  }

  quotes <- hjb_geometric_quotes_cpp(
    S0, K, T, as.integer(N),
    sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta_vec, p, I0,
    control_set,
    as.integer(n_logS), as.integer(n_I), n_state
  )

  if (option_type == "put") {
    warning("Put pricing not yet implemented; returning call prices.")
  }

  result <- list(
    ask_price = quotes$ask,
    bid_price = quotes$bid,
    mid_price = quotes$mid,
    spread = quotes$ask - quotes$bid,
    seller_indiff = quotes$seller_indiff,
    buyer_indiff = quotes$buyer_indiff,
    option_type = option_type,
    asian_type = "geometric",
    params = list(
      S0 = S0, K = K, T = T, N = N,
      sigma = sigma, r_cont = r_cont,
      kappa = kappa,
      lambda_bar_T = lambda_bar_T,
      lambda_bar_P = lambda_bar_P,
      k_A = k_A, k_B = k_B,
      psi_cost = psi_cost,
      p = p, I0 = I0
    ),
    grid_sizes = list(
      n_logS = n_logS,
      n_I = n_I,
      n_Y = n_state,
      n_Z = n_state,
      n_controls = length(control_set)
    )
  )

  class(result) <- c("hjb_asian", "list")
  result
}

#' Print method for HJB Asian option results
#'
#' @param x Object of class \code{hjb_asian} from \code{price_arithmetic_asian_hjb}.
#' @param ... Additional arguments (unused).
#' @return Invisible \code{x}.
#' @export
print.hjb_asian <- function(x, ...) {
  atype <- if (!is.null(x$asian_type)) x$asian_type else "arithmetic"
  cap <- paste0(toupper(substring(atype, 1, 1)), substring(atype, 2))
  cat("HJB Bellman Pricing (", cap, " Asian)\n", sep = "")
  cat("=====================================\n")
  cat(sprintf("  Bid price:   %.6f\n", x$bid_price))
  cat(sprintf("  Ask price:   %.6f\n", x$ask_price))
  cat(sprintf("  Mid price:   %.6f\n", x$mid_price))
  cat(sprintf("  Spread:      %.6f\n", x$spread))
  if (!is.null(x$seller_indiff) && !is.null(x$buyer_indiff)) {
    cat(sprintf("  (Seller indiff: %.6f  |  Buyer indiff: %.6f)\n",
                x$seller_indiff, x$buyer_indiff))
  }
  cat("\n")
  cat("Parameters:  ")
  p <- x$params
  cat(sprintf("S0=%.2f  K=%.2f  T=%.2f  N=%d  sigma=%.2f  r=%.2f\n",
              p$S0, p$K, p$T, p$N, p$sigma, p$r_cont))
  cat(sprintf("            kappa=%.2f  lambda_T=%.3f  lambda_P=%.3f  k_A=%.3f  k_B=%.3f  psi=%.2f\n",
              p$kappa, p$lambda_bar_T, p$lambda_bar_P, p$k_A, p$k_B, p$psi_cost))
  if (!is.null(x$grid_sizes)) {
    g <- x$grid_sizes
    ylab <- if (!is.null(g$n_Z)) "n_Z" else "n_Y"
    yval <- if (!is.null(g$n_Z)) g$n_Z else g$n_Y
    cat(sprintf("Grid sizes:   n_logS=%d  n_I=%d  %s=%d  n_controls=%d\n",
                g$n_logS, g$n_I, ylab, yval, g$n_controls))
  }
  cat("Option:      ", if (!is.null(x$asian_type)) x$asian_type else "arithmetic",
      " Asian  ", x$option_type, "\n", sep = "")
  invisible(x)
}
