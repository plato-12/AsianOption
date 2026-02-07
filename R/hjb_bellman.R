#' Price Arithmetic Asian Option via HJB Bellman Scheme (Endogenous Impact)
#'
#' Solves the endogenous (controlled) pricing problem for arithmetic Asian
#' options under transient price impact using backward induction on a state
#' grid. Returns bid price, ask price, and optimal trading volumes for
#' all time periods.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param T Maturity in years (must be positive)
#' @param N Number of time steps (positive integer)
#' @param sigma Volatility (annualized, must be positive)
#' @param r_cont Continuous risk-free rate (annualized)
#' @param kappa Mean-reversion rate for transient impact state
#'   (non-negative)
#' @param lambda_bar_T Transient impact coefficient at continuous
#'   scale (non-negative)
#' @param lambda_bar_P Permanent impact coefficient at continuous
#'   scale (non-negative)
#' @param k_A Buy-side cost coefficient (non-negative)
#' @param k_B Sell-side cost coefficient (non-negative)
#' @param psi_cost Power-law exponent for the cost function,
#'   in (0, 1]
#' @param eta Noise-trader intensity. Scalar (constant) or vector
#'   of length N. Must be non-negative. Default is 1.0.
#' @param p Binomial probability (default 0.5). Must be in (0, 1).
#' @param I0 Initial transient impact state (default 0)
#' @param control_set Numeric vector of candidate control values.
#'   If NULL, a uniform grid from \code{nu_min} to \code{nu_max}
#'   is constructed.
#' @param nu_min Minimum control value (used if \code{control_set}
#'   is NULL)
#' @param nu_max Maximum control value (used if \code{control_set}
#'   is NULL)
#' @param n_controls Number of control grid points (used if
#'   \code{control_set} is NULL)
#' @param n_logS Grid size for log-S dimension.
#'   Default is \code{2*N + 1}.
#' @param n_I Grid size for I (transient state) dimension.
#'   Default is 51.
#' @param n_Y Grid size for Y (running sum) dimension.
#'   Default is 51.
#' @param option_type Character; either \code{"call"} (default) or
#'   \code{"put"}.
#' @param validate Logical; if TRUE, performs input validation.
#'
#' @details
#' This function implements Algorithm 1 from the paper: a tree-based
#' Bellman scheme for the endogenous HJB pricing problem. It solves
#' both the seller's (ask) and buyer's (bid) problems to produce
#' a bid-ask spread.
#'
#' The seller minimizes cost + expected payoff (inf), while the buyer
#' maximizes expected payoff - cost (sup).
#'
#' The optimal trading rates \code{nu_m} are extracted via a forward
#' pass along the expected (drift-only) path from the initial state
#' \eqn{(S_0, I_0, Y_0 = 0)}.
#' Volumes are \code{nu_m * dt}.
#'
#' @return A list with S3 class \code{"hjb_asian"} containing:
#' \describe{
#'   \item{ask_price}{Seller's value (ask) at t=0}
#'   \item{bid_price}{Buyer's value (bid) at t=0}
#'   \item{spread}{Ask minus bid}
#'   \item{optimal_nu}{Seller's optimal trading rates for all N
#'     periods (numeric vector)}
#'   \item{optimal_volumes}{Seller's optimal volumes for all N
#'     periods (nu * dt)}
#'   \item{optimal_nu_buyer}{Buyer's optimal trading rates}
#'   \item{optimal_volumes_buyer}{Buyer's optimal volumes}
#'   \item{option_type}{Option type used}
#'   \item{asian_type}{Type of Asian option ("arithmetic")}
#'   \item{params}{List of input parameters}
#'   \item{grid_sizes}{List of grid dimensions used}
#' }
#'
#' @export
#'
#' @examples
#' result <- price_arithmetic_asian_hjb(
#'   S0 = 100, K = 100, T = 1, N = 10,
#'   sigma = 0.2, r_cont = 0.05,
#'   kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
#'   k_A = 0.01, k_B = 0.01, psi_cost = 1,
#'   n_I = 21, n_Y = 21
#' )
#' print(result)
#'
#' @seealso \code{\link{price_geometric_asian_hjb}}
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

  cpp_args <- list(
    S0, K, T, as.integer(N), sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta_vec, p, I0,
    control_set,
    as.integer(n_logS), as.integer(n_I), as.integer(n_Y)
  )

  # Seller (ask): problem_type = 0
  raw_ask <- do.call(
    hjb_arithmetic_asian_cpp,
    c(cpp_args, problem_type = 0L)
  )

  # Buyer (bid): problem_type = 1
  raw_bid <- do.call(
    hjb_arithmetic_asian_cpp,
    c(cpp_args, problem_type = 1L)
  )

  if (option_type == "put") {
    warning(
      "Put option not yet implemented for HJB scheme. ",
      "Returning call values."
    )
  }

  result <- list(
    ask_price = raw_ask$value,
    bid_price = raw_bid$value,
    spread = raw_ask$value - raw_bid$value,
    optimal_nu = raw_ask$optimal_nu,
    optimal_volumes = raw_ask$optimal_volumes,
    optimal_nu_buyer = raw_bid$optimal_nu,
    optimal_volumes_buyer = raw_bid$optimal_volumes,
    option_type = option_type,
    asian_type = "arithmetic",
    params = list(
      S0 = S0, K = K, T = T, N = N,
      sigma = sigma, r_cont = r_cont, kappa = kappa,
      lambda_bar_T = lambda_bar_T,
      lambda_bar_P = lambda_bar_P,
      k_A = k_A, k_B = k_B,
      psi_cost = psi_cost, p = p, I0 = I0
    ),
    grid_sizes = list(
      n_logS = n_logS, n_I = n_I, n_Y = n_Y,
      n_controls = length(control_set)
    )
  )
  class(result) <- c("hjb_asian", "list")
  return(result)
}


#' Price Geometric Asian Option via HJB Bellman Scheme
#'
#' Solves the endogenous (controlled) pricing problem for geometric
#' Asian options under transient price impact. Returns bid price,
#' ask price, and optimal trading volumes for all time periods.
#'
#' @inheritParams price_arithmetic_asian_hjb
#' @param n_Z Grid size for Z (running log-integral) dimension.
#'   Default is 51.
#'
#' @return A list with S3 class \code{"hjb_asian"} containing:
#' \describe{
#'   \item{ask_price}{Seller's value (ask) at t=0}
#'   \item{bid_price}{Buyer's value (bid) at t=0}
#'   \item{spread}{Ask minus bid}
#'   \item{optimal_nu}{Seller's optimal trading rates for all N
#'     periods}
#'   \item{optimal_volumes}{Seller's optimal volumes (nu * dt)}
#'   \item{optimal_nu_buyer}{Buyer's optimal trading rates}
#'   \item{optimal_volumes_buyer}{Buyer's optimal volumes}
#'   \item{option_type}{Option type used}
#'   \item{asian_type}{Type of Asian option ("geometric")}
#'   \item{params}{List of input parameters}
#'   \item{grid_sizes}{List of grid dimensions used}
#' }
#'
#' @export
#'
#' @examples
#' result <- price_geometric_asian_hjb(
#'   S0 = 100, K = 100, T = 1, N = 10,
#'   sigma = 0.2, r_cont = 0.05,
#'   kappa = 1, lambda_bar_T = 0.1, lambda_bar_P = 0.05,
#'   k_A = 0.01, k_B = 0.01, psi_cost = 1,
#'   n_I = 21, n_Z = 21
#' )
#' print(result)
#'
#' @seealso \code{\link{price_arithmetic_asian_hjb}}
price_geometric_asian_hjb <- function(
    S0, K, T, N, sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta = 1.0, p = 0.5, I0 = 0,
    control_set = NULL,
    nu_min = -5, nu_max = 5,
    n_controls = 31,
    n_logS = NULL, n_I = 51, n_Z = 51,
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

  cpp_args <- list(
    S0, K, T, as.integer(N), sigma, r_cont,
    kappa, lambda_bar_T, lambda_bar_P,
    k_A, k_B, psi_cost,
    eta_vec, p, I0,
    control_set,
    as.integer(n_logS), as.integer(n_I), as.integer(n_Z)
  )

  raw_ask <- do.call(
    hjb_geometric_asian_cpp,
    c(cpp_args, problem_type = 0L)
  )

  raw_bid <- do.call(
    hjb_geometric_asian_cpp,
    c(cpp_args, problem_type = 1L)
  )

  if (option_type == "put") {
    warning(
      "Put option not yet implemented for HJB scheme. ",
      "Returning call values."
    )
  }

  result <- list(
    ask_price = raw_ask$value,
    bid_price = raw_bid$value,
    spread = raw_ask$value - raw_bid$value,
    optimal_nu = raw_ask$optimal_nu,
    optimal_volumes = raw_ask$optimal_volumes,
    optimal_nu_buyer = raw_bid$optimal_nu,
    optimal_volumes_buyer = raw_bid$optimal_volumes,
    option_type = option_type,
    asian_type = "geometric",
    params = list(
      S0 = S0, K = K, T = T, N = N,
      sigma = sigma, r_cont = r_cont, kappa = kappa,
      lambda_bar_T = lambda_bar_T,
      lambda_bar_P = lambda_bar_P,
      k_A = k_A, k_B = k_B,
      psi_cost = psi_cost, p = p, I0 = I0
    ),
    grid_sizes = list(
      n_logS = n_logS, n_I = n_I, n_Z = n_Z,
      n_controls = length(control_set)
    )
  )
  class(result) <- c("hjb_asian", "list")
  return(result)
}


#' @export
print.hjb_asian <- function(x, ...) {
  cat(sprintf(
    "HJB Bellman Pricing: %s %s Asian Option\n",
    x$option_type, x$asian_type
  ))
  cat("================================================\n")
  cat(sprintf("Ask price (seller): %.6f\n", x$ask_price))
  cat(sprintf("Bid price (buyer):  %.6f\n", x$bid_price))
  cat(sprintf("Spread (ask - bid): %.6f\n", x$spread))
  cat("\n")

  N <- x$params$N
  dt <- x$params$T / N
  times <- (seq_len(N) - 1) * dt
  cat(sprintf(
    "Optimal trading rates (seller), periods 0..%d:\n",
    N - 1
  ))
  nu_df <- data.frame(
    period = seq_len(N) - 1,
    time = round(times, 4),
    nu = round(x$optimal_nu, 4),
    volume = round(x$optimal_volumes, 6)
  )
  print(nu_df, row.names = FALSE)
  cat("\n")

  cat(sprintf(
    "Parameters: S0=%.2f, K=%.2f, T=%.2f, N=%d\n",
    x$params$S0, x$params$K, x$params$T, N
  ))
  cat(sprintf(
    "  sigma=%.4f, r_cont=%.4f, kappa=%.4f\n",
    x$params$sigma, x$params$r_cont, x$params$kappa
  ))
  cat(sprintf(
    "  lambda_bar_T=%.4f, lambda_bar_P=%.4f\n",
    x$params$lambda_bar_T, x$params$lambda_bar_P
  ))
  cat(sprintf(
    "  k_A=%.4f, k_B=%.4f, psi_cost=%.4f\n",
    x$params$k_A, x$params$k_B, x$params$psi_cost
  ))
  cat("\n")

  y_or_z <- if (x$asian_type == "arithmetic") "Y" else "Z"
  cat(sprintf(
    "Grid: %d (log-S) x %d (I) x %d (%s)\n",
    x$grid_sizes$n_logS, x$grid_sizes$n_I,
    x$grid_sizes[[3]], y_or_z
  ))
  cat(sprintf(
    "Controls: %d values\n", x$grid_sizes$n_controls
  ))

  invisible(x)
}
