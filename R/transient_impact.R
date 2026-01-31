#' Price Geometric Asian Option with Transient and Permanent Price Impact
#'
#' Computes the price of a geometric Asian option using the binomial model with
#' both transient and permanent price impact from hedging activities.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda_P Permanent price impact coefficient (non-negative)
#' @param lambda_T Transient price impact coefficient (non-negative)
#' @param alpha Decay rate for transient impact (must be in [0, 1))
#' @param psi Power-law exponent for volume (typically in [0.5, 1])
#' @param volumes Vector of hedging volumes at each time step (length n)
#' @param option_type Character; either "call" (default) or "put"
#' @param validate Logical; if TRUE, performs input validation
#'
#' @details
#' This function implements the transient impact model where price impact has
#' both permanent and transient components. The transient component decays
#' exponentially with rate alpha.
#'
#' The stock price dynamics are:
#' \deqn{S_{m+1} = u \cdot S_m \cdot \exp(\lambda_P v_m^\psi + \lambda_T \sum_{k=0}^m \alpha^{m-k} \epsilon_k v_k^\psi)}
#' for an up move, and similarly for a down move.
#'
#' Key parameters:
#' \itemize{
#'   \item lambda_P: Permanent impact (persistent price change)
#'   \item lambda_T: Transient impact (temporary price change)
#'   \item alpha: Decay rate (alpha = 0 means no memory, alpha close to 1 means long memory)
#'   \item psi: Volume power-law (psi = 1 is linear, psi = 0.5 is square-root)
#' }
#'
#' Unlike the permanent impact model, the risk-neutral probability remains
#' path-dependent even for constant volumes, due to the transient accumulator
#' term in the numerator of the probability formula.
#'
#' @return Numeric value of the option price.
#'
#' @export
#'
#' @examples
#' # Example 1: Constant volumes with transient impact
#' volumes <- rep(1, 10)
#' price <- price_geometric_asian_transient(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda_P = 0.05, lambda_T = 0.05,
#'   alpha = 0.5, psi = 1,
#'   volumes = volumes
#' )
#'
#' # Example 2: Time-varying volumes
#' volumes <- seq(0.5, 1.5, length.out = 10)
#' price <- price_geometric_asian_transient(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda_P = 0.08, lambda_T = 0.02,
#'   alpha = 0.3, psi = 0.5,
#'   volumes = volumes
#' )
#'
#' @references
#' Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @seealso \code{\link{arithmetic_asian_bounds_transient}},
#'   \code{\link{price_geometric_asian}}
price_geometric_asian_transient <- function(S0, K, r, u, d,
                                             lambda_P, lambda_T,
                                             alpha, psi,
                                             volumes,
                                             option_type = "call",
                                             validate = TRUE) {

  if (validate) {
    validate_transient_inputs(S0, K, r, u, d, lambda_P, lambda_T,
                               alpha, psi, volumes)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  n <- length(volumes)

  if (n > 20) {
    warning(sprintf("n = %d is large for exact enumeration (2^%d = %d paths). Computation may be slow.",
                    n, n, 2^n))
  }

  result <- price_geometric_asian_transient_cpp(
    S0, K, r, u, d, lambda_P, lambda_T, alpha, psi,
    volumes, option_type
  )

  return(result)
}


#' Bounds for Arithmetic Asian Option with Transient Price Impact
#'
#' Computes upper and lower bounds for the arithmetic Asian option price using
#' the transient impact model.
#'
#' @inheritParams price_geometric_asian_transient
#' @param compute_path_specific Logical. If TRUE, computes tighter path-specific
#'   upper bound (requires enumeration of all 2^n paths). Default is FALSE.
#'
#' @details
#' Computes bounds for arithmetic Asian options under transient impact:
#' \itemize{
#'   \item Lower bound: Geometric Asian option price (AM-GM inequality)
#'   \item Upper bound (global): Uses worst-case spread parameter
#'   \item Upper bound (path-specific): Uses path-by-path spread (if requested)
#' }
#'
#' The path-specific bound is tighter but requires full path enumeration.
#'
#' @return List containing:
#' \describe{
#'   \item{lower_bound}{Lower bound (geometric Asian price)}
#'   \item{upper_bound_global}{Global upper bound}
#'   \item{upper_bound_path_specific}{Path-specific upper bound (if computed)}
#'   \item{rho_star}{Global spread parameter}
#'   \item{EQ_G}{Expected geometric average}
#'   \item{V0_G}{Geometric Asian price}
#'   \item{n_paths_used}{Number of paths enumerated}
#' }
#'
#' @export
#'
#' @examples
#' # Basic bounds with constant volumes
#' volumes <- rep(1, 10)
#' bounds <- arithmetic_asian_bounds_transient(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda_P = 0.05, lambda_T = 0.05,
#'   alpha = 0.5, psi = 1,
#'   volumes = volumes
#' )
#' print(bounds)
#'
#' # With path-specific bound (tighter but more expensive)
#' volumes <- rep(1, 8)
#' bounds_ps <- arithmetic_asian_bounds_transient(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda_P = 0.05, lambda_T = 0.05,
#'   alpha = 0.5, psi = 1,
#'   volumes = volumes,
#'   compute_path_specific = TRUE
#' )
#'
#' @seealso \code{\link{price_geometric_asian_transient}}
arithmetic_asian_bounds_transient <- function(S0, K, r, u, d,
                                               lambda_P, lambda_T,
                                               alpha, psi,
                                               volumes,
                                               option_type = "call",
                                               compute_path_specific = FALSE,
                                               validate = TRUE) {

  if (validate) {
    validate_transient_inputs(S0, K, r, u, d, lambda_P, lambda_T,
                               alpha, psi, volumes)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  n <- length(volumes)

  if (n > 20 && compute_path_specific) {
    warning("n > 20 with compute_path_specific = TRUE will be very slow. Consider setting to FALSE.")
  }

  result <- arithmetic_asian_bounds_transient_cpp(
    S0, K, r, u, d, lambda_P, lambda_T, alpha, psi,
    volumes, compute_path_specific, option_type
  )

  class(result) <- c("arithmetic_bounds_transient", "list")

  return(result)
}


#' @export
print.arithmetic_bounds_transient <- function(x, ...) {
  cat("Arithmetic Asian Option Bounds (Transient Impact)\n")
  cat("==================================================\n")
  cat(sprintf("Lower bound (V0_G):        %.6f\n", x$lower_bound))
  cat(sprintf("Upper bound (global):      %.6f\n", x$upper_bound_global))

  if (!is.na(x$upper_bound_path_specific)) {
    cat(sprintf("Upper bound (path-spec):   %.6f\n", x$upper_bound_path_specific))
    cat(sprintf("  (using all %d paths)\n", x$n_paths_used))
    cat(sprintf("Midpoint (path-spec):      %.6f\n",
                mean(c(x$lower_bound, x$upper_bound_path_specific))))
  } else {
    cat(sprintf("Midpoint (global):         %.6f\n",
                mean(c(x$lower_bound, x$upper_bound_global))))
  }

  cat(sprintf("Spread (rho*):             %.6f\n", x$rho_star))
  cat(sprintf("E^Q[G_n]:                  %.6f\n", x$EQ_G))

  invisible(x)
}
