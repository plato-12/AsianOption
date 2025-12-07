#' Bounds for Arithmetic Asian Option with Price Impact
#'
#' Computes lower and upper bounds for the arithmetic Asian option (call or put)
#' using the relationship between arithmetic and geometric means (Jensen's
#' inequality).
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer, recommended n <= 20)
#' @param option_type Character; either "call" (default) or "put"
#' @param validate Logical; if TRUE, performs input validation (default TRUE)
#' @param compute_path_specific Logical. If TRUE, computes the tighter path-specific
#'   upper bound using exact enumeration of all 2^n paths. Default is FALSE.
#'
#' @details
#' The arithmetic Asian option has payoff:
#' \itemize{
#'   \item Call: \eqn{V_n = \max(0, A_n - K)}
#'   \item Put: \eqn{V_n = \max(0, K - A_n)}
#' }
#' where \eqn{A_n = \frac{1}{n+1}\sum_{i=0}^{n} S_i}
#'
#' Since \eqn{A_n \geq G_n} (AM-GM inequality), we have:
#' \deqn{V_0^A \geq V_0^G}
#'
#' **Global Upper Bound:**
#'
#' The global upper bound uses the worst-case spread parameter:
#' \deqn{V_0^A \leq V_0^G + \frac{(\rho^* - 1)}{r^n} \mathbb{E}^Q[G_n]}
#'
#' where \eqn{\rho^* = \exp\left[\frac{(\tilde{u}^n - \tilde{d}^n)^2}{4\tilde{u}^n\tilde{d}^n}\right]}
#'
#' **Path-Specific Upper Bound:**
#'
#' A tighter bound can be obtained by using path-specific \eqn{\rho(\omega)} values:
#' \deqn{V_0^A \leq V_0^G + \frac{1}{r^n}\sum_{\omega} P(\omega) \cdot (\rho(\omega) - 1) \cdot G_n(\omega)}
#'
#' where \eqn{\rho(\omega) = \exp\left[\frac{(S_M(\omega) - S_m(\omega))^2}{4 S_m(\omega) S_M(\omega)}\right]}
#' uses the actual min/max prices along each path. This bound satisfies
#' \eqn{V_0^G \leq V_0^A \leq} path-specific bound \eqn{\leq} global bound.
#'
#' The path-specific bound uses exact enumeration of all \eqn{2^n} paths
#' in the binomial tree (no sampling or approximation).
#'
#' @return List containing:
#' \describe{
#'   \item{lower_bound}{Lower bound for arithmetic option (= geometric option price)}
#'   \item{upper_bound}{Upper bound for arithmetic option (global bound, for backward compatibility)}
#'   \item{upper_bound_global}{Global upper bound using \eqn{\rho^*}}
#'   \item{upper_bound_path_specific}{Path-specific upper bound (only if compute_path_specific=TRUE, otherwise NA)}
#'   \item{rho_star}{Spread parameter \eqn{\rho^*}}
#'   \item{EQ_G}{Expected geometric average under risk-neutral measure}
#'   \item{V0_G}{Geometric Asian option price (same as lower_bound)}
#'   \item{n_paths_used}{Number of paths used for path-specific bound (2^n if computed, 0 otherwise)}
#' }
#'
#' @export
#'
#' @examples
#' # Compute basic bounds (global bound only) for call option
#' bounds <- arithmetic_asian_bounds(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 3, option_type = "call"
#' )
#'
#' print(bounds)
#'
#' # Compute bounds for put option
#' bounds_put <- arithmetic_asian_bounds(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 3, option_type = "put"
#' )
#'
#' # Compute with path-specific bound (uses exact enumeration of all 2^n paths)
#' bounds_ps <- arithmetic_asian_bounds(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 5,
#'   compute_path_specific = TRUE
#' )
#'
#' print(bounds_ps)
#'
#' # Estimate arithmetic option price as midpoint of path-specific bounds
#' if (!is.na(bounds_ps$upper_bound_path_specific)) {
#'   estimated_price <- mean(c(bounds_ps$lower_bound,
#'                             bounds_ps$upper_bound_path_specific))
#'   cat("Estimated price:", estimated_price, "\n")
#' }
#'
#' @references
#' Budimir, I., Dragomir, S. S., & Pecaric, J. (2000).
#' Further reverse results for Jensen's discrete inequality and
#' applications in information theory.
#' \emph{Journal of Inequalities in Pure and Applied Mathematics}, 2(1).
#'
#' @seealso \code{\link{price_geometric_asian}}
arithmetic_asian_bounds <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                     option_type = "call",
                                     compute_path_specific = FALSE,
                                     validate = TRUE) {
  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  if (!is.logical(compute_path_specific)) {
    stop("compute_path_specific must be TRUE or FALSE")
  }

  # Call C++ function with exact enumeration
  result <- arithmetic_asian_bounds_extended_cpp(
    S0, K, r, u, d, lambda, v_u, v_d, n,
    compute_path_specific, option_type
  )

  # Add metadata for backward compatibility
  result$upper_bound <- result$upper_bound_global

  class(result) <- c("arithmetic_bounds", "list")

  return(result)
}

#' Print Method for Arithmetic Asian Bounds
#'
#' @param x Object of class \code{arithmetic_bounds}
#' @param ... Additional arguments (unused)
#'
#' @return Invisible x
#' @export
print.arithmetic_bounds <- function(x, ...) {
  cat("Arithmetic Asian Option Bounds\n")
  cat("================================\n")
  cat(sprintf("Lower bound (V0_G):        %.6f\n", x$lower_bound))
  cat(sprintf("Upper bound (global):      %.6f\n", x$upper_bound_global))

  if (!is.null(x$upper_bound_path_specific) &&
        !is.na(x$upper_bound_path_specific)) {
    cat(sprintf("Upper bound (path-spec):   %.6f\n",
                x$upper_bound_path_specific))
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
