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
#'   upper bound by sampling paths. Default is FALSE.
#' @param max_sample_size Integer. Maximum number of paths to sample when computing
#'   path-specific bound. Default is 100000.
#' @param sample_fraction Numeric. Fraction of total paths to sample (between 0 and 1).
#'   Default is 0.1 (10\%).
#' @param sampling_method Character. Sampling method for path-specific bound:
#'   "uniform" (default) or "truncated_normal". Truncated normal uses mean = (2^n-1)/2
#'   and sd = sqrt(n), focusing samples around middle paths.
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
#' For large \eqn{n}, the path-specific bound is estimated via random sampling
#' of paths to maintain computational efficiency.
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
#'   \item{n_paths_sampled}{Number of paths sampled for path-specific bound (0 if not computed)}
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
#' # Compute with path-specific bound (uniform sampling)
#' bounds_ps <- arithmetic_asian_bounds(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 5,
#'   compute_path_specific = TRUE
#' )
#'
#' print(bounds_ps)
#'
#' # Compute with path-specific bound using truncated normal sampling
#' bounds_tn <- arithmetic_asian_bounds(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 5,
#'   compute_path_specific = TRUE,
#'   sampling_method = "truncated_normal"
#' )
#'
#' print(bounds_tn)
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
                                     max_sample_size = 100000,
                                     sample_fraction = 0.1,
                                     sampling_method = "uniform",
                                     validate = TRUE) {
  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  option_type <- match.arg(option_type, c("call", "put"))
  sampling_method <- match.arg(sampling_method, c("uniform", "truncated_normal"))

  if (!is.logical(compute_path_specific)) {
    stop("compute_path_specific must be TRUE or FALSE")
  }

  if (max_sample_size < 1) {
    stop("max_sample_size must be at least 1")
  }

  if (sample_fraction <= 0 || sample_fraction > 1) {
    stop("sample_fraction must be between 0 and 1")
  }

  # Prepare sampled indices for truncated normal sampling
  sampled_indices <- NULL

  if (compute_path_specific && sampling_method == "truncated_normal") {
    # Check if truncnorm package is available
    if (!requireNamespace("truncnorm", quietly = TRUE)) {
      stop("Package 'truncnorm' is required for truncated_normal sampling.\n",
           "Install it with: install.packages('truncnorm')")
    }

    total_paths <- 2^n
    desired_sample <- min(max_sample_size, floor(sample_fraction * total_paths))
    desired_sample <- max(1, desired_sample)

    # Only sample if we don't enumerate all paths
    if (desired_sample < total_paths) {
      # Truncated normal parameters: mean = 2^n / 2, sd = sqrt(n)
      mean_val <- total_paths / 2
      sd_val <- sqrt(n)
      lower_bound <- 0
      upper_bound <- total_paths - 1

      # Sample until we have enough unique indices
      # For small sd relative to range, we may need many iterations
      sampled_indices <- integer(0)
      max_iterations <- 100
      iteration <- 0

      while (length(sampled_indices) < desired_sample && iteration < max_iterations) {
        # Sample more points (oversample significantly due to potential duplicates)
        n_needed <- desired_sample - length(sampled_indices)
        oversample_factor <- max(10, ceiling(100 / (sd_val + 1)))
        n_to_sample <- min(n_needed * oversample_factor, total_paths)

        samples <- truncnorm::rtruncnorm(
          n = n_to_sample,
          a = lower_bound,
          b = upper_bound,
          mean = mean_val,
          sd = sd_val
        )

        # Round to integers
        new_indices <- round(samples)

        # Combine with existing and keep unique
        sampled_indices <- unique(c(sampled_indices, new_indices))

        # Ensure within bounds
        sampled_indices <- sampled_indices[sampled_indices >= 0 &
                                             sampled_indices < total_paths]

        iteration <- iteration + 1
      }

      # Take only the requested number of samples
      if (length(sampled_indices) > desired_sample) {
        sampled_indices <- sampled_indices[1:desired_sample]
      }

      # If we still don't have enough (very rare), fill with uniform samples
      if (length(sampled_indices) < desired_sample) {
        n_more <- desired_sample - length(sampled_indices)
        all_indices <- 0:(total_paths - 1)
        available <- setdiff(all_indices, sampled_indices)
        additional <- sample(available, size = min(n_more, length(available)))
        sampled_indices <- c(sampled_indices, additional)
      }

      # Convert to integer vector (0-indexed for C++)
      sampled_indices <- as.integer(sampled_indices)
    }
  }

  # Call C++ function
  result <- arithmetic_asian_bounds_extended_cpp(
    S0, K, r, u, d, lambda, v_u, v_d, n,
    compute_path_specific, max_sample_size, sample_fraction, option_type,
    sampled_indices
  )

  # Add metadata
  result$upper_bound <- result$upper_bound_global
  result$sampling_method <- sampling_method

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
    cat(sprintf("  (sampled %d paths)\n", x$n_paths_sampled))
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
