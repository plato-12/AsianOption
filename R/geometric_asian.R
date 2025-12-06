#' Price Geometric Asian Option with Price Impact
#'
#' Computes the price of a geometric Asian option (call or put) using the
#' Cox-Ross-Rubinstein (CRR) binomial model with price impact from
#' hedging activities. Automatically selects between exact enumeration (n <= 20)
#' and Monte Carlo simulation (n > 20).
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer)
#' @param option_type Character; either "call" (default) or "put"
#' @param validate Logical; if TRUE, performs input validation
#' @param method Character; "auto" (default), "exact", or "mc". Auto selects
#'   exact for n <= 20, Monte Carlo otherwise
#' @param n_simulations Number of Monte Carlo simulations (default: 100000).
#'   Only used when method="mc" or auto-selected
#' @param seed Random seed for Monte Carlo (NULL for no seed)
#'
#' @details
#' The geometric Asian option payoff is:
#' \itemize{
#'   \item Call: \eqn{V_n = \max(0, G_n - K)}
#'   \item Put: \eqn{V_n = \max(0, K - G_n)}
#' }
#' where \eqn{G_n = (S_0 \cdot S_1 \cdot \ldots \cdot S_n)^{1/(n+1)}}
#'
#' Price impact modifies the stock dynamics:
#' \itemize{
#'   \item Effective up factor: \eqn{\tilde{u} = u \cdot e^{\lambda v^u}}
#'   \item Effective down factor: \eqn{\tilde{d} = d \cdot e^{-\lambda v^d}}
#'   \item Risk-neutral probability: \eqn{p^{eff} = \frac{r - \tilde{d}}{\tilde{u} - \tilde{d}}}
#' }
#'
#' **Method Selection**:
#' \itemize{
#'   \item \strong{Exact} (n <= 20): Enumerates all \eqn{2^n} paths for exact pricing
#'   \item \strong{Monte Carlo} (n > 20): Simulates paths for efficient estimation
#'   \item \strong{Auto} (default): Chooses automatically based on n
#' }
#'
#' For n > 20, exact enumeration requires > 1 million paths and becomes slow.
#' Monte Carlo provides fast, accurate estimates with quantifiable error bounds.
#'
#' @return Geometric Asian option price (numeric). When using Monte Carlo,
#'   only the price is returned; use \code{\link{price_geometric_asian_mc}} directly
#'   for full MC output including standard error and confidence intervals.
#' @export
#'
#' @examples
#' # Small n: automatic exact method
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0, v_u = 0, v_d = 0, n = 10
#' )
#'
#' # Large n: automatic Monte Carlo
#' \donttest{
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 30
#' )
#' }
#'
#' # Force exact method
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 5, method = "exact"
#' )
#'
#' # Force Monte Carlo with custom parameters
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 15,
#'   method = "mc", n_simulations = 50000, seed = 123
#' )
#'
#' @references
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#'
#' @seealso \code{\link{arithmetic_asian_bounds}}, \code{\link{compute_p_adj}},
#'   \code{\link{price_geometric_asian_mc}}
price_geometric_asian <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                   option_type = "call",
                                   validate = TRUE,
                                   method = "auto",
                                   n_simulations = 100000,
                                   seed = NULL) {

  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  method <- match.arg(method, c("auto", "exact", "mc"))

  if (method == "auto") {
    if (n <= 20) {
      method <- "exact"
    } else {
      method <- "mc"
      message(sprintf("Using Monte Carlo method for n=%d (> 20) with %d simulations",
                     n, n_simulations))
    }
  }

  if (method == "exact") {
    if (n > 20) {
      warning(sprintf("Using exact method for n=%d will enumerate 2^%d = %d paths. This may be slow.",
                     n, n, 2^n))
    }
    result <- price_geometric_asian_cpp(S0, K, r, u, d, lambda, v_u, v_d, n, option_type)
  } else {
    mc_result <- price_geometric_asian_mc(
      S0 = S0, K = K, r = r, u = u, d = d,
      lambda = lambda, v_u = v_u, v_d = v_d, n = n,
      n_simulations = n_simulations,
      option_type = option_type,
      seed = seed,
      validate = FALSE
    )
    result <- mc_result$price
  }

  return(result)
}

#' Price Geometric Asian Option using Monte Carlo Simulation
#'
#' Estimates the price of a geometric Asian option using Monte Carlo simulation.
#' This method is efficient for large n (> 20) where exact enumeration is
#' computationally prohibitive.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer)
#' @param n_simulations Number of Monte Carlo paths (default: 100000)
#' @param option_type Character; either "call" (default) or "put"
#' @param seed Random seed for reproducibility (NULL for no seed)
#' @param validate Logical; if TRUE, performs input validation
#'
#' @details
#' Monte Carlo simulation randomly samples price paths according to the
#' risk-neutral probability. For each path:
#' \itemize{
#'   \item Generate random up/down moves using Bernoulli(p_adj)
#'   \item Compute geometric average: \eqn{G = (S_0 \cdot \ldots \cdot S_n)^{1/(n+1)}}
#'   \item Calculate payoff: \eqn{\max(0, G - K)} for calls
#' }
#'
#' The price is estimated as the mean of discounted payoffs. Standard error
#' indicates the accuracy of the estimate.
#'
#' **Convergence**: Standard error decreases as \eqn{1/\sqrt{n_{sim}}}, so
#' 100,000 simulations typically give errors < 0.5\%.
#'
#' **When to use**:
#' \itemize{
#'   \item n > 20: Exact method requires 2^n paths (> 1 million)
#'   \item Large n (e.g., n=50): Only MC is feasible
#'   \item Quick estimates: Use fewer simulations (e.g., 10,000)
#' }
#'
#' @return A list with class "geometric_asian_mc" containing:
#' \itemize{
#'   \item \code{price}: Estimated option price
#'   \item \code{std_error}: Standard error of the estimate
#'   \item \code{n_simulations}: Number of simulations used
#'   \item \code{confidence_interval}: 95\% confidence interval
#'   \item \code{method}: "Monte Carlo"
#' }
#'
#' @export
#'
#' @examples
#' # Monte Carlo for large n
#' result <- price_geometric_asian_mc(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 25,
#'   n_simulations = 50000, seed = 42
#' )
#' print(result)
#'
#' # Compare with exact method for small n
#' \donttest{
#' exact <- price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10, method = "exact"
#' )
#' mc <- price_geometric_asian_mc(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10,
#'   n_simulations = 100000, seed = 42
#' )
#' cat("Exact:", exact, "\n")
#' cat("MC:", mc$price, "±", 1.96 * mc$std_error, "\n")
#' }
#'
#' @references
#' Glasserman, P. (2003). Monte Carlo Methods in Financial Engineering.
#' Springer.
#'
#' @seealso \code{\link{price_geometric_asian}}, \code{\link{arithmetic_asian_bounds}}
price_geometric_asian_mc <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                      n_simulations = 100000,
                                      option_type = "call",
                                      seed = NULL,
                                      validate = TRUE) {

  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)

    if (!is.numeric(n_simulations) || n_simulations <= 0 || n_simulations != as.integer(n_simulations)) {
      stop("n_simulations must be a positive integer")
    }

    if (!is.null(seed) && (!is.numeric(seed) || seed < 0)) {
      stop("seed must be NULL or a non-negative integer")
    }
  }

  option_type <- match.arg(option_type, c("call", "put"))

  seed_val <- if (is.null(seed)) -1L else as.integer(seed)

  result <- price_geometric_asian_mc_cpp(
    S0 = S0, K = K, r = r, u = u, d = d,
    lambda = lambda, v_u = v_u, v_d = v_d, n = n,
    n_simulations = as.integer(n_simulations),
    option_type = option_type,
    seed = seed_val
  )

  ci_margin <- 1.96 * result$std_error
  result$confidence_interval <- c(
    lower = result$price - ci_margin,
    upper = result$price + ci_margin
  )

  result$method <- "Monte Carlo"

  class(result) <- "geometric_asian_mc"

  return(result)
}

#' Print method for geometric_asian_mc objects
#'
#' @param x A geometric_asian_mc object
#' @param ... Additional arguments (not used)
#' @export
print.geometric_asian_mc <- function(x, ...) {
  cat("Geometric Asian Option Price (Monte Carlo)\n")
  cat("==========================================\n")
  cat(sprintf("Price:       %.6f\n", x$price))
  cat(sprintf("Std Error:   %.6f (%.2f%%)\n", x$std_error, 100 * x$std_error / x$price))
  cat(sprintf("95%% CI:      [%.6f, %.6f]\n", x$confidence_interval[1], x$confidence_interval[2]))
  cat(sprintf("Simulations: %d\n", x$n_simulations))
  invisible(x)
}
