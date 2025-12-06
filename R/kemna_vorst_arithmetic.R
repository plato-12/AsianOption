#' Kemna-Vorst Arithmetic Average Asian Option (Monte Carlo)
#'
#' Calculates the price of an arithmetic average Asian option using Monte Carlo
#' simulation with variance reduction via the geometric average control variate.
#' This implements the Kemna & Vorst (1990) method WITHOUT price impact.
#'
#' @param S0 Numeric. Initial stock price at time T0 (start of averaging period).
#'   Must be positive.
#' @param K Numeric. Strike price. Must be positive.
#' @param r Numeric. Continuously compounded risk-free rate (e.g., 0.05 for 5\%).
#'   Use \code{log(r_gross)} to convert from gross rate.
#' @param sigma Numeric. Volatility (annualized standard deviation). Must be
#'   non-negative.
#' @param T0 Numeric. Start time of averaging period. Must be non-negative.
#' @param T Numeric. Maturity time. Must be greater than T0.
#' @param n Integer. Number of averaging points (observations). Must be positive.
#' @param M Integer. Number of Monte Carlo simulations. Default is 10000.
#'   Larger values give more accurate results but take longer.
#' @param option_type Character. Type of option: "call" (default) or "put".
#' @param use_control_variate Logical. If TRUE (default), uses the geometric
#'   average as a control variate for variance reduction. This dramatically
#'   improves accuracy.
#' @param seed Integer. Random seed for reproducibility. Default is NULL (no seed).
#' @param return_diagnostics Logical. If TRUE, returns additional diagnostic
#'   information including confidence intervals, correlation, and variance
#'   reduction factor. Default is FALSE.
#'
#' @return If \code{return_diagnostics = FALSE}, returns a numeric value (the
#'   estimated option price). If \code{return_diagnostics = TRUE}, returns a list with components:
#'   \describe{
#'     \item{price}{Estimated option price}
#'     \item{std_error}{Standard error of the estimate}
#'     \item{lower_ci}{Lower 95\% confidence interval}
#'     \item{upper_ci}{Upper 95\% confidence interval}
#'     \item{geometric_price}{Analytical geometric average price (control variate)}
#'     \item{correlation}{Correlation between arithmetic and geometric payoffs}
#'     \item{variance_reduction_factor}{Ratio of variances (with/without control)}
#'     \item{n_simulations}{Number of Monte Carlo simulations used}
#'     \item{n_steps}{Number of time steps in each simulation}
#'   }
#'
#' @details
#' The arithmetic average at maturity is:
#' \deqn{A_T = \frac{1}{n+1} \sum_{i=0}^{n} S(T_i)}
#'
#' The payoff for a call option is:
#' \deqn{\max(A_T - K, 0)}
#'
#' Unlike the geometric average, there is no closed-form solution for the
#' arithmetic average Asian option. The Kemna-Vorst method uses Monte Carlo
#' simulation with a powerful variance reduction technique:
#'
#' \strong{Control Variate Method:}
#' \enumerate{
#'   \item For each simulated path, calculate both arithmetic average \eqn{A}
#'         and geometric average \eqn{G}
#'   \item Calculate discounted payoffs: \eqn{Y = e^{-r\tau}\max(A-K,0)} and
#'         \eqn{W = e^{-r\tau}\max(G-K,0)}
#'   \item The enhanced estimate is:
#'         \deqn{\hat{C} = E[W] + \frac{1}{M}\sum_{j=1}^{M}(Y_j - W_j)}
#'         where \eqn{E[W]} is the analytical geometric average price
#' }
#'
#' \strong{Why This Works:}
#' The correlation between arithmetic and geometric averages is typically
#' very high (> 0.95), which means the difference \eqn{Y - W} has much lower
#' variance than \eqn{Y} alone. The variance reduction factor is:
#' \deqn{\text{Var}(\hat{C}_{enhanced}) = \text{Var}(\hat{C}_{standard}) \times (1 - \rho^2)}
#'
#' With \eqn{\rho \approx 0.95}, this gives a variance reduction of factor 10-20,
#' or equivalently, requires 10-20 times fewer simulations to achieve the same
#' accuracy.
#'
#' \strong{Recommended Settings:}
#' \itemize{
#'   \item \code{M = 10000}: Good balance of speed and accuracy
#'   \item \code{M = 100000}: High accuracy for final pricing
#'   \item \code{n = 50-250}: Typical for daily/weekly averaging over several months
#'   \item Always use \code{use_control_variate = TRUE} (default)
#' }
#'
#' @section Note on Risk-Free Rate:
#' This function expects \code{r} as a \strong{continuously compounded rate}
#' (e.g., \code{r = 0.05} for 5\% per annum), which is standard for continuous-time
#' models like Black-Scholes.
#'
#' If you have a gross rate (e.g., \code{r_gross = 1.05}), convert it using:
#' \code{r = log(r_gross)}.
#'
#' @references
#' Kemna, A.G.Z. and Vorst, A.C.F. (1990). "A Pricing Method for Options Based
#' on Average Asset Values." \emph{Journal of Banking and Finance}, 14, 113-129.
#'
#' @examples
#' # Basic example: at-the-money call option
#' price_kemna_vorst_arithmetic(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'   T0 = 0, T = 1, n = 50, M = 10000
#' )
#'
#' # With diagnostic information
#' result <- price_kemna_vorst_arithmetic(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'   T0 = 0, T = 1, n = 50, M = 10000,
#'   return_diagnostics = TRUE, seed = 123
#' )
#' print(result)
#' cat("Price:", result$price, "± ", 1.96 * result$std_error, "\n")
#' cat("Correlation:", result$correlation, "\n")
#' cat("Variance reduction factor:", result$variance_reduction_factor, "\n")
#'
#' # Compare with and without variance reduction
#' \donttest{
#' with_control <- price_kemna_vorst_arithmetic(
#'   100, 100, 0.05, 0.2, 0, 1, 50, 10000,
#'   use_control_variate = TRUE, return_diagnostics = TRUE, seed = 123
#' )
#' without_control <- price_kemna_vorst_arithmetic(
#'   100, 100, 0.05, 0.2, 0, 1, 50, 10000,
#'   use_control_variate = FALSE, return_diagnostics = TRUE, seed = 123
#' )
#' cat("With control:    SE =", with_control$std_error, "\n")
#' cat("Without control: SE =", without_control$std_error, "\n")
#' cat("Improvement factor:", without_control$std_error / with_control$std_error, "\n")
#' }
#'
#' # Out-of-the-money option
#' price_kemna_vorst_arithmetic(100, 120, 0.05, 0.2, 0, 1, 50, 10000)
#'
#' # Put option
#' price_kemna_vorst_arithmetic(100, 100, 0.05, 0.2, 0, 1, 50, 10000,
#'                               option_type = "put")
#'
#' @export
price_kemna_vorst_arithmetic <- function(S0, K, r, sigma, T0, T, n, M = 10000,
                                          option_type = "call",
                                          use_control_variate = TRUE,
                                          seed = NULL,
                                          return_diagnostics = FALSE) {

  if (!is.numeric(S0) || length(S0) != 1 || S0 <= 0) {
    stop("S0 must be a positive number")
  }
  if (!is.numeric(K) || length(K) != 1 || K <= 0) {
    stop("K must be a positive number")
  }
  if (!is.numeric(r) || length(r) != 1) {
    stop("r must be a number")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma < 0) {
    stop("sigma must be a non-negative number")
  }
  if (!is.numeric(T0) || length(T0) != 1 || T0 < 0) {
    stop("T0 must be a non-negative number")
  }
  if (!is.numeric(T) || length(T) != 1 || T <= T0) {
    stop("T must be greater than T0")
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  if (!is.numeric(M) || length(M) != 1 || M < 1 || M != as.integer(M)) {
    stop("M must be a positive integer")
  }
  option_type <- match.arg(option_type, c("call", "put"))
  if (!is.logical(use_control_variate) || length(use_control_variate) != 1) {
    stop("use_control_variate must be TRUE or FALSE")
  }
  if (!is.logical(return_diagnostics) || length(return_diagnostics) != 1) {
    stop("return_diagnostics must be TRUE or FALSE")
  }

  seed_value <- if (is.null(seed)) 0L else as.integer(seed)

  if (M < 1000) {
    warning("M = ", M, " is very small. Results may be inaccurate. ",
            "Consider M >= 10000 for reliable estimates.")
  }

  result <- price_kemna_vorst_arithmetic_cpp(
    S0 = S0, K = K, r = r, sigma = sigma,
    T0 = T0, T = T, n = as.integer(n), M = as.integer(M),
    option_type = option_type,
    use_control_variate = use_control_variate,
    seed = seed_value
  )

  class(result) <- c("kemna_vorst_arithmetic", "list")

  if (return_diagnostics) {
    return(result)
  } else {
    return(result$price)
  }
}


#' Kemna-Vorst Arithmetic Average Asian Option (Binomial Parameters)
#'
#' Alternative interface using discrete binomial tree parameters instead of
#' continuous parameters. This provides compatibility with the binomial
#' tree convention used in the rest of the AsianOptPI package.
#'
#' @param S0 Numeric. Initial stock price. Must be positive.
#' @param K Numeric. Strike price. Must be positive.
#' @param r Numeric. Gross risk-free rate per period (e.g., 1.05 for 5\%).
#' @param u Numeric. Up factor in binomial tree. Must be > 1.
#' @param d Numeric. Down factor in binomial tree. Must be < 1 and < u.
#' @param n Integer. Number of time steps. Must be positive.
#' @param M Integer. Number of Monte Carlo simulations. Default is 10000.
#' @param option_type Character. Type of option: "call" (default) or "put".
#' @param use_control_variate Logical. Use variance reduction (default TRUE).
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#' @param return_diagnostics Logical. Return detailed diagnostics (default FALSE).
#'
#' @return Same as \code{price_kemna_vorst_arithmetic}.
#'
#' @details
#' This function converts binomial parameters to continuous parameters and
#' calls \code{price_kemna_vorst_arithmetic}.
#'
#' Conversion formulas:
#' \itemize{
#'   \item \eqn{r_{continuous} = \log(r_{gross})}
#'   \item \eqn{\sigma = \log(u/d) / (2\sqrt{\Delta t})} where \eqn{\Delta t = 1/n}
#'   \item \eqn{T_0 = 0}, \eqn{T = 1}
#' }
#'
#' @examples
#' # Using binomial tree parameters
#' price_kemna_vorst_arithmetic_binomial(
#'   S0 = 100, K = 100, r = 1.05,
#'   u = 1.2, d = 0.8, n = 10, M = 10000
#' )
#'
#' # With diagnostics
#' result <- price_kemna_vorst_arithmetic_binomial(
#'   S0 = 100, K = 100, r = 1.05,
#'   u = 1.2, d = 0.8, n = 10, M = 10000,
#'   return_diagnostics = TRUE, seed = 123
#' )
#' print(result)
#'
#' @export
price_kemna_vorst_arithmetic_binomial <- function(S0, K, r, u, d, n, M = 10000,
                                                    option_type = "call",
                                                    use_control_variate = TRUE,
                                                    seed = NULL,
                                                    return_diagnostics = FALSE) {

  if (!is.numeric(u) || length(u) != 1 || u <= 1) {
    stop("u must be greater than 1")
  }
  if (!is.numeric(d) || length(d) != 1 || d >= 1 || d >= u) {
    stop("d must be less than 1 and less than u")
  }

  r_continuous <- log(r)

  dt <- 1 / n

  sigma <- log(u / d) / (2 * sqrt(dt))

  T0 <- 0
  T <- 1

  price_kemna_vorst_arithmetic(
    S0 = S0, K = K, r = r_continuous, sigma = sigma,
    T0 = T0, T = T, n = n, M = M,
    option_type = option_type,
    use_control_variate = use_control_variate,
    seed = seed,
    return_diagnostics = return_diagnostics
  )
}


#' Print Method for Kemna-Vorst Arithmetic Results
#'
#' @param x Object of class "kemna_vorst_arithmetic"
#' @param ... Additional arguments (ignored)
#'
#' @export
print.kemna_vorst_arithmetic <- function(x, ...) {
  cat("Kemna-Vorst Arithmetic Asian Option (Monte Carlo)\n")
  cat("==================================================\n\n")

  cat(sprintf("Estimated Price:     %.6f\n", x$price))
  cat(sprintf("Standard Error:      %.6f\n", x$std_error))
  cat(sprintf("95%% CI:              [%.6f, %.6f]\n", x$lower_ci, x$upper_ci))
  cat("\n")

  cat(sprintf("Geometric Price (control):  %.6f\n", x$geometric_price))
  cat(sprintf("Correlation (A,G):          %.4f\n", x$correlation))
  cat(sprintf("Variance Reduction Factor:  %.4f (%.1fx improvement)\n",
              x$variance_reduction_factor,
              1 / x$variance_reduction_factor))
  cat("\n")

  cat(sprintf("Simulations:         %d\n", x$n_simulations))
  cat(sprintf("Time Steps:          %d\n", x$n_steps))

  invisible(x)
}


#' Summary Method for Kemna-Vorst Arithmetic Results
#'
#' @param object Object of class "kemna_vorst_arithmetic"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.kemna_vorst_arithmetic <- function(object, ...) {
  print(object)
  invisible(object)
}
