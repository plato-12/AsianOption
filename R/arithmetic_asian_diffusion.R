#' Arithmetic Asian Option Price via Euler-Maruyama Monte Carlo (Exogenous Diffusion)
#'
#' Prices an arithmetic Asian option under exogenous transient price impact
#' using Euler-Maruyama Monte Carlo simulation.
#'
#' @param S0 Initial stock price (positive).
#' @param K Strike price (positive).
#' @param r Risk-free rate (positive).
#' @param sigma Volatility parameter (positive).
#' @param T Time to maturity (positive).
#' @param lambda_T Transient impact coefficient (non-negative).
#' @param I0 Initial transient impact state (real number).
#' @param kappa Mean reversion rate for transient impact (positive).
#' @param eta Noise amplitude for transient impact process. Can be:
#'   - A single non-negative number (constant eta)
#'   - A function of time t in [0,T] returning a non-negative value
#' @param rho Correlation between stock and impact Brownian motions (in [-1,1]).
#'   Default is 0.
#' @param option_type Character string: "call" (default) or "put".
#' @param n_steps Number of time steps in the Euler-Maruyama discretisation
#'   (default: 252).
#' @param n_sims Number of Monte Carlo simulation paths (default: 100000).
#' @param use_control_variate Logical. If TRUE (default), uses the geometric
#'   Asian diffusion closed-form price as a control variate to reduce variance.
#' @param seed Integer seed for reproducibility. 0 means no seed (default: 0).
#' @param n_quad Number of quadrature points for the geometric closed-form
#'   computation when using control variates (default: 1000).
#'
#' @return An object of class \code{"arithmetic_asian_diffusion"} (a list) with:
#'   \describe{
#'     \item{price}{Estimated option price.}
#'     \item{std_error}{Standard error of the estimate.}
#'     \item{lower_ci}{Lower bound of 95\% confidence interval.}
#'     \item{upper_ci}{Upper bound of 95\% confidence interval.}
#'     \item{geometric_price}{Closed-form geometric Asian price (benchmark).}
#'     \item{correlation}{Correlation between arithmetic and geometric MC payoffs.}
#'     \item{use_control_variate}{Whether control variate was used.}
#'     \item{n_sims}{Number of simulations.}
#'     \item{n_steps}{Number of time steps.}
#'   }
#'
#' @details
#' In the exogenous regime with no active trading (\eqn{\nu \equiv 0}), the
#' stock price dynamics are:
#'
#' \deqn{dS_t = S_t (r + \bar{\lambda}_T I_t)\, dt + \sigma S_t\, dW_t}
#' \deqn{dI_t = -\kappa I_t\, dt + \eta(t)\, dW^I_t}
#'
#' where \eqn{W} and \eqn{W^I} are Brownian motions with instantaneous
#' correlation \eqn{\rho}.
#'
#' The arithmetic Asian payoff is \eqn{\Phi_A(A_T) = (A_T - K)^+}, where the
#' average is **discretely monitored** over the \eqn{N + 1} fixings
#' \eqn{t_m = m \Delta t}, \eqn{m = 0, \ldots, N}:
#'
#' \deqn{A_T = \frac{1}{N + 1} \sum_{m = 0}^{N} S_{t_m}.}
#'
#' Both the initial fixing \eqn{S_0} and the terminal fixing \eqn{S_T} are
#' included. This is the same convention used by the geometric control-variate
#' leg of this routine and by \code{\link{price_kemna_vorst_arithmetic}}, so
#' the two are directly comparable; as \eqn{N \to \infty} it converges to the
#' continuously monitored average \eqn{Y_T/T} with \eqn{Y_t = \int_0^t S_u\, du}.
#'
#' The Euler-Maruyama scheme discretises the SDEs on a uniform grid with step
#' \eqn{\Delta t = T/N}. The log-Euler method is used for \eqn{S} to ensure
#' positivity.
#'
#' When \code{use_control_variate = TRUE}, the geometric Asian diffusion
#' closed-form from \code{\link{price_geometric_asian_diffusion}} is used as
#' a control variate, which typically reduces the standard error substantially.
#'
#' The price shock \eqn{W} and the impact shock \eqn{W^I} are drawn from two
#' separate random streams, both reproducible from \code{seed}. The price
#' stream is therefore exactly \code{n_steps} draws per path, in path order,
#' whatever the impact parameters are, and two things follow that callers
#' depend on:
#' \itemize{
#'   \item At \code{lambda_T = 0} the price shocks are the same draws, in the
#'     same order, that \code{\link{price_kemna_vorst_arithmetic}} takes, so
#'     under a common seed the two routines walk the same paths and their
#'     prices agree to floating-point rounding. The measured price impact at
#'     \code{lambda_T = 0} is exactly zero rather than a residual discrepancy.
#'   \item Across a sweep in \code{lambda_T} the price shocks are unchanged, so
#'     a difference of two prices in the sweep is a common-random-numbers
#'     difference and the Monte Carlo error in it is an order of magnitude
#'     below the error in either level.
#' }
#'
#' Interleaving the two shocks in a single stream gives up the first property;
#' drawing the impact shock only when it can matter gives up the second. Only
#' separate streams give both.
#'
#' @references
#' Section 3.2.1 of "Asian option valuation under price impact"
#'
#' @seealso \code{\link{price_geometric_asian_diffusion}} for the geometric
#'   Asian closed-form in the same diffusion limit.
#'
#' @examples
#' # Basic call pricing
#' price_arithmetic_asian_diffusion(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
#'   lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0,
#'   n_steps = 100, n_sims = 10000, seed = 42
#' )
#'
#' # With time-dependent eta
#' eta_func <- function(t) 0.1 * (1 + 0.5 * t)
#' price_arithmetic_asian_diffusion(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
#'   lambda_T = 0.01, I0 = 0.5, kappa = 2, eta = eta_func, rho = 0.3,
#'   n_steps = 100, n_sims = 10000, seed = 42
#' )
#'
#' @export
price_arithmetic_asian_diffusion <- function(S0, K, r, sigma, T,
                                             lambda_T, I0, kappa,
                                             eta, rho = 0,
                                             option_type = "call",
                                             n_steps = 252,
                                             n_sims = 100000,
                                             use_control_variate = TRUE,
                                             seed = 0,
                                             n_quad = 1000) {
  if (S0 <= 0) stop("S0 must be positive")
  if (K <= 0) stop("K must be positive")
  if (r <= 0) stop("r must be positive")
  if (sigma <= 0) stop("sigma must be positive")
  if (T <= 0) stop("T must be positive")
  if (lambda_T < 0) stop("lambda_T must be non-negative")
  if (kappa <= 0) stop("kappa must be positive")
  if (abs(rho) > 1) stop("rho must be in [-1, 1]")
  if (!(option_type %in% c("call", "put"))) {
    stop("option_type must be 'call' or 'put'")
  }
  if (n_steps < 1) stop("n_steps must be at least 1")
  if (n_sims < 1) stop("n_sims must be at least 1")

  dt <- T / n_steps
  t_grid <- seq(0, T - dt, length.out = n_steps)  # left endpoints [0, dt, ..., T-dt]

  if (is.function(eta)) {
    eta_values <- eta(t_grid)
    if (any(eta_values < 0)) {
      stop("eta(t) must be non-negative for all t in [0,T]")
    }
  } else {
    if (eta < 0) stop("eta must be non-negative")
    eta_values <- rep(eta, n_steps)
  }

  mc_result <- price_arithmetic_asian_diffusion_mc_cpp(
    S0 = S0, K = K, r = r, sigma = sigma,
    T_mat = T, lambda_T = lambda_T, I0 = I0, kappa = kappa,
    eta_values = eta_values, rho = rho,
    n_steps = n_steps, n_sims = n_sims,
    option_type = option_type,
    use_control_variate = use_control_variate,
    seed = seed
  )

  geom_closed_form <- price_geometric_asian_diffusion(
    S0 = S0, K = K, r = r, sigma = sigma, T = T,
    lambda_T = lambda_T, I0 = I0, kappa = kappa,
    eta = eta, rho = rho,
    option_type = option_type,
    n_quad = n_quad
  )

  if (use_control_variate) {
    price <- mc_result$price_estimate + geom_closed_form
    std_error <- mc_result$std_error
  } else {
    price <- mc_result$price_estimate
    std_error <- mc_result$std_error
  }

  price <- max(price, 0)

  ci_margin <- 1.96 * std_error

  result <- list(
    price = price,
    std_error = std_error,
    lower_ci = max(price - ci_margin, 0),
    upper_ci = price + ci_margin,
    geometric_price = geom_closed_form,
    correlation = mc_result$correlation,
    use_control_variate = use_control_variate,
    n_sims = n_sims,
    n_steps = n_steps,
    option_type = option_type
  )

  class(result) <- "arithmetic_asian_diffusion"
  return(result)
}


#' @export
print.arithmetic_asian_diffusion <- function(x, ...) {
  cat("Arithmetic Asian Option (Exogenous Diffusion, Euler-Maruyama MC)\n")
  cat("================================================================\n")
  cat(sprintf("  Option type:       %s\n", x$option_type))
  cat(sprintf("  Price:             %.6f\n", x$price))
  cat(sprintf("  Std error:         %.6f\n", x$std_error))
  cat(sprintf("  95%% CI:           [%.6f, %.6f]\n", x$lower_ci, x$upper_ci))
  cat(sprintf("  Geometric price:   %.6f (closed-form benchmark)\n", x$geometric_price))
  cat(sprintf("  Correlation:       %.4f\n", x$correlation))
  cat(sprintf("  Control variate:   %s\n", ifelse(x$use_control_variate, "yes", "no")))
  cat(sprintf("  Simulations:       %d\n", x$n_sims))
  cat(sprintf("  Time steps:        %d\n", x$n_steps))
  invisible(x)
}
