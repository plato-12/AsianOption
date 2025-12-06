#' Black-Scholes European Call Option Price
#'
#' Computes the exact price of a European call option using the classical
#' Black-Scholes (1973) analytical formula. This is the continuous-time
#' benchmark for comparison with discrete binomial models.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Continuously compounded risk-free rate (e.g., 0.05 for 5\% annual rate)
#' @param sigma Volatility (annualized standard deviation, must be non-negative)
#' @param time_to_maturity Time to maturity in years (must be positive)
#'
#' @return European call option price (numeric)
#'
#' @details
#' The Black-Scholes formula for a European call option is:
#' \deqn{C = S_0 N(d_1) - K e^{-rT} N(d_2)}
#'
#' where:
#' \deqn{d_1 = \frac{\log(S_0/K) + (r + \sigma^2/2)T}{\sigma\sqrt{T}}}
#' \deqn{d_2 = d_1 - \sigma\sqrt{T}}
#'
#' and \eqn{N(\cdot)} is the cumulative standard normal distribution function.
#'
#' This formula assumes:
#' \itemize{
#'   \item Stock price follows geometric Brownian motion: \eqn{dS_t = rS_t dt + \sigma S_t dW_t}
#'   \item No dividends
#'   \item Constant risk-free rate and volatility
#'   \item Continuous trading with no transaction costs or price impact
#' }
#'
#' @section Connection to Binomial Model:
#' As the number of time steps \eqn{n \to \infty}, the Cox-Ross-Rubinstein (CRR)
#' binomial model converges to the Black-Scholes price, provided:
#' \itemize{
#'   \item Time step: \eqn{\Delta t = T/n}
#'   \item Up factor: \eqn{u = e^{\sigma\sqrt{\Delta t}}}
#'   \item Down factor: \eqn{d = e^{-\sigma\sqrt{\Delta t}}}
#'   \item Gross rate per step: \eqn{r_{gross} = e^{r \Delta t}}
#' }
#'
#' See \code{vignette("theory", package = "AsianOptPI")} and
#' \code{benchmark/THEORETICAL_CONNECTION.md} for detailed mathematical derivations.
#'
#' @section Rate Convention:
#' Unlike the binomial functions in this package which use gross rates
#' (e.g., \code{r = 1.05}), this function uses the continuously compounded rate
#' (e.g., \code{r = 0.05}). To convert from gross to continuous:
#' \deqn{r_{continuous} = \log(r_{gross}) / T}
#'
#' @references
#' Black, F., & Scholes, M. (1973). The Pricing of Options and Corporate Liabilities.
#' \emph{Journal of Political Economy}, 81(3), 637-654.
#' \doi{10.1086/260062}
#'
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#' \doi{10.1016/0304-405X(79)90015-1}
#'
#' @examples
#' # Basic at-the-money call
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 1)
#'
#' # Out-of-the-money call
#' price_black_scholes_call(S0 = 100, K = 110, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 1)
#'
#' # In-the-money call
#' price_black_scholes_call(S0 = 100, K = 90, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 1)
#'
#' # Effect of volatility
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.1,
#'                          time_to_maturity = 1)
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.3,
#'                          time_to_maturity = 1)
#'
#' # Short-dated vs long-dated options
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 0.25)
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 1.0)
#'
#' @seealso \code{\link{price_black_scholes_put}}, \code{\link{price_european_call}},
#'   \code{\link{price_black_scholes_binomial}}
#'
#' @export
price_black_scholes_call <- function(S0, K, r, sigma, time_to_maturity) {

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
  if (!is.numeric(time_to_maturity) || length(time_to_maturity) != 1 || time_to_maturity <= 0) {
    stop("time_to_maturity must be a positive number")
  }

  if (sigma == 0) {
    S_T <- S0 * exp(r * time_to_maturity)
    return(max(0, S_T - K) * exp(-r * time_to_maturity))
  }

  if (time_to_maturity < 1e-10) {
    return(max(0, S0 - K))
  }

  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * time_to_maturity) / (sigma * sqrt(time_to_maturity))
  d2 <- d1 - sigma * sqrt(time_to_maturity)

  call_price <- S0 * pnorm(d1) - K * exp(-r * time_to_maturity) * pnorm(d2)

  return(call_price)
}


#' Black-Scholes European Put Option Price
#'
#' Computes the exact price of a European put option using the classical
#' Black-Scholes (1973) analytical formula.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Continuously compounded risk-free rate (e.g., 0.05 for 5\% annual rate)
#' @param sigma Volatility (annualized standard deviation, must be non-negative)
#' @param time_to_maturity Time to maturity in years (must be positive)
#'
#' @return European put option price (numeric)
#'
#' @details
#' The Black-Scholes formula for a European put option is:
#' \deqn{P = K e^{-rT} N(-d_2) - S_0 N(-d_1)}
#'
#' where:
#' \deqn{d_1 = \frac{\log(S_0/K) + (r + \sigma^2/2)T}{\sigma\sqrt{T}}}
#' \deqn{d_2 = d_1 - \sigma\sqrt{T}}
#'
#' and \eqn{N(\cdot)} is the cumulative standard normal distribution function.
#'
#' Alternatively, the put price can be derived from put-call parity:
#' \deqn{P = C - S_0 + K e^{-rT}}
#'
#' @section Put-Call Parity:
#' The Black-Scholes put and call prices satisfy:
#' \deqn{C - P = S_0 - K e^{-rT}}
#'
#' This relationship holds exactly for European options without dividends.
#'
#' @references
#' Black, F., & Scholes, M. (1973). The Pricing of Options and Corporate Liabilities.
#' \emph{Journal of Political Economy}, 81(3), 637-654.
#' \doi{10.1086/260062}
#'
#' @examples
#' # Basic at-the-money put
#' price_black_scholes_put(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                         time_to_maturity = 1)
#'
#' # Verify put-call parity
#' S0 <- 100; K <- 100; r <- 0.05; sigma <- 0.2; time_to_maturity <- 1
#' call <- price_black_scholes_call(S0, K, r, sigma, time_to_maturity)
#' put <- price_black_scholes_put(S0, K, r, sigma, time_to_maturity)
#' parity_lhs <- call - put
#' parity_rhs <- S0 - K * exp(-r * time_to_maturity)
#' cat("Put-Call Parity Check:\n")
#' cat("C - P =", parity_lhs, "\n")
#' cat("S - Ke^(-rT) =", parity_rhs, "\n")
#' cat("Difference:", abs(parity_lhs - parity_rhs), "\n")
#'
#' @seealso \code{\link{price_black_scholes_call}}, \code{\link{price_european_put}}
#'
#' @export
price_black_scholes_put <- function(S0, K, r, sigma, time_to_maturity) {
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
  if (!is.numeric(time_to_maturity) || length(time_to_maturity) != 1 || time_to_maturity <= 0) {
    stop("time_to_maturity must be a positive number")
  }

  if (sigma == 0) {
    S_T <- S0 * exp(r * time_to_maturity)
    return(max(0, K - S_T) * exp(-r * time_to_maturity))
  }

  if (time_to_maturity < 1e-10) {
    return(max(0, K - S0))
  }

  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * time_to_maturity) / (sigma * sqrt(time_to_maturity))
  d2 <- d1 - sigma * sqrt(time_to_maturity)

  put_price <- K * exp(-r * time_to_maturity) * pnorm(-d2) - S0 * pnorm(-d1)

  return(put_price)
}


#' Black-Scholes Price Using Binomial Parameters
#'
#' Converts binomial tree parameters (u, d, n) to continuous parameters
#' (sigma, T) and computes the Black-Scholes price. This provides a direct
#' comparison between the discrete CRR model and its continuous limit.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r_gross Gross risk-free rate for total period (e.g., 1.05 for 5\% over total period)
#' @param u Up factor in binomial tree (must be > 1)
#' @param d Down factor in binomial tree (must be < 1 and < u)
#' @param n Number of time steps (positive integer)
#' @param option_type Character: "call" or "put"
#'
#' @return European option price using Black-Scholes formula (numeric)
#'
#' @details
#' This function converts binomial parameters to continuous parameters using
#' the Cox-Ross-Rubinstein (CRR) matching conditions:
#'
#' \strong{Time step:}
#' \deqn{\Delta t = T/n} where \eqn{T = 1} (total time in years)
#'
#' \strong{Volatility extraction:}
#' From the CRR construction \eqn{u = e^{\sigma\sqrt{\Delta t}}}, \eqn{d = e^{-\sigma\sqrt{\Delta t}}}:
#' \deqn{\sigma = \frac{\log(u/d)}{2\sqrt{\Delta t}}}
#'
#' \strong{Rate conversion:}
#' The gross rate \code{r_gross} is for the total period. The continuously
#' compounded annual rate is:
#' \deqn{r_{continuous} = \frac{\log(r_{gross})}{T} = \log(r_{gross})}
#'
#' For example, if \code{r_gross = 1.05} (5\% over the total period),
#' then \code{r_continuous = log(1.05) = 0.04879}.
#'
#' @section Convergence:
#' As \eqn{n \to \infty}, the CRR binomial price converges to the Black-Scholes
#' price when using the proper parameter conversions described above.
#'
#' See \code{benchmark/THEORETICAL_CONNECTION.md} for the full mathematical
#' derivation and convergence analysis.
#'
#' @references
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#' \doi{10.1016/0304-405X(79)90015-1}
#'
#' @examples
#' # Compare with CRR binomial price (lambda=0)
#' S0 <- 100; K <- 100; r_gross <- 1.05; u <- 1.2; d <- 0.8; n <- 50
#'
#' # Black-Scholes limit
#' bs_price <- price_black_scholes_binomial(S0, K, r_gross, u, d, n, "call")
#'
#' # CRR binomial (no price impact)
#' crr_price <- price_european_call(S0, K, r_gross, u, d,
#'                                   lambda = 0, v_u = 0, v_d = 0, n = n)
#'
#' cat("Black-Scholes:", bs_price, "\n")
#' cat("CRR Binomial:", crr_price, "\n")
#' cat("Difference:", abs(bs_price - crr_price), "\n")
#'
#' @seealso \code{\link{price_black_scholes_call}}, \code{\link{price_black_scholes_put}},
#'   \code{\link{price_european_call}}, \code{\link{price_european_put}}
#'
#' @export
price_black_scholes_binomial <- function(S0, K, r_gross, u, d, n,
                                          option_type = "call") {

  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  if (!is.numeric(u) || length(u) != 1 || u <= 1) {
    stop("u must be greater than 1")
  }
  if (!is.numeric(d) || length(d) != 1 || d >= 1 || d >= u) {
    stop("d must be less than 1 and less than u")
  }
  option_type <- match.arg(option_type, c("call", "put"))

  r_continuous <- log(r_gross)

  dt <- 1 / n
  total_time <- 1
  sigma <- log(u / d) / (2 * sqrt(dt))

  if (option_type == "call") {
    price <- price_black_scholes_call(S0, K, r_continuous, sigma, total_time)
  } else {
    price <- price_black_scholes_put(S0, K, r_continuous, sigma, total_time)
  }

  return(price)
}
