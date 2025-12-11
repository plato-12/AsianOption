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
#'
#' @references
#' Black, F., & Scholes, M. (1973). The Pricing of Options and Corporate Liabilities.
#' \emph{Journal of Political Economy}, 81(3), 637-654.
#' \doi{10.1086/260062}
#'
#' @examples
#' price_black_scholes_call(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                          time_to_maturity = 1)
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
#' price_black_scholes_put(S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'                         time_to_maturity = 1)
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

