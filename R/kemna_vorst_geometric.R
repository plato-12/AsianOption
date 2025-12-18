#' Kemna-Vorst Geometric Average Asian Option
#'
#' Calculates the price of a geometric average Asian call option using the
#' closed-form analytical solution from Kemna & Vorst (1990). This is the
#' standard benchmark implementation WITHOUT price impact.
#'
#' @param S0 Numeric. Initial stock price at time T0 (start of averaging period).
#'   Must be positive.
#' @param K Numeric. Strike price. Must be positive.
#' @param r Numeric. Gross risk-free interest rate per period (e.g., 1.05 for 5%).
#'   Must be positive.
#' @param sigma Numeric. Volatility (annualized standard deviation). Must be
#'   non-negative.
#' @param T0 Numeric. Start time of averaging period. Must be non-negative.
#' @param T_mat Numeric. Maturity time. Must be greater than T0.
#' @param option_type Character. Type of option: "call" (default) or "put".
#'
#' @return Numeric. The analytical price of the geometric average Asian option.
#'
#' @details
#' The geometric average at maturity is defined as:
#' \deqn{G_T = \exp\left(\frac{1}{T-T_0} \int_{T_0}^{T} \log(S(\tau)) d\tau\right)}
#'
#' For the discrete case with n+1 observations:
#' \deqn{G_T = \left(\prod_{i=0}^{n} S(T_i)\right)^{1/(n+1)}}
#'
#' The closed-form solution for a call option is:
#' \deqn{C = S_0 e^{d^*} N(d) - K N(d - \sigma_G\sqrt{T-T_0})}
#'
#' where:
#' \deqn{d^* = \frac{1}{2}(r - \frac{\sigma^2}{6})(T - T_0)}
#' \deqn{d = \frac{\log(S_0/K) + \frac{1}{2}(r + \frac{\sigma^2}{6})(T-T_0)}{\sigma\sqrt{(T-T_0)/3}}}
#'
#' and \eqn{N(\cdot)} is the cumulative standard normal distribution function.
#'
#' @references
#' Kemna, A.G.Z. and Vorst, A.C.F. (1990). "A Pricing Method for Options Based
#' on Average Asset Values." \emph{Journal of Banking and Finance}, 14, 113-129.
#'
#' @examples
#' price_kemna_vorst_geometric(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'   T0 = 0, T_mat = 1, option_type = "call"
#' )
#'
#' @export
price_kemna_vorst_geometric <- function(S0, K, r, sigma, T0, T_mat,
                                         option_type = "call") {

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
  if (!is.numeric(T_mat) || length(T_mat) != 1 || T_mat <= T0) {
    stop("T_mat must be greater than T0")
  }
  option_type <- match.arg(option_type, c("call", "put"))

  tau <- T_mat - T0

  if (sigma == 0) {
    G_T <- S0 * exp(r * tau / 2)
    if (option_type == "call") {
      return(max(0, G_T - K) * exp(-r * tau))
    } else {
      return(max(0, K - G_T) * exp(-r * tau))
    }
  }

  sigma_G <- sigma / sqrt(3)
  d_star <- 0.5 * (r - sigma^2 / 6) * tau

  d <- (log(S0 / K) + 0.5 * (r + sigma^2 / 6) * tau) / (sigma * sqrt(tau / 3))

  d2 <- d - sigma_G * sqrt(tau)

  if (option_type == "call") {
    price <- exp(d_star) * S0 * pnorm(d) - K * pnorm(d2)
  } else {
    price <- K * pnorm(-d2) - exp(d_star) * S0 * pnorm(-d)
  }

  return(price)
}

price_kemna_vorst_geometric_binomial <- function(S0, K, r, u, d, n,
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

  r_continuous <- log(r)

  dt <- 1 / n

  sigma <- log(u / d) / (2 * sqrt(dt))

  T0 <- 0
  T_mat <- 1

  price_kemna_vorst_geometric(S0, K, r_continuous, sigma, T0, T_mat, option_type)
}
