#' Price European Call Option with Price Impact
#'
#' Computes the exact price of a European call option using the
#' Cox-Ross-Rubinstein (CRR) binomial model with price impact from
#' hedging activities.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05 for 5\% rate)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer)
#' @param validate Logical; if TRUE, performs input validation
#'
#' @details
#' The European call option payoff is:
#' \deqn{V_n = \max(0, S_n - K)}
#'
#' Price impact modifies the stock dynamics:
#' \itemize{
#'   \item Effective up factor: \eqn{\tilde{u} = u \cdot e^{\lambda v^u}}
#'   \item Effective down factor: \eqn{\tilde{d} = d \cdot e^{-\lambda v^d}}
#'   \item Risk-neutral probability: \eqn{p^{eff} = \frac{r - \tilde{d}}{\tilde{u} - \tilde{d}}}
#' }
#'
#' Unlike path-dependent Asian options, European options only depend on the
#' terminal stock price, allowing for efficient O(n) computation instead of O(2^n).
#'
#' The pricing formula is:
#' \deqn{V_0 = \frac{1}{r^n} \sum_{k=0}^{n} \binom{n}{k} p_{eff}^k (1-p_{eff})^{n-k} \max(0, S_n(k) - K)}
#'
#' @return European call option price (numeric)
#' @export
#'
#' @examples
#' # Basic example with no price impact
#' price_european_call(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0, v_u = 0, v_d = 0, n = 10
#' )
#'
#' # Example with price impact
#' price_european_call(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10
#' )
#'
#' # High price impact increases call value
#' price_european_call(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.2, v_u = 1, v_d = 1, n = 10
#' )
#'
#' @references
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#' \doi{10.1016/0304-405X(79)90015-1}
#'
#' @seealso \code{\link{price_european_put}}, \code{\link{price_geometric_asian}}, \code{\link{compute_p_adj}}
price_european_call <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                 validate = TRUE) {

  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  result <- price_european_call_cpp(S0, K, r, u, d, lambda, v_u, v_d, n)

  return(result)
}

#' Price European Put Option with Price Impact
#'
#' Computes the exact price of a European put option using the
#' Cox-Ross-Rubinstein (CRR) binomial model with price impact from
#' hedging activities.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05 for 5\% rate)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer)
#' @param validate Logical; if TRUE, performs input validation
#'
#' @details
#' The European put option payoff is:
#' \deqn{V_n = \max(0, K - S_n)}
#'
#' Price impact modifies the stock dynamics:
#' \itemize{
#'   \item Effective up factor: \eqn{\tilde{u} = u \cdot e^{\lambda v^u}}
#'   \item Effective down factor: \eqn{\tilde{d} = d \cdot e^{-\lambda v^d}}
#'   \item Risk-neutral probability: \eqn{p^{eff} = \frac{r - \tilde{d}}{\tilde{u} - \tilde{d}}}
#' }
#'
#' Unlike path-dependent Asian options, European options only depend on the
#' terminal stock price, allowing for efficient O(n) computation instead of O(2^n).
#'
#' The pricing formula is:
#' \deqn{V_0 = \frac{1}{r^n} \sum_{k=0}^{n} \binom{n}{k} p_{eff}^k (1-p_{eff})^{n-k} \max(0, K - S_n(k))}
#'
#' @return European put option price (numeric)
#' @export
#'
#' @examples
#' # Basic example with no price impact
#' price_european_put(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0, v_u = 0, v_d = 0, n = 10
#' )
#'
#' # Example with price impact
#' price_european_put(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10
#' )
#'
#' # Verify put-call parity (approximately holds with price impact)
#' call_price <- price_european_call(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10
#' )
#' put_price <- price_european_put(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10
#' )
#'
#' @references
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#' \doi{10.1016/0304-405X(79)90015-1}
#'
#' @seealso \code{\link{price_european_call}}, \code{\link{price_geometric_asian}}, \code{\link{compute_p_adj}}
price_european_put <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                validate = TRUE) {

  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  result <- price_european_put_cpp(S0, K, r, u, d, lambda, v_u, v_d, n)

  return(result)
}

#' Price European Option with Price Impact (Unified Interface)
#'
#' Computes the exact price of a European option (call or put) using the
#' Cox-Ross-Rubinstein (CRR) binomial model with price impact from
#' hedging activities.
#'
#' @param S0 Initial stock price (must be positive)
#' @param K Strike price (must be positive)
#' @param r Gross risk-free rate per period (e.g., 1.05 for 5\% rate)
#' @param u Base up factor in CRR model (must be > d)
#' @param d Base down factor in CRR model (must be positive)
#' @param lambda Price impact coefficient (non-negative)
#' @param v_u Hedging volume on up move (non-negative)
#' @param v_d Hedging volume on down move (non-negative)
#' @param n Number of time steps (positive integer)
#' @param option_type Character; either "call" (default) or "put"
#' @param validate Logical; if TRUE, performs input validation
#'
#' @details
#' The European option payoff is:
#' \itemize{
#'   \item Call: \eqn{V_n = \max(0, S_n - K)}
#'   \item Put: \eqn{V_n = \max(0, K - S_n)}
#' }
#'
#' Price impact modifies the stock dynamics:
#' \itemize{
#'   \item Effective up factor: \eqn{\tilde{u} = u \cdot e^{\lambda v^u}}
#'   \item Effective down factor: \eqn{\tilde{d} = d \cdot e^{-\lambda v^d}}
#'   \item Risk-neutral probability: \eqn{p^{eff} = \frac{r - \tilde{d}}{\tilde{u} - \tilde{d}}}
#' }
#'
#' Unlike path-dependent Asian options, European options only depend on the
#' terminal stock price, allowing for efficient O(n) computation instead of O(2^n).
#'
#' @return European option price (numeric)
#' @export
#'
#' @examples
#' # Call option with no price impact
#' price_european(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0, v_u = 0, v_d = 0, n = 10, option_type = "call"
#' )
#'
#' # Put option with price impact
#' price_european(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10, option_type = "put"
#' )
#'
#' # Verify put-call parity
#' call <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10, "call")
#' put <- price_european(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 10, "put")
#'
#' @references
#' Cox, J. C., Ross, S. A., & Rubinstein, M. (1979).
#' Option pricing: A simplified approach.
#' \emph{Journal of Financial Economics}, 7(3), 229-263.
#' \doi{10.1016/0304-405X(79)90015-1}
#'
#' @seealso \code{\link{price_european_call}}, \code{\link{price_european_put}},
#'   \code{\link{price_geometric_asian}}, \code{\link{compute_p_adj}}
price_european <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                           option_type = "call",
                           validate = TRUE) {
  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  if (option_type == "call") {
    result <- price_european_call_cpp(S0, K, r, u, d, lambda, v_u, v_d, n)
  } else {
    result <- price_european_put_cpp(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  return(result)
}
