#' Price European Option with Price Impact
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
#' Computes exact prices for European options (call or put) using the binomial
#' model with price impact. Price impact from hedging activities modifies the
#' stock dynamics through adjusted up/down factors and risk-neutral probability.
#'
#' Unlike path-dependent Asian options, European options only depend on the
#' terminal stock price, allowing for efficient O(n) computation instead of O(2^n).
#' See the package vignettes and reference paper for detailed mathematical
#' formulations.
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
#' Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @seealso \code{\link{price_geometric_asian}}, \code{\link{compute_p_adj}}
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
