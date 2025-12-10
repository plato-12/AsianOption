#' Price Geometric Asian Option with Price Impact
#'
#' Computes the exact price of a geometric Asian option (call or put) using the
#' Cox-Ross-Rubinstein (CRR) binomial model with price impact from
#' hedging activities. Uses exact enumeration of all 2^n paths.
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
#'
#' @details
#' Computes exact prices for geometric Asian options using complete path
#' enumeration in a binomial tree. Price impact from hedging activities modifies
#' the stock dynamics through adjusted up/down factors and risk-neutral
#' probability.
#'
#' This function enumerates all 2^n possible paths in the binomial tree for
#' exact pricing (no approximation or sampling). For large n (> 20), this
#' requires significant computation time and memory. See the package vignettes
#' and reference paper for detailed mathematical formulations.
#'
#' @return Geometric Asian option price (numeric).
#' @export
#'
#' @examples
#' # Basic example
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0, v_u = 0, v_d = 0, n = 10
#' )
#'
#' # With price impact
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 15
#' )
#'
#' # Put option
#' price_geometric_asian(
#'   S0 = 100, K = 100, r = 1.05, u = 1.2, d = 0.8,
#'   lambda = 0.1, v_u = 1, v_d = 1, n = 10,
#'   option_type = "put"
#' )
#'
#' @references
#' Tiwari, P., & Majumdar, S. (2024). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @seealso \code{\link{arithmetic_asian_bounds}}, \code{\link{compute_p_adj}}
price_geometric_asian <- function(S0, K, r, u, d, lambda, v_u, v_d, n,
                                   option_type = "call",
                                   validate = TRUE) {

  if (validate) {
    validate_inputs(S0, K, r, u, d, lambda, v_u, v_d, n)
  }

  option_type <- match.arg(option_type, c("call", "put"))

  if (n > 20) {
    warning(sprintf("Computing with n=%d will enumerate 2^%d = %s paths. This may be slow.",
                   n, n, format(2^n, big.mark = ",")))
  }

  result <- price_geometric_asian_cpp(S0, K, r, u, d, lambda, v_u, v_d, n, option_type)

  return(result)
}

