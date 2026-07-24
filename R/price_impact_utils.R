#' Compute Adjusted Risk-Neutral Probability
#'
#' Calculates the adjusted risk-neutral probability incorporating
#' price impact from hedging activities.
#'
#' @param r Gross risk-free rate per period
#' @param u Base up factor
#' @param d Base down factor
#' @param lambda Price impact coefficient
#' @param v_u Hedging volume on up move
#' @param v_d Hedging volume on down move
#'
#' @return Adjusted risk-neutral probability (numeric)
#' @export
#'
#' @examples
#' compute_p_adj(r = 1.05, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)
compute_p_adj <- function(r, u, d, lambda, v_u, v_d) {
  u_tilde <- u * exp(lambda * v_u)
  d_tilde <- d * exp(-lambda * v_d)
  p_adj <- (r - d_tilde) / (u_tilde - d_tilde)
  return(p_adj)
}

#' Compute Adjusted Up and Down Factors
#'
#' Calculates the modified up and down factors after incorporating
#' price impact from hedging.
#'
#' @param u Base up factor
#' @param d Base down factor
#' @param lambda Price impact coefficient
#' @param v_u Hedging volume on up move
#' @param v_d Hedging volume on down move
#'
#' @return List with elements \code{u_tilde} and \code{d_tilde}
#' @export
#'
#' @examples
#' compute_adjusted_factors(u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)
compute_adjusted_factors <- function(u, d, lambda, v_u, v_d) {
  list(
    u_tilde = u * exp(lambda * v_u),
    d_tilde = d * exp(-lambda * v_d)
  )
}

#' Check No-Arbitrage Condition
#'
#' Verifies that the no-arbitrage condition
#' \eqn{\tilde{d} < r < \tilde{u}} holds.
#'
#' @inheritParams compute_p_adj
#'
#' @return Logical: TRUE if condition holds, FALSE otherwise
#' @export
#'
#' @examples
#' check_no_arbitrage(r = 1.05, u = 1.2, d = 0.8, lambda = 0.1, v_u = 1, v_d = 1)
check_no_arbitrage <- function(r, u, d, lambda, v_u, v_d) {
  factors <- compute_adjusted_factors(u, d, lambda, v_u, v_d)
  (factors$d_tilde < r) && (r < factors$u_tilde)
}
