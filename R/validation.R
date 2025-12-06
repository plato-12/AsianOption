#' Validate Input Parameters for Asian Option Pricing
#'
#' @param S0 Initial stock price
#' @param K Strike price
#' @param r Gross risk-free rate
#' @param u Up factor
#' @param d Down factor
#' @param lambda Price impact coefficient
#' @param v_u Hedging volume (up)
#' @param v_d Hedging volume (down)
#' @param n Number of time steps
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
validate_inputs <- function(S0, K, r, u, d, lambda, v_u, v_d, n) {

  if (S0 <= 0) stop("S0 must be positive")
  if (K <= 0) stop("K must be positive")
  if (r <= 0) stop("r must be positive (use gross rate, e.g., 1.05)")
  if (u <= 0) stop("u must be positive")
  if (d <= 0) stop("d must be positive")
  if (lambda < 0) stop("lambda must be non-negative")
  if (v_u < 0) stop("v_u must be non-negative")
  if (v_d < 0) stop("v_d must be non-negative")

  if (!is.numeric(n) || n != as.integer(n) || n <= 0) {
    stop("n must be a positive integer")
  }

  if (u <= d) {
    stop("Up factor u must be greater than down factor d")
  }

  u_tilde <- u * exp(lambda * v_u)
  d_tilde <- d * exp(-lambda * v_d)

  if (d_tilde >= r) {
    stop(sprintf(
      "No-arbitrage condition violated: d_tilde (%.4f) >= r (%.4f). Need d_tilde < r.",
      d_tilde, r
    ))
  }

  if (r >= u_tilde) {
    stop(sprintf(
      "No-arbitrage condition violated: r (%.4f) >= u_tilde (%.4f). Need r < u_tilde.",
      r, u_tilde
    ))
  }

  p_adj <- (r - d_tilde) / (u_tilde - d_tilde)

  if (p_adj < 0 || p_adj > 1) {
    stop(sprintf(
      "Adjusted risk-neutral probability out of bounds: p_adj = %.4f (must be in [0,1])",
      p_adj
    ))
  }

  if (n > 20) {
    num_paths <- 2^n
    if (n > 30) {
      warning(sprintf(
        "n = %d will enumerate 2^%d = %.2e paths. This may be slow.",
        n, n, num_paths
      ))
    } else {
      warning(sprintf(
        "n = %d will enumerate 2^%d = %g paths. This may be slow.",
        n, n, num_paths
      ))
    }
  }

  invisible(NULL)
}
