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


#' Validate Input Parameters for Transient Impact Model
#'
#' @param S0 Initial stock price
#' @param K Strike price
#' @param r Gross risk-free rate
#' @param u Up factor
#' @param d Down factor
#' @param lambda_P Permanent impact coefficient
#' @param lambda_T Transient impact coefficient
#' @param alpha Transient decay rate
#' @param psi Volume power-law exponent
#' @param volumes Vector of hedging volumes
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
validate_transient_inputs <- function(S0, K, r, u, d,
                                       lambda_P, lambda_T,
                                       alpha, psi, volumes) {

  # Basic parameter validation
  if (S0 <= 0) stop("S0 must be positive")
  if (K <= 0) stop("K must be positive")
  if (r <= 0) stop("r must be positive (use gross rate, e.g., 1.05)")
  if (u <= 0) stop("u must be positive")
  if (d <= 0) stop("d must be positive")

  if (u <= d) {
    stop("Up factor u must be greater than down factor d")
  }

  # Transient impact parameters
  if (lambda_P < 0) stop("lambda_P (permanent impact) must be non-negative")
  if (lambda_T < 0) stop("lambda_T (transient impact) must be non-negative")

  if (alpha < 0 || alpha >= 1) {
    stop("alpha (decay rate) must be in [0, 1)")
  }

  if (psi <= 0) {
    stop("psi (volume power) must be positive")
  }

  # Volume validation
  if (!is.numeric(volumes) || length(volumes) == 0) {
    stop("volumes must be a non-empty numeric vector")
  }

  if (any(volumes < 0)) {
    stop("All volumes must be non-negative")
  }

  n <- length(volumes)

  if (n > 20) {
    num_paths <- 2^n
    if (n > 30) {
      warning(sprintf(
        "n = %d will enumerate 2^%d = %.2e paths. Computation will be very slow.",
        n, n, num_paths
      ))
    } else {
      warning(sprintf(
        "n = %d will enumerate 2^%d = %g paths. This may be slow.",
        n, n, num_paths
      ))
    }
  }

  # No-arbitrage validation (worst case)
  v_max <- max(volumes)
  lambda_eff <- lambda_P + lambda_T
  v_max_impact <- v_max^psi

  # Maximum transient accumulator (geometric series sum)
  # I_max = v_max^psi / (1 - alpha) for consecutive up moves
  if (alpha < 1) {
    I_max <- v_max_impact / (1 - alpha)
  } else {
    I_max <- n * v_max_impact
  }

  # The adjusted factors use alpha * I_m (decayed accumulator)
  # u_tilde = u * exp(lambda_eff * v + lambda_T * alpha * I_m)
  # d_tilde = d * exp(-lambda_eff * v + lambda_T * alpha * I_m)
  #
  # For no-arbitrage: d_tilde < r < u_tilde must hold for all paths
  # Worst case for d_tilde < r: when alpha * I_m is maximum (d_tilde largest)
  # Worst case for r < u_tilde: when alpha * I_m is minimum (u_tilde smallest)

  alpha_I_max <- alpha * I_max
  alpha_I_min <- -alpha * I_max  # Symmetric: consecutive down moves

  # Worst-case d_tilde (maximum value, when alpha * I_m is maximum)
  d_tilde_max <- d * exp(-lambda_eff * v_max_impact + lambda_T * alpha_I_max)

  # Worst-case u_tilde (minimum value, when alpha * I_m is minimum)
  u_tilde_min <- u * exp(lambda_eff * v_max_impact + lambda_T * alpha_I_min)

  if (d_tilde_max >= r) {
    stop(sprintf(
      "No-arbitrage condition may be violated: worst-case d_tilde (%.4f) >= r (%.4f).",
      d_tilde_max, r
    ))
  }

  if (r >= u_tilde_min) {
    stop(sprintf(
      "No-arbitrage condition may be violated: r (%.4f) >= worst-case u_tilde (%.4f).",
      r, u_tilde_min
    ))
  }

  invisible(NULL)
}
