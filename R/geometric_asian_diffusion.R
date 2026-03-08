#' Geometric Asian Option Price in Exogenous Diffusion Limit
#'
#' Computes the closed-form price for a geometric Asian call option in the
#' exogenous diffusion limit with transient price impact.
#'
#' @param S0 Initial stock price (positive).
#' @param K Strike price (positive).
#' @param r Risk-free rate (positive, typically close to 1 in discrete models).
#' @param sigma Volatility parameter (positive).
#' @param T Time to maturity (positive).
#' @param lambda_T Transient impact coefficient (non-negative).
#' @param I0 Initial transient impact state (can be any real number).
#' @param kappa Mean reversion rate for transient impact (positive).
#' @param eta Noise amplitude for transient impact process. Can be:
#'   - A single positive number (constant eta)
#'   - A function of time t in [0,T] returning a non-negative value
#' @param rho Correlation between stock and impact Brownian motions (in [-1,1]).
#' @param option_type Character string: "call" (default) or "put".
#' @param n_quad Number of quadrature points for numerical integration (default: 1000).
#'
#' @return The price of the geometric Asian option in the exogenous diffusion limit.
#'
#' @details
#' In the exogenous regime with no trading control (nu = 0), the stock price
#' dynamics are:
#'
#'   dS_t = S_t * (r + lambda_T * I_t) dt + sigma * S_t * dW_t
#'   dI_t = -kappa * I_t dt + eta(t) * dW^I_t
#'
#' where W and W^I are Brownian motions with correlation rho.
#'
#' The running log-integral Z_T = integral_0^T log(S_u) du is Gaussian,
#' so G_T = exp(Z_T/T) is lognormal. This yields a Black-Scholes type
#' closed form:
#'
#'   U(0) = exp(-r*T) * [exp(mu_G + sigma_G^2/2) * Phi(d1) - K * Phi(d2)]
#'
#' where:
#'   - mu_G = m_Z / T
#'   - sigma_G^2 = v_Z / T^2
#'   - m_Z and v_Z are the mean and variance of Z_T
#'   - Phi is the standard normal CDF
#'   - d1 = (mu_G - log(K) + sigma_G^2) / sigma_G
#'   - d2 = d1 - sigma_G
#'
#' @references
#' Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @examples
#' # Example 1: Constant eta, no correlation
#' price_geometric_asian_diffusion(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
#'   lambda_T = 0.01, I0 = 0, kappa = 1, eta = 0.1, rho = 0
#' )
#'
#' # Example 2: Time-dependent eta
#' eta_func <- function(t) 0.1 * (1 + 0.5 * t)
#' price_geometric_asian_diffusion(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
#'   lambda_T = 0.01, I0 = 0.5, kappa = 2, eta = eta_func, rho = 0.3
#' )
#'
#' @export
price_geometric_asian_diffusion <- function(S0, K, r, sigma, T,
                                           lambda_T, I0, kappa,
                                           eta, rho = 0,
                                           option_type = "call",
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

  if (is.function(eta)) {
    eta_func <- eta
    test_vals <- eta_func(seq(0, T, length.out = 10))
    if (any(test_vals < 0)) {
      stop("eta(t) must be non-negative for all t in [0,T]")
    }
  } else {
    if (eta < 0) stop("eta must be non-negative")
    eta_func <- function(t) rep(eta, length(t))
  }

  K_kernel <- function(s) {
    (T - s) / kappa - (1 - exp(-kappa * (T - s))) / kappa^2
  }

  if (abs(kappa * T) < 1e-10) {
    impact_mean_term <- lambda_T * I0 * T^2 / 2
  } else {
    impact_mean_term <- lambda_T * I0 * (T / kappa - (1 - exp(-kappa * T)) / kappa^2)
  }

  m_Z <- T * log(S0) + (r - 0.5 * sigma^2) * T^2 / 2 + impact_mean_term

  var_term1 <- sigma^2 * T^3 / 3

  if (lambda_T > 0) {
    s_grid <- seq(0, T, length.out = n_quad)
    integrand2 <- K_kernel(s_grid)^2 * eta_func(s_grid)^2
    var_term2 <- lambda_T^2 * integrate_trapezoid(s_grid, integrand2)

    if (abs(rho) > 1e-10) {
      integrand3 <- (T - s_grid) * K_kernel(s_grid) * eta_func(s_grid)
      var_term3 <- 2 * rho * sigma * lambda_T * integrate_trapezoid(s_grid, integrand3)
    } else {
      var_term3 <- 0
    }
  } else {
    var_term2 <- 0
    var_term3 <- 0
  }

  v_Z <- var_term1 + var_term2 + var_term3

  if (v_Z < 0) {
    warning("Computed variance is negative, setting to zero. Check parameters.")
    v_Z <- 0
  }

  mu_G <- m_Z / T
  sigma_G_sq <- v_Z / T^2
  sigma_G <- sqrt(sigma_G_sq)

  if (sigma_G < 1e-10) {
    G_T <- exp(mu_G)
    if (option_type == "call") {
      payoff <- max(G_T - K, 0)
    } else {
      payoff <- max(K - G_T, 0)
    }
    return(exp(-r * T) * payoff)
  }

  d1 <- (mu_G - log(K) + sigma_G_sq) / sigma_G
  d2 <- d1 - sigma_G

  if (option_type == "call") {
    price <- exp(-r * T) * (
      exp(mu_G + 0.5 * sigma_G_sq) * pnorm(d1) - K * pnorm(d2)
    )
  } else {
    price <- exp(-r * T) * (
      K * pnorm(-d2) - exp(mu_G + 0.5 * sigma_G_sq) * pnorm(-d1)
    )
  }

  return(price)
}


#' Trapezoidal Integration Helper
#'
#' Simple trapezoidal rule for numerical integration.
#'
#' @param x Grid points (assumed equally spaced).
#' @param y Function values at grid points.
#'
#' @return Approximate integral using trapezoidal rule.
#' @keywords internal
integrate_trapezoid <- function(x, y) {
  n <- length(x)
  if (n != length(y)) stop("x and y must have same length")
  if (n < 2) return(0)

  dx <- diff(x)
  avg_y <- (y[-n] + y[-1]) / 2
  sum(dx * avg_y)
}
