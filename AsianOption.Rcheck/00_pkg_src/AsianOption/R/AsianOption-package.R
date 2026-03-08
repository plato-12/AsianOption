#' AsianOption: Asian Option Pricing under Price Impact
#'
#' Implements valuation of Asian options under transient and permanent market
#' impact. The package provides three complementary pricing approaches from
#' Tiwari and Majumdar (2025):
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{price_kemna_vorst_geometric}}: Kemna-Vorst geometric Asian (frictionless benchmark)
#'   \item \code{\link{price_kemna_vorst_arithmetic}}: Kemna-Vorst arithmetic Asian (frictionless benchmark)
#'   \item \code{\link{price_geometric_asian_diffusion}}: Exogenous diffusion closed-form geometric Asian
#'   \item \code{\link{price_arithmetic_asian_diffusion}}: Exogenous diffusion arithmetic Asian (Monte Carlo)
#'   \item \code{\link{price_geometric_asian_hjb}}: Endogenous HJB geometric Asian (Bellman scheme)
#'   \item \code{\link{price_arithmetic_asian_hjb}}: Endogenous HJB arithmetic Asian (Bellman scheme)
#' }
#'
#' @section Pricing Approaches:
#' \enumerate{
#'   \item \strong{Kemna-Vorst benchmark}: Frictionless Black-Scholes-type pricing
#'     for geometric and arithmetic Asian options (no price impact).
#'   \item \strong{Exogenous diffusion}: Passive trading regime where the impact
#'     state evolves as an exogenous Ornstein-Uhlenbeck process. The geometric
#'     Asian call admits a closed-form solution; the arithmetic Asian is priced
#'     via Monte Carlo.
#'   \item \strong{Endogenous HJB}: Strategic trading regime where the hedger's
#'     trading rate feeds back into the price dynamics. Solved via a tree-based
#'     Bellman recursion producing indifference bid and ask prices.
#' }
#'
#' @references
#' Tiwari, P., & Majumdar, S. (2025). Asian option valuation under price impact.
#' \emph{arXiv preprint}. \doi{10.48550/arXiv.2512.07154}
#'
#' @examples
#' # Kemna-Vorst benchmark (frictionless)
#' price_kemna_vorst_geometric(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2,
#'   T0 = 0, T_mat = 1
#' )
#'
#' # Exogenous diffusion closed-form (geometric Asian with impact)
#' price_geometric_asian_diffusion(
#'   S0 = 100, K = 100, r = 0.05, sigma = 0.2, T = 1,
#'   lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.5, rho = 0
#' )
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib AsianOption, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats pnorm
## usethis namespace: end
NULL
