# Convergence study for the utility-indifference engine (design.md M6).
#
# Run with:  Rscript inst/scripts/indiff_convergence.R
#
# Produces three tables:
#   (1) the frictionless limit against Kemna-Vorst, refining one dimension at
#       a time, which is the accuracy statement quoted in the paper;
#   (2) the engine against the EXACT value of the discrete model it implements,
#       which separates model discretisation from grid interpolation error;
#   (3) one-at-a-time refinement of the base case with frictions, which is the
#       stability check for any number reported from this engine.
#
# The acceptance target for M6 is that quoted numbers move by less than 0.5%
# under one further refinement step.  Table (3) is where that is checked; note
# that the PRICE LEVEL meets it comfortably while the SPREAD is materially
# harder and needs a fine inventory grid.

library(AsianOption)

S0 <- 100; K <- 100; T <- 1; sigma <- 0.2; r <- 0.05

# --- benchmarks -------------------------------------------------------------

# Kemna-Vorst geometric closed form, a time-0 present value like everything
# else here.
kv_geo <- price_kemna_vorst_geometric(S0, K, r, sigma, 0, T)

# Exact expected payoff of the DISCRETE model the engine implements, for the
# geometric case with lambda_I = 0 and no trading.  With
#   log S_m - log S0 = m c dt + sigma sqrt(dt) sum_{k<m} xi_k,   c = mu - sigma^2/2,
# the trapezoidal accumulator is
#   a_N = dt [ sum_{m=1}^{N-1} x_m + x_N/2 ]
#       = A + B sum_k (N - k - 1/2) xi_k,
# whose weights are half-integers, so the law of the sum is obtained exactly by
# convolution on a lattice of spacing 1/2.  No simulation is involved.
exact_discrete <- function(N, rule = c("trapezoid", "left")) {
  rule <- match.arg(rule)
  dt <- T / N; c0 <- r - sigma^2 / 2
  if (rule == "trapezoid") {
    A <- dt * c0 * dt * (N * (N - 1) / 2 + N / 2)
    w <- N - (0:(N - 1)) - 0.5
  } else {
    A <- dt * c0 * dt * (N * (N - 1) / 2)
    w <- N - (0:(N - 1)) - 1
  }
  B <- dt * sigma * sqrt(dt)
  step <- 0.5
  iw <- round(w / step)
  off <- sum(abs(iw)); len <- 2 * off + 1
  p <- numeric(len); p[off + 1] <- 1
  idx <- seq_len(len)
  for (s in iw) {
    q <- numeric(len)
    lo <- idx - s; hi <- idx + s
    ok <- lo >= 1 & lo <= len; q[idx[ok]] <- q[idx[ok]] + 0.5 * p[lo[ok]]
    ok <- hi >= 1 & hi <= len; q[idx[ok]] <- q[idx[ok]] + 0.5 * p[hi[ok]]
    p <- q
  }
  keep <- p > 0
  aN <- A + B * ((idx - 1 - off) * step)[keep]
  sum(p[keep] * pmax(S0 * exp(aN / T) - K, 0)) * exp(-r * T)
}

# --- helpers ----------------------------------------------------------------

frictionless <- function(N, n_R, n_logS = NULL, accum_rule = "trapezoid") {
  price_geometric_asian_indiff(
    S0 = S0, K = K, T = T, N = N, sigma = sigma, r_cont = r,
    lambda_I = 0, eta = 0, lambda_bar_T = 0, lambda_bar_P = 0,
    k_A = 0, k_B = 0, gamma = 1e-6, Gamma_Q = 1e-6, Gamma_J = 1e-6,
    Q_bar = 10, nu_bar = 10, control_set = 0,
    n_I = 5L, n_Q = 5L, n_J = 5L, n_R = n_R, n_logS = n_logS,
    accum_rule = accum_rule
  )
}

cat("=========================================================\n")
cat(" (1) Frictionless limit vs Kemna-Vorst\n")
cat("=========================================================\n")
cat(sprintf("continuum Kemna-Vorst (discounted) = %.6f\n\n", kv_geo))
cat(sprintf("%5s %8s %8s %14s %10s\n", "N", "n_logS", "n_R", "ask", "err %"))
for (N in c(10L, 25L, 50L)) {
  for (n_R in c(41L, 81L, 161L, 321L)) {
    z <- frictionless(N, n_R)
    cat(sprintf("%5d %8d %8d %14.6f %9.3f%%\n", N, z$grid_sizes$n_logS,
                z$grid_sizes$n_R, z$ask_price, 100 * (z$ask_price / kv_geo - 1)))
  }
}

cat("\nEffect of the two discretisation choices (N = 25, n_R = 161):\n")
z_ok  <- frictionless(25L, 161L)
z_lef <- frictionless(25L, 161L, accum_rule = "left")
z_una <- frictionless(25L, 161L, n_logS = 41L)
cat(sprintf("  trapezoid + aligned grid  %10.6f  %+8.3f%%\n",
            z_ok$ask_price, 100 * (z_ok$ask_price / kv_geo - 1)))
cat(sprintf("  left-endpoint accumulator %10.6f  %+8.3f%%\n",
            z_lef$ask_price, 100 * (z_lef$ask_price / kv_geo - 1)))
cat(sprintf("  unaligned log-price grid  %10.6f  %+8.3f%%\n",
            z_una$ask_price, 100 * (z_una$ask_price / kv_geo - 1)))

cat("\n=========================================================\n")
cat(" (2) Engine vs the EXACT value of its own discrete model\n")
cat("=========================================================\n")
cat("This isolates grid interpolation error: the engine must converge to the\n")
cat("exact column, and the exact column converges to Kemna-Vorst in N.\n\n")
cat(sprintf("%5s %14s %10s %8s %14s %10s\n",
            "N", "exact discrete", "vs KV %", "n_R", "engine", "vs exact %"))
for (N in c(25L, 50L)) {
  ex <- exact_discrete(N)
  for (n_R in c(81L, 161L, 321L, 641L)) {
    z <- frictionless(N, n_R)
    cat(sprintf("%5d %14.6f %9.3f%% %8d %14.6f %9.3f%%\n",
                N, ex, 100 * (ex / kv_geo - 1), z$grid_sizes$n_R,
                z$ask_price, 100 * (z$ask_price / ex - 1)))
  }
}

cat("\n=========================================================\n")
cat(" (3) One-at-a-time refinement of the base case with frictions\n")
cat("=========================================================\n")
base <- list(S0 = S0, K = K, T = T, N = 25L, sigma = sigma, r_cont = r,
             n_I = 21L, n_Q = 21L, n_J = 15L, n_R = 121L, n_controls = 15L)
run <- function(ov = list())
  do.call(price_geometric_asian_indiff, utils::modifyList(base, ov))

ref <- run()
cat(sprintf("base: bid = %.6f  ask = %.6f  spread = %.6f  (%.1f s)\n\n",
            ref$bid_price, ref$ask_price, ref$spread,
            ref$diagnostics$runtime_sec))
cat(sprintf("%-28s %10s %10s %10s %10s\n",
            "refinement", "ask", "d ask %", "spread", "d spread %"))
refine <- list(
  "N: 25 -> 50"            = list(N = 50L),
  "n_R: 121 -> 241"        = list(n_R = 241L),
  "n_Q: 21 -> 41"          = list(n_Q = 41L),
  "n_J: 15 -> 29"          = list(n_J = 29L),
  "n_controls: 15 -> 29"   = list(n_controls = 29L),
  "accum_sd: 5 -> 7"       = list(accum_sd = 7)
)
for (nm in names(refine)) {
  z <- run(refine[[nm]])
  cat(sprintf("%-28s %10.6f %9.2f%% %10.6f %9.2f%%\n", nm, z$ask_price,
              100 * (z$ask_price / ref$ask_price - 1), z$spread,
              100 * (z$spread / ref$spread - 1)))
}
cat("\nAcceptance target: |d ask| and |d spread| below 0.5%.\n")
cat("The price level normally meets this; the spread is the binding case and\n")
cat("is most sensitive to n_Q, because one time step moves the inventory by\n")
cat("only nu_bar * dt and that must be resolved by the inventory grid.\n")
