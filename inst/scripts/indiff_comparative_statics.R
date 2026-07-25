# Paper-support script: three-model comparison and comparative statics for the
# utility-indifference engine (design.md M6, analogues of Tables 4 and 5).
#
# Run with:  Rscript inst/scripts/indiff_comparative_statics.R
#
# IMPORTANT.  Every comparative static here is reported together with a
# refinement check, because several of them are NOT stable at coarse grids.
# In particular the sign of d(spread)/d(lambda_bar_T) changes between N = 10
# and N = 20, and the magnitude of the spread roughly halves between n_Q = 9
# and n_Q = 33.  Do not quote a sign from a single grid.

library(AsianOption)

S0 <- 100; K <- 100; T <- 1; sigma <- 0.2; r <- 0.05

kv_geo <- local({
  sg <- sigma / sqrt(3); b <- 0.5 * (r - sigma^2 / 6)
  d1 <- (log(S0 / K) + (b + sg^2 / 2) * T) / (sg * sqrt(T))
  exp(-r * T) * (S0 * exp(b * T) * pnorm(d1) - K * pnorm(d1 - sg * sqrt(T)))
})

base <- list(S0 = S0, K = K, T = T, N = 25L, sigma = sigma, r_cont = r,
             gamma = 0.05, k_A = 0.05, k_B = 0.05,
             lambda_bar_T = 0.05, lambda_bar_P = 0.025, kappa_J = 1,
             Q_bar = 2, nu_bar = 4,
             n_I = 21L, n_Q = 21L, n_J = 15L, n_R = 121L, n_controls = 15L)
run <- function(ov = list())
  do.call(price_geometric_asian_indiff, utils::modifyList(base, ov))

# --- Table 4 analogue: three models side by side ----------------------------

cat("=========================================================\n")
cat(" Table 4 analogue: frictionless, exogenous, endogenous\n")
cat("=========================================================\n")
cat(sprintf("%-42s %12s %12s\n", "model", "bid", "ask"))
cat(sprintf("%-42s %12.6f %12.6f\n",
            "Kemna-Vorst geometric (frictionless)", kv_geo, kv_geo))

exo <- price_geometric_asian_diffusion(
  S0 = S0, K = K, r = r, sigma = sigma, T = T,
  lambda_T = 0.05, I0 = 0, kappa = 1, eta = 0.5, rho = 0
)
cat(sprintf("%-42s %12.6f %12.6f\n",
            "exogenous diffusion (single price)", exo, exo))

for (g in c(0.02, 0.05, 0.1)) {
  z <- run(list(gamma = g))
  cat(sprintf("%-42s %12.6f %12.6f\n",
              sprintf("utility indifference (gamma = %.2f)", g),
              z$bid_price, z$ask_price))
}

cat("\nNote the contrast with the legacy endogenous module, whose ask was\n")
cat("roughly twice the Kemna-Vorst price. Here the quotes bracket it.\n")

# --- Table 5 analogue: comparative statics with refinement -------------------

sweep <- function(label, param, vals, grids) {
  cat(sprintf("\n%s\n", label))
  cat(sprintf("  %-16s", "grid"))
  cat(paste(sprintf("%10s", format(vals)), collapse = ""), "   direction\n")
  for (gn in names(grids)) {
    sp <- vapply(vals, function(v) {
      ov <- grids[[gn]]
      if (param == "k") { ov$k_A <- v; ov$k_B <- v } else ov[[param]] <- v
      run(ov)$spread
    }, numeric(1))
    dir <- if (all(diff(sp) > 0)) "increasing"
           else if (all(diff(sp) < 0)) "decreasing" else "non-monotone"
    cat(sprintf("  %-16s%s   %s\n", gn,
                paste(sprintf("%10.5f", sp), collapse = ""), dir))
  }
}

grids <- list(
  "coarse"  = list(N = 10L, n_Q = 9L,  n_J = 9L,  n_R = 41L, n_controls = 9L),
  "medium"  = list(N = 25L, n_Q = 21L, n_J = 15L, n_R = 121L, n_controls = 15L),
  "fine"    = list(N = 40L, n_Q = 33L, n_J = 21L, n_R = 161L, n_controls = 21L)
)

cat("\n=========================================================\n")
cat(" Table 5 analogue: spread comparative statics\n")
cat("=========================================================\n")

sweep("spread vs temporary cost k_A = k_B", "k",
      c(0.005, 0.05, 0.2, 1.0), grids)
sweep("spread vs risk aversion gamma", "gamma",
      c(0.01, 0.05, 0.2), grids)
sweep("spread vs transient execution impact lambda_bar_T", "lambda_bar_T",
      c(0, 0.1, 0.3, 0.6), grids)
sweep("spread vs inventory concession lambda_bar_P", "lambda_bar_P",
      c(0, 0.1, 0.3, 0.6), grids)
sweep("spread vs dealer impact decay kappa_J", "kappa_J",
      c(0.5, 2, 8), grids)

cat("\n---------------------------------------------------------\n")
cat("Reading the table\n")
cat("---------------------------------------------------------\n")
cat("* k_A = k_B and gamma: increasing on every grid. These are the two\n")
cat("  robust results, and the cost direction reverses the sign reported for\n")
cat("  the legacy model: trading here is hedging, not manipulation, so\n")
cat("  costlier execution widens the quotes.\n")
cat("* lambda_bar_T: sign is grid-dependent at coarse resolution. Quote it\n")
cat("  only from the fine row, and only after checking it is stable under one\n")
cat("  further refinement.\n")
cat("* lambda_bar_P: decreasing. This is expected rather than anomalous.\n")
cat("  A linear inventory-proportional concession contributes\n")
cat("  lambda_bar_P (q_T^2 - q_0^2)/2 to total execution cash, so it is\n")
cat("  costless on any round trip and only bites through terminal inventory.\n")
cat("  It is not a friction in the sense that k_A, k_B are.\n")
cat("* kappa_J: the round-trip cost of the transient term is\n")
cat("  lambda_bar_T * kappa_J * integral(j^2), which is not monotone in\n")
cat("  kappa_J because faster decay also shrinks j. Sign is grid-dependent.\n")

# --- optimal hedging paths ---------------------------------------------------

cat("\n=========================================================\n")
cat(" Optimal hedge along the zero-shock trajectory\n")
cat("=========================================================\n")
z <- run(list(store_policy = TRUE))
cat(sprintf("%5s %10s %10s %10s %10s\n", "step", "nu short", "Q short",
            "nu long", "Q long"))
for (m in seq_len(min(10L, base$N))) {
  cat(sprintf("%5d %10.4f %10.4f %10.4f %10.4f\n", m,
              z$optimal_nu_seller[m], z$Q_path_seller[m + 1],
              z$optimal_nu_buyer[m], z$Q_path_buyer[m + 1]))
}
cat("\nA dealer short the call accumulates a positive hedge; a dealer long the\n")
cat("call accumulates the mirror-image short position.\n")
