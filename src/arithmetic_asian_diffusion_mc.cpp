#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
using namespace Rcpp;

namespace {

// ---------------------------------------------------------------------------
// Two independent draws from R's single generator
// ---------------------------------------------------------------------------
//
// This sampler needs two logically independent streams of normals: one for the
// price shock W, one for the impact shock W^I.  R exposes a single global
// generator, so the two are kept apart by swapping .Random.seed in and out
// around each block of draws.
//
// The point of the separation is that the PRICE stream is then exactly
// n_steps draws per path, in path order, WHATEVER the impact parameters are.
// Two properties follow, and callers depend on both:
//
//   * at lambda_T = 0 the price shocks are the same draws, in the same order,
//     that price_kemna_vorst_arithmetic() takes, so the two routines walk the
//     same paths and their prices agree to floating-point rounding; and
//
//   * across a sweep in lambda_T the price shocks are unchanged, so a
//     difference of two prices in the sweep is a common-random-numbers
//     difference and the Monte Carlo error in it is an order of magnitude
//     below the error in either level.
//
// Interleaving the two draws in one stream -- the obvious implementation --
// gives up the first property; skipping the impact draw when it cannot matter
// gives up the second.  Only separate streams give both.

std::vector<int> rng_save() {
  PutRNGstate();
  Environment g = Environment::global_env();
  IntegerVector s = g[".Random.seed"];
  return std::vector<int>(s.begin(), s.end());
}

void rng_restore(const std::vector<int>& v) {
  Environment g = Environment::global_env();
  g[".Random.seed"] = IntegerVector(v.begin(), v.end());
  GetRNGstate();
}

std::vector<int> rng_seed_state(int s) {
  Environment base_env("package:base");
  Function set_seed = base_env["set.seed"];
  set_seed(s);
  Environment g = Environment::global_env();
  IntegerVector st = g[".Random.seed"];
  return std::vector<int>(st.begin(), st.end());
}

// Paths per RNG block.  The stream layout does not depend on this -- a block
// draws nb * n_steps normals contiguously, so every path still occupies its own
// n_steps-long slice -- it only amortises the cost of swapping the state.
const int RNG_BLOCK = 256;

}  // namespace

// [[Rcpp::export]]
List price_arithmetic_asian_diffusion_mc_cpp(
    double S0, double K, double r, double sigma,
    double T_mat, double lambda_T, double I0, double kappa,
    NumericVector eta_values, double rho,
    int n_steps, int n_sims,
    std::string option_type,
    bool use_control_variate,
    int seed
) {
  double dt = T_mat / n_steps;
  double sqrt_dt = std::sqrt(dt);
  double discount = std::exp(-r * T_mat);

  double half_sigma_sq = 0.5 * sigma * sigma;
  double rho_complement = std::sqrt(1.0 - rho * rho);

  // When lambda_T == 0 the impact state cannot enter the dynamics of S, so it
  // is not simulated.  This is now only a saving of work: the price stream is
  // separate, so skipping the impact stream does not shift it.
  bool impact_active = (lambda_T != 0.0);

  std::vector<int> state_S, state_I;
  if (seed != 0) {
    state_S = rng_seed_state(seed);
    // A fixed, non-zero scramble of the user's seed, so the impact stream is
    // reproducible from `seed` alone yet never coincides with the price stream.
    state_I = rng_seed_state(static_cast<int>(
        static_cast<unsigned int>(seed) ^ 0x9E3779B9u));
  } else {
    // Unseeded: continue the session's stream for the prices, and take one
    // draw off it to start the impact stream.  Nothing can be compared across
    // calls in this case anyway, since neither this routine nor the
    // Kemna-Vorst benchmark fixes a seed.
    state_S = rng_save();
    rng_restore(state_S);
    double u = unif_rand();
    state_S = rng_save();
    state_I = rng_seed_state(
        static_cast<int>(u * 2147483000.0) + 1);
  }

  NumericVector arith_payoffs(n_sims);
  NumericVector geom_payoffs(n_sims);

  std::vector<double> zS((size_t)RNG_BLOCK * n_steps);
  std::vector<double> zI(impact_active ? (size_t)RNG_BLOCK * n_steps : 0);

  for (int j0 = 0; j0 < n_sims; j0 += RNG_BLOCK) {
    int nb = std::min(RNG_BLOCK, n_sims - j0);
    size_t ndraw = (size_t)nb * n_steps;

    rng_restore(state_S);
    for (size_t t = 0; t < ndraw; t++) zS[t] = norm_rand();
    state_S = rng_save();

    if (impact_active) {
      rng_restore(state_I);
      for (size_t t = 0; t < ndraw; t++) zI[t] = norm_rand();
      state_I = rng_save();
    }

    for (int b = 0; b < nb; b++) {
      int j = j0 + b;
      const double* z1 = &zS[(size_t)b * n_steps];
      const double* z2 = impact_active ? &zI[(size_t)b * n_steps] : NULL;

      double S = S0;
      double I = I0;
      // Discretely monitored average over the n_steps + 1 fixings
      // t_0, ..., t_n: the running sum is seeded with S(t_0) = S0 and each
      // simulated node is added after S is advanced, so the terminal fixing
      // S(t_n) = S_T is included.  This matches both the geometric leg below
      // and price_kemna_vorst_arithmetic().
      double sum_S = S0;
      double sum_log_S = std::log(S0);

      for (int m = 0; m < n_steps; m++) {
        double N1 = z1[m];
        double Z1 = N1;

        if (impact_active) {
          double Z2 = rho * N1 + rho_complement * z2[m];
          I = I + (-kappa * I) * dt + eta_values[m] * Z2 * sqrt_dt;
        }

        double log_S_increment =
            (r - half_sigma_sq + lambda_T * I) * dt + sigma * Z1 * sqrt_dt;
        S = S * std::exp(log_S_increment);

        sum_S += S;
        sum_log_S += std::log(S);
      }

      double A = sum_S / (n_steps + 1);
      double G = std::exp(sum_log_S / (n_steps + 1));

      if (option_type == "call") {
        arith_payoffs[j] = discount * std::max(0.0, A - K);
        geom_payoffs[j] = discount * std::max(0.0, G - K);
      } else {
        arith_payoffs[j] = discount * std::max(0.0, K - A);
        geom_payoffs[j] = discount * std::max(0.0, K - G);
      }
    }
  }

  double mean_arith = Rcpp::mean(arith_payoffs);
  double std_arith = Rcpp::sd(arith_payoffs);

  double price_estimate;
  double std_error;
  double geom_mc_price = Rcpp::mean(geom_payoffs);

  if (use_control_variate) {
    NumericVector diffs = arith_payoffs - geom_payoffs;
    double mean_diff = Rcpp::mean(diffs);
    double std_diff = Rcpp::sd(diffs);

    price_estimate = mean_diff;
    std_error = std_diff / std::sqrt((double)n_sims);
  } else {
    price_estimate = mean_arith;
    std_error = std_arith / std::sqrt((double)n_sims);
  }

  double correlation = 0.0;
  {
    double mean_a = Rcpp::mean(arith_payoffs);
    double mean_g = Rcpp::mean(geom_payoffs);
    double cov = 0.0, var_a = 0.0, var_g = 0.0;
    for (int j = 0; j < n_sims; j++) {
      double da = arith_payoffs[j] - mean_a;
      double dg = geom_payoffs[j] - mean_g;
      cov += da * dg;
      var_a += da * da;
      var_g += dg * dg;
    }
    if (var_a > 0 && var_g > 0) {
      correlation = cov / std::sqrt(var_a * var_g);
    }
  }

  return List::create(
    Named("price_estimate") = price_estimate,
    Named("std_error") = std_error,
    Named("geom_mc_price") = geom_mc_price,
    Named("raw_arith_price") = mean_arith,
    Named("raw_arith_se") = std_arith / std::sqrt((double)n_sims),
    Named("correlation") = correlation,
    Named("n_sims") = n_sims,
    Named("n_steps") = n_steps
  );
}
