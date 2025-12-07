test_that("Arithmetic bounds satisfy inequality", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds$lower_bound <= bounds$upper_bound)
})

test_that("Arithmetic bounds are non-negative", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds$lower_bound >= 0)
  expect_true(bounds$upper_bound >= 0)
  expect_true(bounds$EQ_G >= 0)
})

test_that("Rho star is at least 1", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds$rho_star >= 1)
})

test_that("Lower bound equals geometric option price", {
  V_G <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_equal(bounds$lower_bound, V_G, tolerance = 1e-10)
  expect_equal(bounds$V0_G, V_G, tolerance = 1e-10)
})

test_that("Bounds object has correct structure", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_type(bounds, "list")
  expect_true("lower_bound" %in% names(bounds))
  expect_true("upper_bound" %in% names(bounds))
  expect_true("upper_bound_global" %in% names(bounds))
  expect_true("rho_star" %in% names(bounds))
  expect_true("EQ_G" %in% names(bounds))
  expect_true("V0_G" %in% names(bounds))

  expect_true(is.numeric(bounds$lower_bound))
  expect_true(is.numeric(bounds$upper_bound))
  expect_true(is.numeric(bounds$rho_star))
  expect_true(is.numeric(bounds$EQ_G))
  expect_true(is.numeric(bounds$V0_G))

  expect_length(bounds$lower_bound, 1)
  expect_length(bounds$upper_bound, 1)
})

test_that("Bounds object has correct class", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_s3_class(bounds, "arithmetic_bounds")
  expect_s3_class(bounds, "list")
})

test_that("Print method works for arithmetic_bounds", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_output(print(bounds), "Arithmetic Asian Option Bounds")
  expect_output(print(bounds), "Lower bound")
  expect_output(print(bounds), "Upper bound")
  expect_output(print(bounds), "Midpoint")
  expect_output(print(bounds), "Spread")
  expect_output(print(bounds), "E\\^Q\\[G_n\\]")
})

test_that("Bounds tighten with lower volatility", {
  bounds_high <- arithmetic_asian_bounds(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 3)
  spread_high <- bounds_high$upper_bound - bounds_high$lower_bound

  bounds_low <- arithmetic_asian_bounds(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 3)
  spread_low <- bounds_low$upper_bound - bounds_low$lower_bound

  expect_true(spread_low < spread_high)
})

test_that("Expected geometric average is positive", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds$EQ_G > 0)
  expect_false(is.na(bounds$EQ_G))
  expect_false(is.infinite(bounds$EQ_G))
})

test_that("Bounds scale with initial stock price", {

  bounds_100 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  bounds_200 <- arithmetic_asian_bounds(200, 200, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds_200$lower_bound > bounds_100$lower_bound)
  expect_true(bounds_200$upper_bound > bounds_100$upper_bound)
  expect_true(bounds_200$EQ_G > bounds_100$EQ_G)
})

test_that("ITM options have higher bounds", {

  bounds_itm <- arithmetic_asian_bounds(100, 80, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  bounds_atm <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  bounds_otm <- arithmetic_asian_bounds(100, 120, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds_itm$lower_bound > bounds_atm$lower_bound)
  expect_true(bounds_atm$lower_bound > bounds_otm$lower_bound)
})

test_that("Rho star increases with volatility spread", {

  bounds_low <- arithmetic_asian_bounds(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 3)

  bounds_high <- arithmetic_asian_bounds(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 3)

  expect_true(bounds_high$rho_star > bounds_low$rho_star)
})

test_that("Bounds work for n=1 case", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1)

  expect_true(bounds$lower_bound >= 0)
  expect_true(bounds$upper_bound >= bounds$lower_bound)
  expect_true(bounds$rho_star >= 1)
})

test_that("Bounds work for various n values", {
  for (n in c(1, 2, 3, 5, 8, 10)) {
    bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n)

    expect_true(bounds$lower_bound >= 0, info = paste("n =", n))
    expect_true(bounds$upper_bound >= bounds$lower_bound, info = paste("n =", n))
    expect_true(bounds$rho_star >= 1, info = paste("n =", n))
    expect_true(bounds$EQ_G > 0, info = paste("n =", n))
  }
})

test_that("Price impact increases bounds", {
  bounds_no <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 3)

  bounds_yes <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_true(bounds_yes$lower_bound >= bounds_no$lower_bound)
})

test_that("Midpoint is between bounds", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  midpoint <- mean(c(bounds$lower_bound, bounds$upper_bound))

  expect_true(midpoint >= bounds$lower_bound)
  expect_true(midpoint <= bounds$upper_bound)
})

test_that("Results are reproducible", {
  bounds1 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)
  bounds2 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3)

  expect_equal(bounds1$lower_bound, bounds2$lower_bound)
  expect_equal(bounds1$upper_bound, bounds2$upper_bound)
  expect_equal(bounds1$rho_star, bounds2$rho_star)
  expect_equal(bounds1$EQ_G, bounds2$EQ_G)
})

test_that("Put option bounds satisfy inequality", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_true(bounds$lower_bound <= bounds$upper_bound)
})

test_that("Put option bounds are non-negative", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_true(bounds$lower_bound >= 0)
  expect_true(bounds$upper_bound >= 0)
  expect_true(bounds$EQ_G >= 0)
})

test_that("Put lower bound equals geometric put price", {
  V_G_put <- price_geometric_asian(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                    option_type = "put")
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_equal(bounds$lower_bound, V_G_put, tolerance = 1e-10)
  expect_equal(bounds$V0_G, V_G_put, tolerance = 1e-10)
})

test_that("Put bounds increase with strike", {
  bounds_K90 <- arithmetic_asian_bounds(100, 90, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                         option_type = "put")
  bounds_K100 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                          option_type = "put")
  bounds_K110 <- arithmetic_asian_bounds(100, 110, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                          option_type = "put")

  expect_true(bounds_K90$lower_bound < bounds_K100$lower_bound)
  expect_true(bounds_K100$lower_bound < bounds_K110$lower_bound)
})

test_that("Put bounds decrease with initial stock price", {

  bounds_S90 <- arithmetic_asian_bounds(90, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                         option_type = "put")
  bounds_S100 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                          option_type = "put")
  bounds_S110 <- arithmetic_asian_bounds(110, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                          option_type = "put")

  expect_true(bounds_S90$lower_bound > bounds_S100$lower_bound)
  expect_true(bounds_S100$lower_bound > bounds_S110$lower_bound)
})

test_that("Deep ITM put has high bounds", {

  bounds <- arithmetic_asian_bounds(100, 500, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_true(bounds$lower_bound > 200)
  expect_true(bounds$upper_bound > bounds$lower_bound)
})

test_that("Deep OTM put has low bounds", {

  bounds <- arithmetic_asian_bounds(100, 1, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_true(bounds$lower_bound < 1)
  expect_true(bounds$lower_bound >= 0)
})

test_that("Put rho star is at least 1", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  expect_true(bounds$rho_star >= 1)
})

test_that("Put bounds work for n=1 case", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1,
                                     option_type = "put")

  expect_true(bounds$lower_bound >= 0)
  expect_true(bounds$upper_bound >= bounds$lower_bound)
  expect_true(bounds$rho_star >= 1)
})

test_that("Put bounds work for various n values", {
  for (n in c(1, 2, 3, 5, 8)) {
    bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n,
                                       option_type = "put")

    expect_true(bounds$lower_bound >= 0, info = paste("n =", n))
    expect_true(bounds$upper_bound >= bounds$lower_bound, info = paste("n =", n))
    expect_true(bounds$rho_star >= 1, info = paste("n =", n))
    expect_true(bounds$EQ_G > 0, info = paste("n =", n))
  }
})

test_that("Put midpoint is between bounds", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                     option_type = "put")

  midpoint <- mean(c(bounds$lower_bound, bounds$upper_bound))

  expect_true(midpoint >= bounds$lower_bound)
  expect_true(midpoint <= bounds$upper_bound)
})

test_that("Put results are reproducible", {
  bounds1 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")
  bounds2 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                      option_type = "put")

  expect_equal(bounds1$lower_bound, bounds2$lower_bound)
  expect_equal(bounds1$upper_bound, bounds2$upper_bound)
  expect_equal(bounds1$rho_star, bounds2$rho_star)
  expect_equal(bounds1$EQ_G, bounds2$EQ_G)
})

test_that("option_type parameter validation works", {

  expect_no_error(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                            option_type = "call")
  )
  expect_no_error(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                            option_type = "put")
  )

  expect_error(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                            option_type = "invalid"),
    "should be one of"
  )
})

test_that("Call and put bounds use same rho_star and EQ_G", {

  bounds_call <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                          option_type = "call")
  bounds_put <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                         option_type = "put")

  expect_equal(bounds_call$rho_star, bounds_put$rho_star)
  expect_equal(bounds_call$EQ_G, bounds_put$EQ_G)

  expect_false(isTRUE(all.equal(bounds_call$lower_bound, bounds_put$lower_bound)))
})

# ============================================================================
# Tests for Path-Specific Upper Bound (Exact Enumeration)
# ============================================================================

test_that("Path-specific bound is computed when requested", {
  bounds_ps <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                        compute_path_specific = TRUE)

  expect_true(!is.na(bounds_ps$upper_bound_path_specific))
  expect_true(is.numeric(bounds_ps$upper_bound_path_specific))
  expect_true(bounds_ps$n_paths_used > 0)
})

test_that("Path-specific bound is NA when not requested", {
  bounds_no_ps <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                                           compute_path_specific = FALSE)

  expect_true(is.na(bounds_no_ps$upper_bound_path_specific))
  expect_equal(bounds_no_ps$n_paths_used, 0)
})

test_that("Path-specific bound satisfies ordering: lower <= path-specific <= global", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                     compute_path_specific = TRUE)

  expect_true(bounds$lower_bound <= bounds$upper_bound_path_specific)
  expect_true(bounds$upper_bound_path_specific <= bounds$upper_bound_global)
})

test_that("Path-specific bound is tighter than global bound", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 5,
                                     compute_path_specific = TRUE)

  spread_ps <- bounds$upper_bound_path_specific - bounds$lower_bound
  spread_global <- bounds$upper_bound_global - bounds$lower_bound

  expect_true(spread_ps <= spread_global)
})

test_that("Path-specific bound uses all 2^n paths", {
  n <- 5
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n,
                                     compute_path_specific = TRUE)

  expect_equal(bounds$n_paths_used, 2^n)
})

test_that("Path-specific bound works for n=1 case", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 1,
                                     compute_path_specific = TRUE)

  expect_true(!is.na(bounds$upper_bound_path_specific))
  expect_true(bounds$lower_bound <= bounds$upper_bound_path_specific)
  expect_true(bounds$upper_bound_path_specific <= bounds$upper_bound_global)
  expect_equal(bounds$n_paths_used, 2)
})

test_that("Path-specific bound validation: compute_path_specific must be logical", {
  expect_error(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                            compute_path_specific = "yes"),
    "compute_path_specific must be TRUE or FALSE"
  )

  expect_error(
    arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 3,
                            compute_path_specific = 1),
    "compute_path_specific must be TRUE or FALSE"
  )
})

test_that("Path-specific bound works for put options", {
  bounds_put <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                         option_type = "put",
                                         compute_path_specific = TRUE)

  expect_true(!is.na(bounds_put$upper_bound_path_specific))
  expect_true(bounds_put$lower_bound <= bounds_put$upper_bound_path_specific)
  expect_true(bounds_put$upper_bound_path_specific <=
                bounds_put$upper_bound_global)
})

test_that("Path-specific bound is tighter for put options", {
  bounds_put <- arithmetic_asian_bounds(100, 110, 1.05, 1.3, 0.7, 0.1, 1, 1, 5,
                                         option_type = "put",
                                         compute_path_specific = TRUE)

  spread_ps <- bounds_put$upper_bound_path_specific - bounds_put$lower_bound
  spread_global <- bounds_put$upper_bound_global - bounds_put$lower_bound

  expect_true(spread_ps <= spread_global)
})

test_that("Path-specific bound tightens with lower volatility", {

  bounds_high <- arithmetic_asian_bounds(100, 100, 1.05, 1.3, 0.7, 0.1, 1, 1, 5,
                                          compute_path_specific = TRUE)
  spread_high_ps <- bounds_high$upper_bound_path_specific -
                    bounds_high$lower_bound

  bounds_low <- arithmetic_asian_bounds(100, 100, 1.05, 1.1, 0.9, 0.1, 1, 1, 5,
                                         compute_path_specific = TRUE)
  spread_low_ps <- bounds_low$upper_bound_path_specific -
                   bounds_low$lower_bound

  expect_true(spread_low_ps < spread_high_ps)
})

test_that("Path-specific bound is non-negative", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                     compute_path_specific = TRUE)

  expect_true(bounds$upper_bound_path_specific >= 0)
})

test_that("Path-specific bound is reproducible (exact enumeration)", {
  bounds1 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 4,
                                      compute_path_specific = TRUE)
  bounds2 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 4,
                                      compute_path_specific = TRUE)

  expect_equal(bounds1$upper_bound_path_specific,
               bounds2$upper_bound_path_specific)
  expect_equal(bounds1$n_paths_used, bounds2$n_paths_used)
})

test_that("Path-specific bound improvement is significant for high volatility", {

  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.5, 0.6, 0.1, 1, 1, 5,
                                     compute_path_specific = TRUE)

  improvement <- (bounds$upper_bound_global - bounds$upper_bound_path_specific)

  expect_true(improvement > 0)
  expect_true(bounds$upper_bound_path_specific > bounds$lower_bound)
})

test_that("Path-specific bound works with no price impact (lambda=0)", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 4,
                                     compute_path_specific = TRUE)

  expect_true(!is.na(bounds$upper_bound_path_specific))
  expect_true(bounds$lower_bound <= bounds$upper_bound_path_specific)
  expect_true(bounds$upper_bound_path_specific <= bounds$upper_bound_global)
})

test_that("Path-specific bound increases with price impact", {

  bounds_no <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0, 0, 0, 5,
                                        compute_path_specific = TRUE)

  bounds_yes <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.2, 1, 1, 5,
                                         compute_path_specific = TRUE)

  expect_true(bounds_yes$upper_bound_path_specific >=
                bounds_no$upper_bound_path_specific)
})

test_that("Path-specific bound varies across moneyness levels", {

  bounds_itm <- arithmetic_asian_bounds(100, 80, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                         compute_path_specific = TRUE)

  bounds_atm <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                         compute_path_specific = TRUE)

  bounds_otm <- arithmetic_asian_bounds(100, 120, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                         compute_path_specific = TRUE)

  expect_true(!is.na(bounds_itm$upper_bound_path_specific))
  expect_true(!is.na(bounds_atm$upper_bound_path_specific))
  expect_true(!is.na(bounds_otm$upper_bound_path_specific))

  expect_true(bounds_itm$upper_bound_path_specific >
                bounds_otm$upper_bound_path_specific)
})

test_that("Path-specific bound structure is correct", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 4,
                                     compute_path_specific = TRUE)

  expect_true("upper_bound_path_specific" %in% names(bounds))
  expect_true("n_paths_used" %in% names(bounds))

  expect_true(is.numeric(bounds$upper_bound_path_specific))
  expect_true(is.numeric(bounds$n_paths_used))

  expect_length(bounds$upper_bound_path_specific, 1)
  expect_length(bounds$n_paths_used, 1)
})

test_that("Path-specific bound print method includes path-specific info", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 4,
                                     compute_path_specific = TRUE)

  expect_output(print(bounds), "Upper bound \\(path-spec\\)")
  expect_output(print(bounds), "using all .* paths")
  expect_output(print(bounds), "Midpoint \\(path-spec\\)")
})

test_that("Path-specific bound backward compatibility: upper_bound field exists", {
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 4,
                                     compute_path_specific = TRUE)

  expect_true("upper_bound" %in% names(bounds))
  expect_equal(bounds$upper_bound, bounds$upper_bound_global)
})

test_that("Path-specific bound works for various n values", {
  for (n in c(1, 2, 3, 5, 8)) {
    bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n,
                                       compute_path_specific = TRUE)

    expect_true(!is.na(bounds$upper_bound_path_specific),
                info = paste("n =", n))
    expect_true(bounds$lower_bound <= bounds$upper_bound_path_specific,
                info = paste("n =", n))
    expect_true(bounds$upper_bound_path_specific <= bounds$upper_bound_global,
                info = paste("n =", n))
    expect_true(bounds$n_paths_used > 0, info = paste("n =", n))
    expect_equal(bounds$n_paths_used, 2^n, info = paste("n =", n))
  }
})

test_that("Path-specific bound enumerates all paths for small n", {
  n <- 6
  bounds <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, n,
                                     compute_path_specific = TRUE)

  # Should use all 2^n paths
  expect_equal(bounds$n_paths_used, 2^n)
  expect_equal(bounds$n_paths_used, 64)
})

test_that("Path-specific bound is exact (not an estimate)", {
  # Since we enumerate all paths, running twice should give identical results
  bounds1 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                      compute_path_specific = TRUE)
  bounds2 <- arithmetic_asian_bounds(100, 100, 1.05, 1.2, 0.8, 0.1, 1, 1, 5,
                                      compute_path_specific = TRUE)

  # Should be exactly equal (no randomness)
  expect_identical(bounds1$upper_bound_path_specific,
                   bounds2$upper_bound_path_specific)
  expect_identical(bounds1$n_paths_used, bounds2$n_paths_used)
})
