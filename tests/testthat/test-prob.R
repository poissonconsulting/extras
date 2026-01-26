library(testthat)

# Test Beta-Binomial Distribution
test_that("prob_beta_binom works correctly", {
  skip_if_not_installed("extraDistr")

  # Basic functionality
  result <- prob_beta_binom(c(0, 1, 2), size = 3, prob = 0.5, theta = 0.1)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # When theta = 0, should revert to binomial
  binom_result <- prob_binom(c(0, 1, 2), size = 3, prob = 0.5)
  bb_result <- prob_beta_binom(c(0, 1, 2), size = 3, prob = 0.5, theta = 0)
  expect_equal(bb_result, binom_result, tolerance = 1e-10)

  # Monotonicity
  x_vals <- 0:5
  result <- prob_beta_binom(x_vals, size = 5, prob = 0.3, theta = 0.2)
  expect_true(all(diff(result) >= 0))

  # Edge cases
  expect_equal(prob_beta_binom(0, size = 0, prob = 0.5, theta = 0.1), 1)
  expect_true(prob_beta_binom(-1, size = 1, prob = 0.5, theta = 0.1) == 0)
  expect_equal(prob_beta_binom(5, size = 3, prob = 0.5, theta = 0.1), 1)
})

# Test Bernoulli Distribution
test_that("prob_bern works correctly", {
  # Basic functionality
  result <- prob_bern(c(TRUE, FALSE), prob = 0.7)
  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(all(result >= 0 & result <= 1))

  # Logical equivalence
  expect_equal(prob_bern(c(0, 1), prob = 0.3), prob_binom(c(0, 1), size = 1, prob = 0.3))

  # Edge cases
  expect_equal(prob_bern(TRUE, prob = 0), 1)
  expect_equal(prob_bern(FALSE, prob = 0), 1)
  expect_equal(prob_bern(TRUE, prob = 1), 1)
  expect_equal(prob_bern(FALSE, prob = 1), 0)
})

# Test Binomial Distribution
test_that("prob_binom works correctly", {
  # Basic functionality
  result <- prob_binom(c(0, 1, 2), size = 2, prob = 0.3)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- 0:10
  result <- prob_binom(x_vals, size = 10, prob = 0.5)
  expect_true(all(diff(result) >= 0))

  # Edge cases
  expect_equal(prob_binom(0, size = 5, prob = 0), 1)
  expect_equal(prob_binom(5, size = 5, prob = 1), 1)

  # Compare with base R
  expect_equal(prob_binom(3, size = 10, prob = 0.4), pbinom(3, 10, 0.4))
})

# Test Gamma Distribution
test_that("prob_gamma works correctly", {
  # Basic functionality
  result <- prob_gamma(c(0, 1, 2), shape = 1, rate = 2)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- seq(0, 5, 0.5)
  result <- prob_gamma(x_vals, shape = 2, rate = 1)
  expect_true(all(diff(result) >= 0))

  # Compare with base R
  expect_equal(prob_gamma(2.5, shape = 3, rate = 0.5), pgamma(2.5, 3, 0.5))

  # Edge cases
  expect_equal(prob_gamma(0, shape = 1, rate = 1), 0)
})

# Test Gamma-Poisson Distribution
test_that("prob_gamma_pois works correctly", {
  # Basic functionality
  result <- prob_gamma_pois(c(0, 1, 2), lambda = 1, theta = 1)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Should be equivalent to negative binomial
  expect_equal(prob_gamma_pois(c(0, 1, 2), lambda = 2, theta = 0.5),
               prob_neg_binom(c(0, 1, 2), lambda = 2, theta = 0.5))

  # Monotonicity
  x_vals <- 0:10
  result <- prob_gamma_pois(x_vals, lambda = 3, theta = 0.2)
  expect_true(all(diff(result) >= 0))
})

# Test Zero-Inflated Gamma-Poisson Distribution
test_that("prob_gamma_pois_zi works correctly", {
  # Basic functionality
  result <- prob_gamma_pois_zi(c(1, 3, 4), lambda = 3, theta = 1, prob = 0.5)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # When prob = 0, should equal regular gamma-poisson
  regular <- prob_gamma_pois(c(1, 2, 3), lambda = 2, theta = 0.3)
  zi_no_inflation <- prob_gamma_pois_zi(c(1, 2, 3), lambda = 2, theta = 0.3, prob = 0)
  expect_equal(zi_no_inflation, regular)

  # Monotonicity for positive values
  x_vals <- 1:10
  result <- prob_gamma_pois_zi(x_vals, lambda = 2, theta = 0.1, prob = 0.3)
  expect_true(all(diff(result) >= 0))
})

# Test Log-Normal Distribution
test_that("prob_lnorm works correctly", {
  # Basic functionality
  result <- prob_lnorm(10, meanlog = 0, sdlog = 2)
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result >= 0 & result <= 1)

  # Vector input
  result <- prob_lnorm(c(1, 5, 10), meanlog = 1, sdlog = 0.5)
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- seq(0.1, 10, 0.5)
  result <- prob_lnorm(x_vals, meanlog = 0, sdlog = 1)
  expect_true(all(diff(result) >= 0))

  # Compare with base R
  expect_equal(prob_lnorm(5, meanlog = 1, sdlog = 0.5), plnorm(5, 1, 0.5))
})

# Test Negative Binomial Distribution
test_that("prob_neg_binom works correctly", {
  # Basic functionality
  result <- prob_neg_binom(c(0, 1, 2), lambda = 2, theta = 1)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- 0:15
  result <- prob_neg_binom(x_vals, lambda = 5, theta = 0.5)
  expect_true(all(diff(result) >= 0))

  # Compare with base R
  expect_equal(prob_neg_binom(3, lambda = 4, theta = 0.2), pnbinom(3, mu = 4, size = 1/0.2))

  # When theta approaches 0, should approach Poisson
  pois_result <- ppois(5, lambda = 3)
  nb_result <- prob_neg_binom(5, lambda = 3, theta = 1e-10)
  expect_equal(nb_result, pois_result, tolerance = 1e-8)
})

# Test Normal Distribution
test_that("prob_norm works correctly", {
  # Basic functionality
  result <- prob_norm(c(-2:2))
  expect_type(result, "double")
  expect_length(result, 5)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- seq(-5, 5, 0.5)
  result <- prob_norm(x_vals, mean = 0, sd = 1)
  expect_true(all(diff(result) >= 0))

  # Standard normal properties
  expect_equal(prob_norm(0), 0.5)
  expect_true(prob_norm(-1.96) < 0.05)
  expect_true(prob_norm(1.96) > 0.95)

  # Compare with base R
  expect_equal(prob_norm(1.5, mean = 2, sd = 0.5), pnorm(1.5, 2, 0.5))
})

# Test Poisson Distribution
test_that("prob_pois works correctly", {
  # Basic functionality
  result <- prob_pois(c(1, 3, 4), lambda = 3)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- 0:20
  result <- prob_pois(x_vals, lambda = 5)
  expect_true(all(diff(result) >= 0))

  # Compare with base R
  expect_equal(prob_pois(7, lambda = 4), ppois(7, 4))

  # Edge case
  expect_equal(prob_pois(0, lambda = 0), 1)
})

# Test Zero-Inflated Poisson Distribution
test_that("prob_pois_zi works correctly", {
  # Basic functionality
  result <- prob_pois_zi(c(1, 3, 4), lambda = 3, prob = 0.5)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # When prob = 0, should equal regular Poisson
  regular <- prob_pois(c(1, 2, 3), lambda = 4)
  zi_no_inflation <- prob_pois_zi(c(1, 2, 3), lambda = 4, prob = 0)
  expect_equal(zi_no_inflation, regular)

  # Monotonicity for positive values
  x_vals <- 1:15
  result <- prob_pois_zi(x_vals, lambda = 3, prob = 0.2)
  expect_true(all(diff(result) >= 0))

  # Zero inflation effect
  zi_result <- prob_pois_zi(0, lambda = 2, prob = 0.3)
  regular_result <- prob_pois(0, lambda = 2)
  expect_true(zi_result > regular_result)
})

# Test Skew Normal Distribution
test_that("prob_skewnorm works correctly", {
  skip_if_not_installed("sn")

  # Basic functionality
  result <- prob_skewnorm(c(-2:2))
  expect_type(result, "double")
  expect_length(result, 5)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- seq(-5, 5, 0.5)
  result <- prob_skewnorm(x_vals, mean = 0, sd = 1, shape = 0)
  expect_true(all(diff(result) >= 0))

  # When shape = 0, should equal normal
  normal_result <- prob_norm(c(-1, 0, 1), mean = 2, sd = 1.5)
  skew_result <- prob_skewnorm(c(-1, 0, 1), mean = 2, sd = 1.5, shape = 0)
  expect_equal(skew_result, normal_result, tolerance = 1e-10)

  # Different skewness
  pos_skew <- prob_skewnorm(1, shape = 2)
  neg_skew <- prob_skewnorm(1, shape = -2)
  normal_val <- prob_skewnorm(1, shape = 0)
  expect_true(pos_skew != neg_skew)
})

# Test Student's t Distribution
test_that("prob_student works correctly", {
  # Basic functionality
  result <- prob_student(c(1, 3.5, 4), mean = 1, sd = 2, theta = 1/3)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))

  # Monotonicity
  x_vals <- seq(-5, 5, 0.5)
  result <- prob_student(x_vals, mean = 0, sd = 1, theta = 0.1)
  expect_true(all(diff(result) >= 0))

  # Error handling for negative sd
  expect_error(prob_student(1, mean = 0, sd = -1, theta = 0.5))

  # Compare with base R t-distribution
  df <- 1 / 0.2  # theta = 0.2
  expected <- pt((2 - 1) / 1.5, df)  # (x - mean) / sd
  result <- prob_student(2, mean = 1, sd = 1.5, theta = 0.2)
  expect_equal(result, expected)

  # As theta approaches 0 (df approaches infinity), should approach normal
  large_df_result <- prob_student(1, mean = 0, sd = 1, theta = 1e-10)
  normal_result <- prob_norm(1, mean = 0, sd = 1)
  expect_equal(large_df_result, normal_result, tolerance = 1e-6)
})

# Test parameter validation and edge cases
test_that("functions handle edge cases and invalid parameters", {
  # Test with empty vectors
  expect_length(prob_norm(numeric(0)), 0)
  expect_length(prob_pois(integer(0), lambda = 1), 0)

  # Test with NA values
  expect_true(is.na(prob_norm(NA)))
  expect_true(is.na(prob_pois(NA, lambda = 1)))

  # Test with Inf values
  expect_equal(prob_norm(Inf), 1)
  expect_equal(prob_norm(-Inf), 0)

  # Test parameter bounds
  expect_true(all(prob_binom(0:5, size = 5, prob = 0) >= 0))
  expect_true(all(prob_binom(0:5, size = 5, prob = 1) >= 0))
})

# Integration tests - ensure CDFs have proper properties
test_that("CDFs satisfy mathematical properties", {
  # CDF should be non-decreasing
  test_monotonicity <- function(f, x_vals, ...) {
    result <- f(x_vals, ...)
    all(diff(result) >= -1e-10)  # Allow for small numerical errors
  }

  # Test several distributions
  expect_true(test_monotonicity(prob_norm, seq(-3, 3, 0.1)))
  expect_true(test_monotonicity(prob_pois, 0:20, lambda = 5))
  expect_true(test_monotonicity(prob_gamma, seq(0, 10, 0.1), shape = 2, rate = 1))

  # CDF should approach 1 for large values (for unbounded distributions)
  expect_true(prob_norm(10) > 0.99)
  expect_true(prob_pois(50, lambda = 5) > 0.99)
  expect_true(prob_gamma(50, shape = 1, rate = 1) > 0.99)

  # CDF should be 0 or close to 0 for very small values where applicable
  expect_equal(prob_gamma(0, shape = 1, rate = 1), 0)
  expect_true(prob_norm(-10) < 0.01)
})
