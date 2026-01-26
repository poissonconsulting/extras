library(testthat)

# Test Beta-Binomial Quantile Function
test_that("quant_beta_binom works correctly", {
  # Test theta = 0 case (should revert to binomial)
  result <- quant_beta_binom(c(0.1, 0.4, 0.6), size = 3, prob = 0.5, theta = 0)
  expected <- quant_binom(c(0.1, 0.4, 0.6), size = 3, prob = 0.5)
  expect_equal(result, expected)

  # Test that theta > 0 throws error (not implemented yet)
  expect_error(quant_beta_binom(c(0.1, 0.4, 0.6), size = 3, prob = 0.5, theta = 0.1),
               "Quantile function for the beta-binomial distribution not currently implemented")

  # Test edge cases
  expect_equal(quant_beta_binom(0, size = 3, prob = 0.5, theta = 0), 0)
  expect_equal(quant_beta_binom(1, size = 3, prob = 0.5, theta = 0), 3)
})

# Test Bernoulli Quantile Function
test_that("quant_bern works correctly", {
  # Test basic functionality
  result <- quant_bern(c(0.1, 0.4, 0.6, 0.9), prob = 0.7)
  expect_type(result, "double")
  expect_length(result, 4)

  # Test edge cases
  expect_equal(quant_bern(0, prob = 0.5), 0)
  expect_equal(quant_bern(1, prob = 0.5), 1)

  # Test with different probabilities
  expect_equal(quant_bern(0.1, prob = 0.9), 0)
  expect_equal(quant_bern(0.95, prob = 0.9), 1)
})

# Test Binomial Quantile Function
test_that("quant_binom works correctly", {
  # Test basic functionality
  result <- quant_binom(c(0.1, 0.4, 0.6), size = 2, prob = 0.3)
  expect_type(result, "double")
  expect_length(result, 3L)
  expect_true(all(result >= 0 & result <= 2))

  # Test edge cases
  expect_equal(quant_binom(0, size = 5, prob = 0.5), 0)
  expect_equal(quant_binom(1, size = 5, prob = 0.5), 5)

  # Test with prob = 0 and prob = 1
  expect_equal(quant_binom(0.5, size = 3, prob = 0), 0)
  expect_equal(quant_binom(0.5, size = 3, prob = 1), 3)

  # Test vector inputs
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  result <- quant_binom(probs, size = 10, prob = 0.5)
  expect_length(result, 5)
  expect_true(all(result >= 0 & result <= 10))
})

# Test Gamma Quantile Function
test_that("quant_gamma works correctly", {
  # Test basic functionality
  result <- quant_gamma(c(0.1, 0.4, 0.6), shape = 1, rate = 2)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result > 0))

  # Test edge cases
  expect_equal(quant_gamma(0, shape = 1, rate = 1), 0)
  expect_equal(quant_gamma(1, shape = 1, rate = 1), Inf)

  # Test with different parameters
  result1 <- quant_gamma(0.5, shape = 2, rate = 1)
  result2 <- quant_gamma(0.5, shape = 1, rate = 2)
  expect_true(result1 != result2)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_gamma(probs, shape = 2, rate = 1)
  expect_true(all(diff(result) > 0))
})

# Test Gamma-Poisson Quantile Function
test_that("quant_gamma_pois works correctly", {
  # Test that it calls quant_neg_binom
  result1 <- quant_gamma_pois(c(0.1, 0.4, 0.6), lambda = 1, theta = 1)
  result2 <- quant_neg_binom(c(0.1, 0.4, 0.6), lambda = 1, theta = 1)
  expect_equal(result1, result2)

  # Test basic functionality
  result <- quant_gamma_pois(c(0.1, 0.4, 0.6), lambda = 2, theta = 0.5)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0))
})

# Test Zero-Inflated Gamma-Poisson Quantile Function
test_that("quant_gamma_pois_zi works correctly", {
  # Test basic functionality
  result <- quant_gamma_pois_zi(c(0.1, 0.4, 0.6), lambda = 3, theta = 1, prob = 0.5)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0))

  # Test with prob = 0 (no zero inflation)
  result1 <- quant_gamma_pois_zi(c(0.1, 0.4, 0.6), lambda = 2, theta = 0.5, prob = 0)
  result2 <- quant_neg_binom(c(0.1, 0.4, 0.6), lambda = 2, theta = 0.5)
  expect_equal(result1, result2)

  # Test edge cases
  expect_equal(quant_gamma_pois_zi(0, lambda = 1, theta = 1, prob = 0), 0)
})

# Test Log-Normal Quantile Function
test_that("quant_lnorm works correctly", {
  # Test basic functionality
  result <- quant_lnorm(c(0.1, 0.4, 0.6), meanlog = 0, sdlog = 2)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result > 0))

  # Test edge cases
  expect_equal(quant_lnorm(0, meanlog = 0, sdlog = 1), 0)
  expect_equal(quant_lnorm(1, meanlog = 0, sdlog = 1), Inf)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_lnorm(probs, meanlog = 1, sdlog = 0.5)
  expect_true(all(diff(result) > 0))

  # Test parameter effects
  result1 <- quant_lnorm(0.5, meanlog = 0, sdlog = 1)
  result2 <- quant_lnorm(0.5, meanlog = 1, sdlog = 1)
  expect_true(result2 > result1)
})

# Test Negative Binomial Quantile Function
test_that("quant_neg_binom works correctly", {
  # Test basic functionality
  result <- quant_neg_binom(c(0.1, 0.4, 0.6), lambda = 2, theta = 1)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0))

  # Test edge cases
  expect_equal(quant_neg_binom(0, lambda = 1, theta = 1), 0)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_neg_binom(probs, lambda = 3, theta = 0.5)
  expect_true(all(diff(result) >= 0))

  # Test parameter effects
  result1 <- quant_neg_binom(0.5, lambda = 1, theta = 1)
  result2 <- quant_neg_binom(0.5, lambda = 2, theta = 1)
  expect_true(result2 >= result1)
})

# Test Normal Quantile Function
test_that("quant_norm works correctly", {
  # Test basic functionality
  result <- quant_norm(c(0.1, 0.4, 0.6))
  expect_type(result, "double")
  expect_length(result, 3)

  # Test standard normal properties
  expect_equal(quant_norm(0.5), 0, tolerance = 1e-10)
  expect_true(quant_norm(0.1) < 0)
  expect_true(quant_norm(0.9) > 0)

  # Test edge cases
  expect_equal(quant_norm(0), -Inf)
  expect_equal(quant_norm(1), Inf)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_norm(probs, mean = 2, sd = 3)
  expect_true(all(diff(result) > 0))

  # Test parameter effects
  expect_equal(quant_norm(0.5, mean = 5, sd = 1), 5)
  expect_true(quant_norm(0.75, mean = 0, sd = 2) > quant_norm(0.75, mean = 0, sd = 1))
})

# Test Poisson Quantile Function
test_that("quant_pois works correctly", {
  # Test basic functionality
  result <- quant_pois(c(0.1, 0.4, 0.6), lambda = 3)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0))

  # Test edge cases
  expect_equal(quant_pois(0, lambda = 1), 0)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_pois(probs, lambda = 5)
  expect_true(all(diff(result) >= 0))

  # Test parameter effects
  result1 <- quant_pois(0.5, lambda = 1)
  result2 <- quant_pois(0.5, lambda = 5)
  expect_true(result2 >= result1)

  # Test low lambda case
  result <- quant_pois(c(0.1, 0.5, 0.9), lambda = 0.1)
  expect_true(all(result >= 0))
})

# Test Zero-Inflated Poisson Quantile Function
test_that("quant_pois_zi works correctly", {
  # Test basic functionality
  result <- quant_pois_zi(c(0.1, 0.4, 0.6), lambda = 3, prob = 0.5)
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(result >= 0))

  # Test with prob = 0 (no zero inflation)
  result1 <- quant_pois_zi(c(0.1, 0.4, 0.6), lambda = 2, prob = 0)
  result2 <- quant_pois(c(0.1, 0.4, 0.6), lambda = 2)
  expect_equal(result1, result2)

  # Test edge cases
  expect_equal(quant_pois_zi(0, lambda = 1, prob = 0), 0)

  # Test high zero inflation
  result <- quant_pois_zi(c(0.1, 0.4, 0.6), lambda = 5, prob = 0.9)
  expect_true(all(result >= 0))
})

# Test Skew Normal Quantile Function
test_that("quant_skewnorm works correctly", {
  skip_if_not_installed("sn")

  # Test basic functionality
  result <- quant_skewnorm(c(0.1, 0.4, 0.6))
  expect_type(result, "double")
  expect_length(result, 3)

  # Test with shape = 0 (should be similar to normal)
  result1 <- quant_skewnorm(0.5, mean = 0, sd = 1, shape = 0)
  result2 <- quant_norm(0.5, mean = 0, sd = 1)
  expect_equal(result1, result2, tolerance = 1e-10)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_skewnorm(probs, shape = 2)
  expect_true(all(diff(result) > 0))

  # Test different skewness values
  result_neg <- quant_skewnorm(0.5, shape = -2)
  result_pos <- quant_skewnorm(0.5, shape = 2)
  result_zero <- quant_skewnorm(0.5, shape = 0)
  expect_true(result_neg != result_pos)
})

# Test Student's t Quantile Function
test_that("quant_student works correctly", {
  # Test basic functionality
  result <- quant_student(c(0.1, 0.4, 0.6), mean = 1, sd = 2, theta = 1/3)
  expect_type(result, "double")
  expect_length(result, 3)

  # Test edge cases
  expect_equal(quant_student(0.5, mean = 5, sd = 1, theta = 0), 5)

  # Test monotonicity
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result <- quant_student(probs, mean = 0, sd = 1, theta = 0.1)
  expect_true(all(diff(result) > 0))

  # Test parameter effects
  result1 <- quant_student(0.5, mean = 0, sd = 1, theta = 0.1)
  result2 <- quant_student(0.5, mean = 2, sd = 1, theta = 0.1)
  expect_equal(result2 - result1, 2, tolerance = 1e-10)

  # Test that higher theta (lower df) gives more extreme values
  result_low_df <- quant_student(0.9, mean = 0, sd = 1, theta = 1)
  result_high_df <- quant_student(0.9, mean = 0, sd = 1, theta = 0.01)
  expect_true(abs(result_low_df) > abs(result_high_df))

  # Test parameter validation would require chk package functions
  # These tests assume the chk package functions work correctly
})

# Test input validation for all functions
test_that("functions handle invalid inputs appropriately", {
  # Test empty vectors
  expect_length(quant_norm(numeric(0)), 0)
  expect_length(quant_pois(numeric(0), lambda = 1), 0)

  # Test NA values
  expect_true(is.na(quant_norm(NA)))
  expect_true(is.na(quant_pois(NA, lambda = 1)))

  # Test out of bounds probabilities
  expect_warning(quant_norm(-0.1))
  expect_warning(quant_norm(1.1))

  # Test negative parameters where not allowed
  expect_warning(quant_gamma(0.5, shape = -1, rate = 1))
  expect_warning(quant_lnorm(0.5, meanlog = 0, sdlog = -1))
})

# Test consistency across related functions
test_that("related functions are consistent", {
  # Gamma-Poisson should equal Negative Binomial
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  result1 <- quant_gamma_pois(probs, lambda = 2, theta = 0.5)
  result2 <- quant_neg_binom(probs, lambda = 2, theta = 0.5)
  expect_equal(result1, result2)

  # Zero-inflated functions with prob = 0 should equal non-zero-inflated
  result1 <- quant_pois_zi(probs, lambda = 3, prob = 0)
  result2 <- quant_pois(probs, lambda = 3)
  expect_equal(result1, result2)

  result1 <- quant_gamma_pois_zi(probs, lambda = 2, theta = 1, prob = 0)
  result2 <- quant_neg_binom(probs, lambda = 2, theta = 1)
  expect_equal(result1, result2)
})
