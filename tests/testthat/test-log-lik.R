test_that("log_lik_pois", {
  expect_identical(log_lik_pois(1, 2), dpois(1, 2, log = TRUE))
})

test_that("log_lik_norm", {
  expect_identical(log_lik_norm(1, 2), dnorm(1, 2, log = TRUE))
})

test_that("log_lik_lnorm", {
  expect_identical(log_lik_lnorm(1, 2), dlnorm(1, 2, log = TRUE))
})

test_that("log_lik_binom", {
  expect_identical(log_lik_binom(1, 2, 0.7), dbinom(1, 2, 0.7, log = TRUE))
})

test_that("log_lik_bern", {
  expect_identical(log_lik_bern(1, 0.7), dbinom(1, 1, 0.7, log = TRUE))
})
