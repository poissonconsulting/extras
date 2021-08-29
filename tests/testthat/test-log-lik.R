test_that("log_lik_pois", {
  expect_identical(log_lik_pois(1, 2), dpois(1, 2, log = TRUE))
})

test_that("log_lik_pois_zi", {
  expect_identical(log_lik_pois_zi(1, 2), dpois(1, 2, log = TRUE))
  expect_identical(log_lik_pois_zi(0, 2), dpois(0, 2, log = TRUE))
  expect_identical(log_lik_pois_zi(0, 2, 1), 0)
  expect_identical(log_lik_pois_zi(1, 2, 1), -Inf)
  expect_equal(log_lik_pois_zi(c(0, 2), 2, 0.5), c(-0.566219169516973, -2))
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

test_that("log_lik_neg_binom", {
  expect_identical(log_lik_neg_binom(0, 2, 1), dnbinom(0, mu = 2, size = 1, log = TRUE))
  expect_identical(log_lik_neg_binom(0, 2, 2), dnbinom(0, size = 1/2, mu = 2, log = TRUE))
})

test_that("log_lik_gamma_pois", {
  expect_identical(log_lik_gamma_pois(0, 2, 2), dnbinom(0, mu = 2, size = 1/2, log = TRUE))
})
