test_that("log_lik_bern", {
  expect_identical(log_lik_bern(numeric(0)), numeric(0))
  expect_identical(log_lik_bern(1, prob = numeric(0)), numeric(0))
  expect_identical(log_lik_bern(NA), NA_real_)
  expect_identical(log_lik_bern(1, NA), NA_real_)
  expect_identical(log_lik_bern(0, 0), 0)
  expect_identical(log_lik_bern(0L, 0), 0)
  expect_identical(log_lik_bern(1, 1), 0)
  expect_equal(log_lik_bern(0), -0.693147180559945)
  expect_equal(log_lik_bern(1, 0.7), -0.356674943938732)
})

test_that("log_lik_binom", {
  expect_identical(log_lik_binom(numeric(0)), numeric(0))
  expect_identical(log_lik_binom(1, numeric(0)), numeric(0))
  expect_identical(log_lik_binom(1, prob = numeric(0)), numeric(0))
  expect_identical(log_lik_binom(NA), NA_real_)
  expect_identical(log_lik_binom(1, NA), NA_real_)
  expect_identical(log_lik_binom(1, prob = NA), NA_real_)
  expect_identical(log_lik_binom(0, 0), 0)
  expect_identical(log_lik_binom(0L, 0), 0)
  expect_equal(log_lik_binom(1, 1), -0.693147180559945)
  expect_equal(log_lik_binom(0), -0.693147180559945)
  expect_equal(log_lik_binom(1, prob = 0.7), -0.356674943938732)

  expect_identical(log_lik_binom(1, 2, 0.7), dbinom(1, 2, 0.7, log = TRUE))
})


test_that("log_lik_pois", {
  expect_equal(log_lik_pois(1, 2), -1.30685281944005)
})

test_that("log_lik_pois_zi", {
  expect_identical(log_lik_pois_zi(1, 2), dpois(1, 2, log = TRUE))
  expect_identical(log_lik_pois_zi(0, 2), dpois(0, 2, log = TRUE))
  expect_identical(log_lik_pois_zi(0, 2, 1), 0)
  expect_identical(log_lik_pois_zi(1, 2, 1), -Inf)
  expect_equal(log_lik_pois_zi(c(0, 2), 2, 0.5), c(-0.566219169516973, -2))
  expect_equal(log_lik_pois_zi(3, 3.5, 0), log_lik_pois(3, 3.5))
  expect_equal(log_lik_pois_zi(3, 3.5, 0), -1.53347056374195)
  expect_equal(log_lik_pois_zi(3, 3.5, 0.1), -1.63883107939978)
  expect_equal(log_lik_pois_zi(3, 3.5, 0.2), -1.75661411505616)
  expect_equal(log_lik_pois_zi(3, 3.5, 1), -Inf)
})

test_that("log_lik_norm", {
  expect_identical(log_lik_norm(1, 2), dnorm(1, 2, log = TRUE))
})

test_that("log_lik_lnorm", {
  expect_identical(log_lik_lnorm(1, 2), dlnorm(1, 2, log = TRUE))
})

test_that("log_lik_neg_binom", {
  expect_identical(log_lik_neg_binom(0, 2, 1), dnbinom(0, mu = 2, size = 1, log = TRUE))
  expect_identical(log_lik_neg_binom(0, 2, 2), dnbinom(0, size = 1/2, mu = 2, log = TRUE))
})

test_that("log_lik_gamma_pois", {
  expect_identical(log_lik_gamma_pois(0, 2, 2), dnbinom(0, mu = 2, size = 1/2, log = TRUE))
})

test_that("log_lik_gamma_pois_zi", {
  expect_identical(log_lik_gamma_pois_zi(1, 2), log_lik_pois(1, 2))
  expect_identical(log_lik_gamma_pois_zi(1, 2, 1), log_lik_gamma_pois(1, 2, 1))
  expect_identical(log_lik_gamma_pois_zi(1, 2, 0, 0.5), log_lik_pois_zi(1, 2, 0.5))
})
