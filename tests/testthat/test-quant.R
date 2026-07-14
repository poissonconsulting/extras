test_that("quant_beta", {
  expect_identical(quant_beta(numeric(0)), numeric(0))
  expect_identical(quant_beta(NA), NA_real_)
  expect_identical(quant_beta(0.5, NA), NA_real_)
  expect_equal(quant_beta(0), 0)
  expect_equal(quant_beta(1), 1)
  expect_equal(quant_beta(0.5, 2, 3), qbeta(0.5, 2, 3))
  expect_equal(quant_beta(0.5, 2, 3), 0.38572756813239)
  expect_equal(prob_beta(quant_beta(0.7, 2, 3), 2, 3), 0.7)
})

test_that("quant_exp", {
  expect_identical(quant_exp(numeric(0)), numeric(0))
  expect_identical(quant_exp(NA), NA_real_)
  expect_identical(quant_exp(0.5, NA), NA_real_)
  expect_equal(quant_exp(0), 0)
  expect_equal(quant_exp(1), Inf)
  expect_equal(quant_exp(0.5, 2), qexp(0.5, 2))
  expect_equal(quant_exp(0.5, 2), 0.346573590279973)
  expect_equal(prob_exp(quant_exp(0.7, 2), 2), 0.7)
})

test_that("quant_unif", {
  expect_identical(quant_unif(numeric(0)), numeric(0))
  expect_identical(quant_unif(NA), NA_real_)
  expect_identical(quant_unif(0.5, NA), NA_real_)
  expect_equal(quant_unif(0), 0)
  expect_equal(quant_unif(1), 1)
  expect_equal(quant_unif(0.5), qunif(0.5))
  expect_equal(quant_unif(0.5), 0.5)
  expect_equal(quant_unif(0.5, 0, 2), 1)
  expect_equal(prob_unif(quant_unif(0.7), 0, 1), 0.7)
})

test_that("quant_bern", {
  expect_identical(quant_bern(numeric(0)), numeric(0))
  expect_identical(quant_bern(0.5, numeric(0)), numeric(0))
  expect_identical(quant_bern(NA), NA_real_)
  expect_identical(quant_bern(0.5, NA), NA_real_)
  expect_equal(quant_bern(0.3, 0.7), 0)
  expect_equal(quant_bern(0.8, 0.7), 1)
  expect_equal(quant_bern(0, 0.5), 0)
  expect_equal(quant_bern(1, 0.5), 1)
  expect_equal(
    quant_bern(c(0.3, 0.8), 0.7),
    qbinom(c(0.3, 0.8), size = 1, prob = 0.7)
  )
})

test_that("quant_binom", {
  expect_identical(quant_binom(numeric(0)), numeric(0))
  expect_identical(quant_binom(0.5, numeric(0)), numeric(0))
  expect_identical(quant_binom(NA), NA_real_)
  expect_identical(quant_binom(0.5, NA), NA_real_)
  expect_identical(quant_binom(0.5, prob = NA), NA_real_)
  expect_equal(quant_binom(0.3, 10, 0.4), qbinom(0.3, 10, 0.4))
  expect_equal(quant_binom(0.3, 10, 0.4), 3)
  expect_equal(quant_binom(0, 5, 0.5), 0)
  expect_equal(quant_binom(1, 5, 0.5), 5)
})

test_that("quant_pois", {
  expect_identical(quant_pois(numeric(0)), numeric(0))
  expect_identical(quant_pois(0.5, numeric(0)), numeric(0))
  expect_identical(quant_pois(NA), NA_real_)
  expect_identical(quant_pois(0.5, NA), NA_real_)
  expect_equal(quant_pois(0.7, 3), qpois(0.7, 3))
  expect_equal(quant_pois(0.7, 3), 4)
  expect_equal(quant_pois(0, 1), 0)
})

test_that("quant_pois_zi", {
  expect_identical(quant_pois_zi(NA), NA_real_)
  expect_equal(
    quant_pois_zi(c(0.3, 0.45, 0.6), 3, prob = 0),
    quant_pois(c(0.3, 0.45, 0.6), 3)
  )
  expect_equal(quant_pois_zi(c(0.3, 0.45, 0.6), 3, prob = 0.4), c(0, 1, 2))
})

test_that("quant_neg_binom", {
  expect_identical(quant_neg_binom(numeric(0)), numeric(0))
  expect_identical(quant_neg_binom(NA), NA_real_)
  expect_identical(quant_neg_binom(0.5, NA), NA_real_)
  expect_equal(
    quant_neg_binom(0.7, 4, 0.2),
    qnbinom(0.7, mu = 4, size = 1 / 0.2)
  )
  expect_equal(quant_neg_binom(0.7, 4, 0.2), 5)
  expect_equal(quant_neg_binom(0, 1, 1), 0)
})

test_that("quant_gamma_pois", {
  expect_identical(quant_gamma_pois(NA), NA_real_)
  expect_equal(quant_gamma_pois(0.7, 4, 0.2), quant_neg_binom(0.7, 4, 0.2))
  expect_equal(quant_gamma_pois(0.7, 4, 0.2), 5)
})

test_that("quant_gamma_pois_zi", {
  expect_identical(quant_gamma_pois_zi(NA), NA_real_)
  expect_equal(
    quant_gamma_pois_zi(c(0.3, 0.5, 0.7), 3, 0.5, prob = 0),
    quant_neg_binom(c(0.3, 0.5, 0.7), 3, 0.5)
  )
  expect_equal(
    quant_gamma_pois_zi(c(0.3, 0.5, 0.7), 3, 0.5, prob = 0.4),
    c(0, 1, 2)
  )
})

test_that("quant_norm", {
  expect_identical(quant_norm(numeric(0)), numeric(0))
  expect_identical(quant_norm(NA), NA_real_)
  expect_identical(quant_norm(0.5, NA), NA_real_)
  expect_identical(quant_norm(0.5, sd = NA), NA_real_)
  expect_equal(quant_norm(0.5), 0)
  expect_equal(quant_norm(0.5, 3, 2), 3)
  expect_equal(quant_norm(0), -Inf)
  expect_equal(quant_norm(1), Inf)
  expect_equal(quant_norm(0.975), qnorm(0.975))
  expect_equal(quant_norm(0.975), 1.95996398454005)
  expect_equal(prob_norm(quant_norm(0.7, 2, 0.5), 2, 0.5), 0.7)
})

test_that("quant_lnorm", {
  expect_identical(quant_lnorm(numeric(0)), numeric(0))
  expect_identical(quant_lnorm(NA), NA_real_)
  expect_identical(quant_lnorm(0.5, NA), NA_real_)
  expect_identical(quant_lnorm(0.5, sdlog = NA), NA_real_)
  expect_equal(quant_lnorm(0), 0)
  expect_equal(quant_lnorm(1), Inf)
  expect_equal(quant_lnorm(0.5, 1, 0.5), qlnorm(0.5, 1, 0.5))
  expect_equal(quant_lnorm(0.5, 1, 0.5), exp(1))
  expect_equal(prob_lnorm(quant_lnorm(0.7, 1, 0.5), 1, 0.5), 0.7)
})

test_that("quant_gamma", {
  expect_identical(quant_gamma(numeric(0)), numeric(0))
  expect_identical(quant_gamma(NA), NA_real_)
  expect_identical(quant_gamma(0.5, NA), NA_real_)
  expect_identical(quant_gamma(0.5, rate = NA), NA_real_)
  expect_equal(quant_gamma(0), 0)
  expect_equal(quant_gamma(1), Inf)
  expect_equal(quant_gamma(0.5, 2, 1), qgamma(0.5, 2, 1))
  expect_equal(quant_gamma(0.5, 2, 1), 1.67834699001666)
  expect_equal(prob_gamma(quant_gamma(0.7, 2, 1), 2, 1), 0.7)
})

test_that("quant_skewnorm", {
  skip_if_not_installed("sn")
  expect_identical(quant_skewnorm(NA), NA_real_)
  expect_equal(quant_skewnorm(0.5, 3, 2, shape = 0), quant_norm(0.5, 3, 2))
  expect_equal(quant_skewnorm(0.5, 2, 1, 2), 2.65537040026575)
  expect_equal(
    prob_skewnorm(quant_skewnorm(0.7, 2, 1, 2), 2, 1, 2),
    0.7,
    tolerance = 1e-6
  )
})

test_that("quant_student", {
  expect_identical(quant_student(NA, 1, 1, 0.5), NA_real_)
  expect_equal(quant_student(0.5, c(1, 2), 1, 0.5), c(1, 2))
  expect_error(quant_student(0.5, mean = 0, sd = -1, theta = 0.5))
  expect_equal(quant_student(0.5, 2, 1, 0.5), 2)
  expect_equal(quant_student(0.5, 2, 1, 0), 2)
  expect_equal(quant_student(0.975, 0, 1, 0.5), qt(0.975, df = 2))
  expect_equal(quant_student(0.975, 0, 1, 0.5), 4.30265272974946)
  expect_equal(prob_student(quant_student(0.7, 2, 1, 0.5), 2, 1, 0.5), 0.7)
})

test_that("quant_skewlnorm", {
  skip_if_not_installed("sn")
  expect_identical(quant_skewlnorm(NA), NA_real_)
  expect_equal(
    quant_skewlnorm(c(0.1, 0.5, 0.9), 0.3, 0.7, shape = 0),
    quant_lnorm(c(0.1, 0.5, 0.9), 0.3, 0.7)
  )
  expect_equal(quant_skewlnorm(0.5, 0, 1, 2), 1.92585572275397)
  expect_equal(
    prob_skewlnorm(quant_skewlnorm(0.7, 0, 1, 2), 0, 1, 2),
    0.7,
    tolerance = 1e-6
  )
})
