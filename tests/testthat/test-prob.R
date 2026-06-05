test_that("prob_bern", {
  expect_identical(prob_bern(numeric(0)), numeric(0))
  expect_identical(prob_bern(1, numeric(0)), numeric(0))
  expect_identical(prob_bern(NA), NA_real_)
  expect_identical(prob_bern(1, NA), NA_real_)
  expect_equal(prob_bern(0, 0.7), 0.3)
  expect_equal(prob_bern(1, 0.7), 1)
  expect_equal(prob_bern(0, 0), 1)
  expect_equal(prob_bern(1, 0), 1)
  expect_equal(prob_bern(0, 1), 0)
  expect_equal(prob_bern(c(0, 1), 0.3), prob_binom(c(0, 1), size = 1, prob = 0.3))
})

test_that("prob_binom", {
  expect_identical(prob_binom(numeric(0)), numeric(0))
  expect_identical(prob_binom(1, numeric(0)), numeric(0))
  expect_identical(prob_binom(1, prob = numeric(0)), numeric(0))
  expect_identical(prob_binom(NA), NA_real_)
  expect_identical(prob_binom(1, NA), NA_real_)
  expect_identical(prob_binom(1, prob = NA), NA_real_)
  expect_equal(prob_binom(3, 10, 0.4), pbinom(3, 10, 0.4))
  expect_equal(prob_binom(3, 10, 0.4), 0.3822806016)
  expect_equal(prob_binom(0, 0), 1)
  expect_equal(prob_binom(5, 5, 1), 1)
})

test_that("prob_pois", {
  expect_identical(prob_pois(numeric(0)), numeric(0))
  expect_identical(prob_pois(1, numeric(0)), numeric(0))
  expect_identical(prob_pois(NA), NA_real_)
  expect_identical(prob_pois(1, NA), NA_real_)
  expect_equal(prob_pois(4, 3), ppois(4, 3))
  expect_equal(prob_pois(4, 3), 0.815263244523772)
  expect_equal(prob_pois(0, 0), 1)
})

test_that("prob_pois_zi", {
  expect_identical(prob_pois_zi(NA), NA_real_)
  expect_identical(prob_pois_zi(1, NA), NA_real_)
  expect_identical(prob_pois_zi(1, prob = NA), NA_real_)
  expect_equal(prob_pois_zi(4, 3, prob = 0), prob_pois(4, 3))
  expect_equal(prob_pois_zi(0, 3, prob = 0.4), 0.429872241020718)
  expect_equal(prob_pois_zi(2, 3, prob = 0.4), 0.653914048676106)
})

test_that("prob_neg_binom", {
  expect_identical(prob_neg_binom(numeric(0)), numeric(0))
  expect_identical(prob_neg_binom(1, numeric(0)), numeric(0))
  expect_identical(prob_neg_binom(NA), NA_real_)
  expect_identical(prob_neg_binom(1, NA), NA_real_)
  expect_equal(prob_neg_binom(3, 4, 0.2), pnbinom(3, mu = 4, size = 1 / 0.2))
  expect_equal(prob_neg_binom(3, 4, 0.2), 0.489947306323285)
})

test_that("prob_gamma_pois", {
  expect_identical(prob_gamma_pois(NA), NA_real_)
  expect_equal(prob_gamma_pois(3, 4, 0.2), prob_neg_binom(3, 4, 0.2))
  expect_equal(prob_gamma_pois(3, 4, 0.2), 0.489947306323285)
})

test_that("prob_gamma_pois_zi", {
  expect_identical(prob_gamma_pois_zi(NA), NA_real_)
  expect_equal(prob_gamma_pois_zi(4, 3, 0.5, prob = 0), prob_gamma_pois(4, 3, 0.5))
  expect_equal(prob_gamma_pois_zi(0, 3, 0.5, prob = 0.4), 0.496)
  expect_equal(prob_gamma_pois_zi(2, 3, 0.5, prob = 0.4), 0.71488)
})

test_that("prob_norm", {
  expect_identical(prob_norm(numeric(0)), numeric(0))
  expect_identical(prob_norm(1, numeric(0)), numeric(0))
  expect_identical(prob_norm(1, sd = numeric(0)), numeric(0))
  expect_identical(prob_norm(NA), NA_real_)
  expect_identical(prob_norm(1, NA), NA_real_)
  expect_identical(prob_norm(1, sd = NA), NA_real_)
  expect_equal(prob_norm(0), 0.5)
  expect_equal(prob_norm(Inf), 1)
  expect_equal(prob_norm(-Inf), 0)
  expect_equal(prob_norm(1.5, 2, 0.5), pnorm(1.5, 2, 0.5))
  expect_equal(prob_norm(1.5, 2, 0.5), 0.158655253931457)
})

test_that("prob_lnorm", {
  expect_identical(prob_lnorm(numeric(0)), numeric(0))
  expect_identical(prob_lnorm(NA), NA_real_)
  expect_identical(prob_lnorm(1, NA), NA_real_)
  expect_identical(prob_lnorm(1, sdlog = NA), NA_real_)
  expect_equal(prob_lnorm(0), 0)
  expect_equal(prob_lnorm(5, 1, 0.5), plnorm(5, 1, 0.5))
  expect_equal(prob_lnorm(5, 1, 0.5), 0.888554336706808)
})

test_that("prob_gamma", {
  expect_identical(prob_gamma(numeric(0)), numeric(0))
  expect_identical(prob_gamma(NA), NA_real_)
  expect_identical(prob_gamma(1, NA), NA_real_)
  expect_identical(prob_gamma(1, rate = NA), NA_real_)
  expect_equal(prob_gamma(0), 0)
  expect_equal(prob_gamma(Inf), 1)
  expect_equal(prob_gamma(2.5, 3, 0.5), pgamma(2.5, 3, 0.5))
  expect_equal(prob_gamma(2.5, 3, 0.5), 0.131532334517549)
})

test_that("prob_beta_binom", {
  skip_if_not_installed("extraDistr")
  expect_equal(prob_beta_binom(c(0, 1, 2), 3, 0.5, 0), prob_binom(c(0, 1, 2), 3, 0.5))
  expect_equal(prob_beta_binom(c(0, 1, 2), 3, 0.5, 0.2), c(0.159090909090909, 0.500000000000001, 0.840909090909093))
  expect_equal(prob_beta_binom(-1, 1, 0.5, 0.2), 0)
  expect_equal(prob_beta_binom(5, 3, 0.5, 0.2), 1)
})

test_that("prob_skewnorm", {
  skip_if_not_installed("sn")
  expect_identical(prob_skewnorm(NA), NA_real_)
  expect_equal(prob_skewnorm(c(-1, 0, 1), 2, 1.5, shape = 0), pnorm(c(-1, 0, 1), 2, 1.5))
  expect_equal(prob_skewnorm(1, 2, 1, 2), 0.00171887994528883)
})

test_that("prob_student", {
  expect_identical(prob_student(NA, 1, 1, 0.5), NA_real_)
  expect_error(prob_student(1, sd = -1, theta = 0.5))
  expect_equal(prob_student(2, 1, 1.5, 0.2), pt((2 - 1) / 1.5, df = 1 / 0.2))
  expect_equal(prob_student(2, 1, 1.5, 0.2), 0.73274538569948)
  expect_equal(prob_student(1, 0, 1, 0), prob_norm(1))
})
