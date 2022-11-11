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
  expect_equal(log_lik_gamma_pois(1, 2), -1.30685281944005)
  expect_equal(log_lik_gamma_pois(1, 2, 0.5), -1.38629436111989)
  expect_equal(log_lik_gamma_pois(0, 2, 2), -0.80471895621705)
})

test_that("gamma_pois_zi missing values", {
  expect_identical(log_lik_gamma_pois_zi(numeric(0), numeric(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(log_lik_gamma_pois_zi(NA, 1, 1, 0.5), NA_real_)
  expect_identical(log_lik_gamma_pois_zi(1, NA, 1, 0.5), NA_real_)
  expect_identical(log_lik_gamma_pois_zi(1, 1, NA, 0.5), NA_real_)
  expect_identical(log_lik_gamma_pois_zi(1, 1, 1, NA), NA_real_)
})

test_that("gamma_pois_zi known values", {
  expect_equal(log_lik_gamma_pois_zi(0, 3), -3)
  expect_equal(log_lik_gamma_pois_zi(0, 3, 0.5, 0.5), -0.544727175441672)
  expect_equal(log_lik_gamma_pois_zi(1, 2), -1.30685281944005)
  expect_equal(log_lik_gamma_pois_zi(2, 2), -1.30685281944005)
  expect_equal(log_lik_gamma_pois_zi(1, 2, 0.5), -1.38629436111989)
  expect_equal(log_lik_gamma_pois_zi(1, 2, 0.5, 0.5), -2.07944154167984)
})

test_that("gamma_pois_zi vectorized", {
  expect_equal(log_lik_gamma_pois_zi(0:3, 2, 0, 0), c(-2, -1.30685281944005, -1.30685281944005, -1.71231792754822))
  expect_equal(log_lik_gamma_pois_zi(c(0, 1, 3, 0), 3, 0.5, 0.5), c(-0.544727175441672, -2.3434070875143, -2.67191115448634, -0.544727175441672))
  expect_equal(log_lik_gamma_pois_zi(0:3, 0:3, rep(1, 4), 0), c(0, -1.38629436111989, -1.90954250488444, -2.24934057847523))
  expect_equal(log_lik_gamma_pois_zi(0:3, 3:0, 0:3, seq(0, 1, length.out = 4)), c(-3, -1.90954250488444, -3.43967790223022, -Inf))
})

test_that("gamma missing values", {
  expect_identical(log_lik_gamma(NA), NA_real_)
  expect_identical(log_lik_gamma(1, NA), NA_real_)
  expect_identical(log_lik_gamma(1, rate = NA), NA_real_)
  expect_identical(log_lik_gamma(1, shape = NA, rate = NA), NA_real_)
})

test_that("gamma known values", {
  expect_equal(log_lik_gamma(1, 1, 1), -1)
  expect_equal(log_lik_gamma(0, 0, 0), Inf)
  expect_equal(log_lik_gamma(1, 2, 2), -0.613705638880109)
  expect_equal(log_lik_gamma(1, 2, 5), -1.7811241751318)
  expect_equal(log_lik_gamma(5, 1, 2), -9.30685281944005)
  expect_equal(log_lik_gamma(0.5, 0.5, 0.5), -0.8223649429247)
  expect_equal(log_lik_gamma(-1, 1, 1), -Inf)
  expect_identical(log_lik_gamma(1, 2, 5), dgamma(1, 2, 5, log = TRUE))
})

test_that("gamma vectorized", {
  expect_equal(log_lik_gamma(4:6, 1:3, c(0.5, 1, 2)), c(-2.69314718055995, -3.3905620875659, -7.030186700424))
  expect_equal(log_lik_gamma(1:3, c(0.5, 0.4, 0.3), 3:1), c(-3.02305879859065, -4.93530725381377, -4.86482659688575))
  expect_equal(log_lik_gamma(seq(0, 1, length.out = 4), 1:4, seq(0, 2, length.out = 4)), c(-Inf, -2.13176472710666, -1.52992006830982, -1.01917074698827))
})

test_that("student missing values", {
  expect_identical(log_lik_student(numeric(0), numeric(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(log_lik_student(NA, 1, 1, 0.5), NA_real_)
  expect_identical(log_lik_student(1, NA, 1, 0.5), NA_real_)
  expect_identical(log_lik_student(1, 1, NA, 0.5), NA_real_)
  expect_identical(log_lik_student(1, 1, 1, NA), NA_real_)
})

test_that("student known values", {
  expect_equal(log_lik_student(0, 3), -5.41893853320467)
  expect_equal(log_lik_student(0, 3, 0), -Inf)
  expect_equal(log_lik_student(0, 3, 0.5, 0.5), -4.76323205902963)
  expect_equal(log_lik_student(1, 2), -1.41893853320467)
  expect_equal(log_lik_student(2, 2), -0.918938533204673)
  expect_equal(log_lik_student(1, 2, 0.5), -2.22579135264473)
  expect_equal(log_lik_student(1, 0, 0, 0.5))
  expect_equal(log_lik_student(1, 2, 0.5), log_lik_norm(1, 2, 0.5))
  expect_equal(log_lik_student(1, theta = 1/2), dt(1, df = 2, log = TRUE))
  expect_lt(log_lik_norm(5), log_lik_student(5, theta = 1/5))
})

test_that("student vectorized", {
  expect_equal(log_lik_student(0:3, 2, 0.5, 0), c(-8.22579135264473, -2.22579135264473, -0.225791352644727, -2.22579135264473))
  expect_equal(log_lik_student(0:3, 2, 0.5, 0), log_lik_norm(0:3, 2, 0.5))
  expect_equal(log_lik_student(c(0, 1, 3, 0), 3, 0.5, 0.5), c(-4.76323205902963, -3.6424104562843, -0.346573590279973, -4.76323205902963))
  expect_equal(log_lik_student(0:3, 0:3, rep(1, 4), 0.5), c(-1.03972077083992, -1.03972077083992, -1.03972077083992, -1.03972077083992))
  expect_equal(log_lik_student(0:3, 3:0, 1:4, seq(0, 1, length.out = 4)), c(-5.41893853320467, -1.85412144553053, -2.26458627847768, -2.97731134959771))
})

test_that("beta_binom missing values", {
  expect_identical(log_lik_beta_binom(numeric(0), numeric(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(log_lik_beta_binom(1, numeric(0)), numeric(0))
  expect_identical(log_lik_beta_binom(1, 1, prob = numeric(0)), numeric(0))
  expect_identical(log_lik_beta_binom(1, 1, theta = numeric(0)), numeric(0))
  expect_identical(log_lik_beta_binom(NA, 1, 1, 0.5), NA_real_)
  expect_identical(log_lik_beta_binom(1, NA, 1, 0.5), NA_real_)
  expect_identical(log_lik_beta_binom(1, 1, NA, 0.5), NA_real_)
  expect_identical(log_lik_beta_binom(1, 1, 1, NA), NA_real_)
})

test_that("beta_binom known values", {
  expect_equal(log_lik_beta_binom(0, 3), -2.07944154167984)
  expect_equal(log_lik_beta_binom(0, 3, 0), 0)
  expect_equal(log_lik_beta_binom(0, 3, 1), -Inf)
  expect_equal(log_lik_beta_binom(1, 3, 1), -Inf)
  expect_identical(log_lik_beta_binom(3, 3, 1), 0)
  expect_identical(log_lik_beta_binom(0, 0), 0)
  expect_equal(log_lik_beta_binom(0, 3, 0.5, 0.5), -1.6094379124341)
  expect_equal(log_lik_beta_binom(1, 2, 0.2, 1), -1.54489939129653)
  expect_equal(log_lik_beta_binom(2, 2, 0.2, 10), -1.75253875607477)
  expect_equal(log_lik_beta_binom(1, 2, 0.5), -0.693147180559945)
  expect_equal(log_lik_beta_binom(1, 2, 0.5), log_lik_binom(1, 2, 0.5))
  expect_equal(log_lik_beta_binom(1, 2, 0.2), dbinom(1, 2, 0.2, log = TRUE))
})

test_that("beta_binom vectorized", {
  expect_equal(log_lik_beta_binom(0:3, 2, 0.5, 0), c(-1.38629436111989, -0.693147180559945, -1.38629436111989, -Inf))
  expect_equal(log_lik_beta_binom(0:3, 2, 0.5, 0), log_lik_binom(0:3, 2, 0.5))
  expect_equal(log_lik_beta_binom(c(0, 1, 3, 4, 5), 3, 0.5, 0.5), c(-1.6094379124341, -1.20397280432594, -1.6094379124341, -Inf, -Inf))
  expect_equal(log_lik_beta_binom(0:3, 5:8, seq(0, 1, length.out = 4), 0.5), c(0, -1.47834815081045, -2.50875218945204, 0))
  expect_equal(log_lik_beta_binom(0:3, 3:0, seq(0, 1, length.out = 4), 0.5), c(0, -1.03407376753054, -Inf, -Inf))
})
