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
  expect_error(log_lik_bern(1, 0.7, tlower = 0))
  expect_error(log_lik_bern(1, 0.7, tupper = 0.4))
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

test_that("log_lik_binom truncated", {
  expect_identical(log_lik_binom(1, tlower = NA), NA_real_)
  expect_identical(log_lik_binom(1, tupper = NA), NA_real_)
  expect_identical(log_lik_binom(1, tlower = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_identical(log_lik_binom(1, tupper = numeric(0)), numeric(0))
  expect_identical(log_lik_binom(0, 0, tlower = 1), -Inf)
  expect_identical(log_lik_binom(4L, 10, 0.5, tlower = 5), -Inf)
  expect_equal(log_lik_binom(4L, 10, 0.5, tlower = 2), -1.57356397353121)
  expect_equal(log_lik_binom(4, 10, 0.5, tlower = 2, tupper = 7), -1.51669586073549)
  expect_equal(log_lik_binom(c(4, 10), 10, 0.5, tlower = 2, tupper = 7), c(-1.51669586073549, -Inf))
  expect_equal(log_lik_binom(c(2, 7), 10, 0.5, tlower = 2, tupper = 7), c(-3.05714090168264, -2.07631164867091))
  expect_equal(log_lik_binom(c(1, 8), 10, 0.5, tlower = 2, tupper = 7), rep(-Inf, 2))
  expect_equal(log_lik_binom(c(1, 1), 10, 0.5, tlower = c(1, 2), tupper = 7), c(-4.57161340245925, -Inf))
  expect_equal(log_lik_binom(1, 10, 0.4, tlower = 0), log_lik_binom(1, 10, 0.4, tlower = -1))
})


test_that("log_lik_pois", {
  expect_identical(log_lik_pois(numeric(0)), numeric(0))
  expect_identical(log_lik_pois(1, numeric(0)), numeric(0))
  expect_identical(log_lik_pois(NA), NA_real_)
  expect_identical(log_lik_pois(1, NA), NA_real_)
  expect_equal(log_lik_pois(1, 2), -1.30685281944005)
})

test_that("log_lik_pois truncated", {
  expect_identical(log_lik_pois(1, tlower = NA), NA_real_)
  expect_identical(log_lik_pois(1, tupper = NA), NA_real_)
  expect_identical(log_lik_pois(1, tlower = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_identical(log_lik_pois(1, tupper = numeric(0)), numeric(0))
  expect_identical(log_lik_pois(0, 1, tlower = 1), -Inf)
  expect_identical(log_lik_pois(4L, 10, tlower = 5), -Inf)
  expect_equal(log_lik_pois(4L, 10, tlower = 2), -3.96721393440305)
  expect_equal(log_lik_pois(4L, 10, tupper = 7), -2.45458816223323)
  expect_equal(log_lik_pois(4, 10, tlower = 2, tupper = 7), -2.45231786495658)
  expect_equal(log_lik_pois(c(4, 10), 10, tlower = 2, tupper = 7), c(-2.45231786495658, -Inf))
  expect_equal(log_lik_pois(c(2, 7), 10, tlower = 2, tupper = 7), c(-4.57258140115667, -0.891670116691908))
  expect_equal(log_lik_pois(c(1, 8), 10, tlower = 2, tupper = 7), rep(-Inf, 2))
  expect_equal(log_lik_pois(c(1, 1), 10, tlower = c(1, 2), tupper = 7), c(-6.1840834330596, -Inf))
  expect_equal(log_lik_pois(1, 10, tlower = 0), log_lik_pois(1, 10, tlower = -1))
})

test_that("log_lik_pois_zi", {
  expect_identical(log_lik_pois_zi(numeric(0)), numeric(0))
  expect_identical(log_lik_pois_zi(1, numeric(0)), numeric(0))
  expect_identical(log_lik_pois_zi(1, prob = numeric(0)), numeric(0))
  expect_identical(log_lik_pois_zi(NA), NA_real_)
  expect_identical(log_lik_pois_zi(1, NA), NA_real_)
  expect_identical(log_lik_pois_zi(1, prob = NA), NA_real_)

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
  expect_equal(log_lik_pois_zi(1, 10, 0.4, tlower = 0), log_lik_pois_zi(1, 10, 0.4, tlower = -1))
})

test_that("log_lik_pois_zi truncated", {
  expect_identical(log_lik_pois_zi(1, prob = 0.5, tlower = NA), NA_real_)
  expect_identical(log_lik_pois_zi(1, prob = 0.5, tupper = NA), NA_real_)
  expect_warning(log_lik_pois_zi(0, 1, 0.5, tlower = 1), "Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  expect_equal(suppressWarnings(log_lik_pois_zi(0, 1, 0.5, tlower = 1)), -Inf)
  expect_equal(log_lik_pois_zi(4L, 10, prob = 0.2, tupper = 7), -2.45442322338786)
  expect_equal(log_lik_pois_zi(4, 10, prob = 0.3, tupper = 7), -2.45444384223136)
  expect_equal(log_lik_pois_zi(c(4, 10), prob = 0.5, 10, tupper = 7), c(-2.45448507864298, -Inf))
  expect_equal(log_lik_pois_zi(c(2, 7), prob = 0.5, 10, tupper = 7), c(-4.57474861484307, -0.893837330378314))
  expect_equal(log_lik_pois_zi(c(1, 8), prob = 0.5, 10, tupper = 7), c(-6.18418652727717, -Inf))
  expect_equal(suppressWarnings(log_lik_pois_zi(c(1, 1), prob = 0.5, 10, tlower = c(1, 2), tupper = 7)), c(-6.18201931359077, -Inf))
  expect_warning(log_lik_pois_zi(c(1, 1), prob = 0.5, 10, tlower = c(1, 2), tupper = 7), "Specifying a lower truncation point greater than 0 doesn't make sense for a zero-inflated distribution.")
  expect_equal(log_lik_pois_zi(2, prob = c(0.5, 0.1), 10, tupper = c(7, 10)), c(-4.57474861484307, -5.54840699843274))
  expect_equal(log_lik_pois_zi(1, 10, prob = 0.5, tlower = 0), log_lik_pois_zi(1, prob = 0.5, 10, tlower = -1))
})

test_that("log_lik_norm", {
  expect_identical(log_lik_norm(numeric(0)), numeric(0))
  expect_identical(log_lik_norm(1, numeric(0)), numeric(0))
  expect_identical(log_lik_norm(1, sd = numeric(0)), numeric(0))
  expect_identical(log_lik_norm(NA), NA_real_)
  expect_identical(log_lik_norm(1, NA), NA_real_)
  expect_identical(log_lik_norm(1, sd = NA), NA_real_)
  expect_identical(log_lik_norm(1, 2), dnorm(1, 2, log = TRUE))
})

test_that("log_lik_norm truncated", {
  expect_identical(log_lik_norm(1, tlower = NA), NA_real_)
  expect_identical(log_lik_norm(1, tupper = NA), NA_real_)
  expect_identical(log_lik_norm(1, tlower = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_identical(log_lik_norm(1, tupper = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_equal(log_lik_norm(10, 10, 1, tlower = 0), -0.918938533204673)
  expect_equal(log_lik_norm(10, 10, 1, tlower = 0, tupper = 10), -0.225791352644727)
  expect_equal(log_lik_norm(c(10, 2), 10, 1, tlower = 0, tupper = 10), c(-0.225791352644727, -32.2257913526447))
  expect_equal(log_lik_norm(c(10, 2), c(10, 3), 1, tlower = 0, tupper = 10), c(-0.225791352644727, -1.41758772323864))
  expect_equal(log_lik_norm(c(10, 2), c(10, 3), c(1, 2), tlower = 0, tupper = 10), c(-0.225791352644727, -1.66769294409916))
  expect_equal(log_lik_norm(c(10, 2), c(10, 3), c(1, 2), tlower = c(0, 1), tupper = 10), c(-0.225791352644727, -1.56405539976945))
  expect_equal(log_lik_norm(c(10, 2), c(10, 3), c(1, 2), tlower = c(0, 1), tupper = c(10, 12)), c(-0.225791352644727, -1.56432789634906))
})

test_that("log_lik_lnorm", {
  expect_identical(log_lik_lnorm(1, 2), dlnorm(1, 2, log = TRUE))
})

test_that("log_lik_lnorm truncated", {
  expect_identical(log_lik_lnorm(1, tlower = NA), NA_real_)
  expect_identical(log_lik_lnorm(1, tupper = NA), NA_real_)
  expect_identical(log_lik_lnorm(1, tlower = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_identical(log_lik_lnorm(1, tupper = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_equal(log_lik_lnorm(10, 10, 1, tlower = 0), -32.8466217514975)
  expect_equal(log_lik_lnorm(10, 10, 1, tlower = 0, tupper = 10), -0.24548202100403)
  expect_equal(log_lik_lnorm(c(10, 2), 10, 1, tlower = 0, tupper = 10), c(-0.24548202100403, -12.3197006846308))
  expect_equal(log_lik_lnorm(c(10, 2), c(10, 3), 1, tlower = 0, tupper = 10), c(-0.24548202100403, -2.85723643248397))
  expect_equal(log_lik_lnorm(c(10, 2), c(10, 3), c(1, 2), tlower = 0, tupper = 10), c(-0.24548202100403, -1.95887802625377))
  expect_equal(log_lik_lnorm(c(10, 2), c(10, 3), c(1, 2), tlower = c(0, 1), tupper = 10), c(-0.24548202100403, -1.75589163000754))
  expect_equal(log_lik_lnorm(c(10, 2), c(10, 3), c(1, 2), tlower = c(0, 1), tupper = c(10, 12)), c(-0.24548202100403, -1.86651640350366))
})


test_that("log_lik_neg_binom", {
  expect_identical(log_lik_neg_binom(0, 2, 1), dnbinom(0, mu = 2, size = 1, log = TRUE))
  expect_identical(log_lik_neg_binom(0, 2, 2), dnbinom(0, size = 1 / 2, mu = 2, log = TRUE))
})

test_that("log_lik_neg_binom truncated", {
  expect_identical(log_lik_neg_binom(1, tlower = NA), NA_real_)
  expect_identical(log_lik_neg_binom(1, tupper = NA), NA_real_)
  expect_identical(log_lik_neg_binom(1, tlower = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_identical(log_lik_neg_binom(1, tupper = numeric(0)), numeric(0)) #FIXME: fix this and add tests for other dists
  expect_equal(log_lik_neg_binom(1, 10, 0.2, tlower = 10), -Inf) # FIXME: need check that tlower is an integer!
  expect_equal(log_lik_neg_binom(1, 10, 0.2, tlower = 9), -Inf)

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
  expect_equal(log_lik_student(0, 3, 0, 1), -Inf)
  expect_equal(log_lik_student(0, 3, 0.5, 0.5), -4.76323205902963)
  expect_equal(log_lik_student(0, 3, 0.5, 1), -4.06250061793368)
  expect_equal(log_lik_student(1, 2), -1.41893853320467)
  expect_equal(log_lik_student(2, 2), -0.918938533204673)
  expect_equal(log_lik_student(1, 2, 0.5), -2.22579135264473)
  expect_equal(log_lik_student(1, 0, 0, 0.5), -Inf)
  expect_equal(log_lik_student(1, 2, 0.5), log_lik_norm(1, 2, 0.5))
  expect_equal(log_lik_student(1, theta = 1 / 2), dt(1, df = 2, log = TRUE))
  expect_lt(log_lik_norm(5), log_lik_student(5, theta = 1 / 5))
})

test_that("student vectorized", {
  expect_equal(log_lik_student(0:3, 2, 0.5, 0), c(-8.22579135264473, -2.22579135264473, -0.225791352644727, -2.22579135264473))
  expect_equal(log_lik_student(0:3, 2, 0.5, 0), log_lik_norm(0:3, 2, 0.5))
  expect_equal(log_lik_student(c(0, 1, 3, 0), 3, 0.5, 0.5), c(-4.76323205902963, -3.6424104562843, -0.346573590279973, -4.76323205902963))
  expect_equal(log_lik_student(0:3, 0:3, rep(1, 4), 0.5), c(-1.03972077083992, -1.03972077083992, -1.03972077083992, -1.03972077083992))
  expect_equal(log_lik_student(0:3, 3:0, 0:3, seq(0, 1, length.out = 4)), c(-Inf, -1.57625299452707, -1.96248581517591, -2.93648935507746))
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
  expect_equal(log_lik_beta_binom(1, 3, 0.3), -0.818710403535291)
  expect_identical(log_lik_beta_binom(3, 3, 1), 0)
  expect_identical(log_lik_beta_binom(0, 0), 0)
  expect_equal(log_lik_beta_binom(0, 3, 0.5, 0.5), -1.6094379124341)
  expect_equal(log_lik_beta_binom(1, 2, 0.2, 1), -1.54489939129653)
  expect_equal(log_lik_beta_binom(2, 2, 0.2, 10), -1.75253875607477)
  expect_equal(log_lik_beta_binom(1, 2, 0.5), -0.693147180559945)
  expect_equal(log_lik_beta_binom(10, 2, 0.5, 0.1), -Inf)
  expect_equal(log_lik_beta_binom(10, 2, 0.5, -0.1), NaN)
  expect_equal(log_lik_beta_binom(1, 2, 0.5), log_lik_binom(1, 2, 0.5))
  expect_equal(log_lik_beta_binom(1, 2, 0.2), dbinom(1, 2, 0.2, log = TRUE))
})

test_that("beta binomial deviance function is memoized", {
  skip_if_not_installed("memoise")
  expect_true(memoise::is.memoized(lgamma_size_x))
})

test_that("lgamma_size_x produces expected outputs", {
  size <- 100
  x <- 1
  expect_equal(
    lgamma_size_x(size, x),
    lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
  )
  size <- 100
  x <- 1:100
  expect_equal(
    lgamma_size_x(size, x),
    lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
  )
  size <- 201:300
  x <- 1:100
  expect_equal(
    lgamma_size_x(size, x),
    lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
  )
})

test_that("beta_binom memoized function gives same outputs as non-memoized function", {
  skip_if_not_installed("memoise")
  expect_equal(
    log_lik_beta_binom(1:100, 200, 0.5, 0.1, memoize = FALSE),
    log_lik_beta_binom(1:100, 200, 0.5, 0.1, memoize = TRUE)
  )
  expect_equal(
    log_lik_beta_binom(1:100, 200, 0.1, 0, memoize = FALSE),
    log_lik_beta_binom(1:100, 200, 0.1, 0, memoize = TRUE)
  )
})

test_that("beta_binom vectorized", {
  expect_equal(log_lik_beta_binom(0:3, 2, 0.5, 0), c(-1.38629436111989, -0.693147180559945, -1.38629436111989, -Inf))
  expect_equal(log_lik_beta_binom(0:3, 2, 0.5, 0), log_lik_binom(0:3, 2, 0.5))
  expect_equal(log_lik_beta_binom(c(0, 1, 3, 4, 5), 3, 0.5, 0.5), c(-1.6094379124341, -1.20397280432594, -1.6094379124341, -Inf, -Inf))
  expect_equal(log_lik_beta_binom(0:3, 5:8, seq(0, 1, length.out = 4), 0.5), c(0, -1.47834815081045, -2.50875218945204, -Inf))
  expect_equal(log_lik_beta_binom(0:3, 3:0, seq(0, 1, length.out = 4), 0.5), c(0, -1.03407376753054, -Inf, -Inf))
})

test_that("beta_binom log_lik", {
  withr::with_seed(
    101,
    samples <- ran_beta_binom(100, size = 50, prob = 0.1, theta = 1)
  )
  expect_snapshot(
    log_lik_beta_binom(
      x = samples,
      size = 50,
      prob = 0.2,
      theta = 1.1
    )
  )
})

test_that("skewnorm missing values", {
  skip_if_not_installed("sn")
  expect_identical(log_lik_skewnorm(numeric(0), numeric(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(log_lik_skewnorm(1, numeric(0)), numeric(0))
  expect_identical(log_lik_skewnorm(1, 1, sd = numeric(0)), numeric(0))
  expect_identical(log_lik_skewnorm(1, 1, shape = numeric(0)), numeric(0))
  expect_identical(log_lik_skewnorm(NA, 1, 1, 0.5), NA_real_)
  expect_identical(log_lik_skewnorm(1, NA, 1, 0.5), NA_real_)
  expect_identical(log_lik_skewnorm(1, 1, NA, 0.5), NA_real_)
  expect_identical(log_lik_skewnorm(1, 1, 1, NA), NA_real_)
})

test_that("skewnorm known values", {
  skip_if_not_installed("sn")
  expect_equal(log_lik_skewnorm(0.5, 3), -4.04393853320467)
  expect_equal(log_lik_skewnorm(0.5, 3, 0), -Inf)
  expect_equal(log_lik_skewnorm(0.5, 3, 1, -4), -3.35079135264473)
  expect_equal(log_lik_skewnorm(-100, 3, 1, -10), -5304.72579135264)
  expect_equal(log_lik_skewnorm(-100, 3, 0.3, Inf), -Inf)
  expect_equal(log_lik_skewnorm(100, 3, 1), -4705.4189385332)
  expect_equal(log_lik_skewnorm(0, 0), -0.918938533204673)
  expect_equal(log_lik_skewnorm(0, 3, 0.5, 0.5), -24.1403703935951)
  expect_equal(log_lik_skewnorm(1, 2, 0.2, -1), -11.1163537268622)
  expect_equal(log_lik_skewnorm(2, 2, 0.2, 10), 0.690499379229428)
  expect_equal(log_lik_skewnorm(1, 2, 0.5, -10), -1.53264417208478)
  expect_equal(log_lik_skewnorm(1, 2, 0.5, 0), log_lik_norm(1, 2, 0.5))
  expect_equal(log_lik_skewnorm(1, 2, 0.2), dnorm(1, 2, 0.2, log = TRUE))
})

test_that("skewnorm vectorized", {
  skip_if_not_installed("sn")
  expect_equal(
    log_lik_skewnorm(0:3, 2, 0.5, 0),
    c(-8.22579135264473, -2.22579135264473, -0.225791352644727, -2.22579135264473)
  )
  expect_equal(
    log_lik_skewnorm(0:3, 2, 0.5, 0),
    log_lik_norm(0:3, 2, 0.5)
  )
  expect_equal(
    log_lik_skewnorm(c(0, 1, 3, 4, 5), 3, 0.5, 0.5),
    c(
      -24.1403703935951, -11.3158285057668, -0.225791352644727,
      -1.70539795110823, -7.55565708141375
    )
  )
  expect_equal(
    log_lik_skewnorm(0:3, 5:8, seq(0, 1, length.out = 4), -2),
    c(NaN, -111.627179063977, -27.9453262445366, -12.7257913526447)
  )
  expect_equal(
    log_lik_skewnorm(0:3, 3:0, seq(0, 1, length.out = 4), 0.5),
    c(NaN, -6.33312346480051, -1.20232051137493, -4.79493480825696)
  )
  expect_equal(
    log_lik_skewnorm(0:3, 3:0, 0.2, -1:2),
    c(-111.116353440211, -11.8095006207706, -11.1163537268622, -111.116353440211)
  )
})
