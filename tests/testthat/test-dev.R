test_that("beta_binom missing values", {
  expect_identical(dev_beta_binom(logical(0), integer(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(dev_beta_binom(NA, 1, 1, 1), NA_real_)
  expect_identical(dev_beta_binom(1, NA, 1, 1), NA_real_)
  expect_identical(dev_beta_binom(1, 1, NA, 1), NA_real_)
  expect_identical(dev_beta_binom(1, 1, 1, NA), NA_real_)
})

test_that("beta_binom known values", {
  expect_equal(dev_beta_binom(1), 1.38629436111989)
  expect_equal(dev_beta_binom(1, 1, 0), Inf)
  expect_equal(dev_beta_binom(1, 1, 0, 0.5), Inf)
  expect_equal(dev_beta_binom(1, 1, 0.5, 0), 1.38629436111989)
  expect_identical(dev_beta_binom(1, 1, 0.5, 0), dev_binom(1, 1, 0.5))
  expect_equal(dev_beta_binom(1, 1, 0.5, 1), 1.38629430121326)
  expect_equal(dev_beta_binom(1, 1, 0.5, 0.5), 1.38629430121326)
  expect_equal(dev_beta_binom(1, 2, 0.2, 0), 0.892574205256839)
  expect_equal(dev_beta_binom(1, 2, 0.2, 1), 0.892574205256839)
  expect_equal(dev_beta_binom(1, 2, 0.2, 0.5), 0.892574205256839)
  expect_equal(dev_beta_binom(1, 5, 0.3, 0), 0.257320924779852)
  expect_equal(dev_beta_binom(1, 5, 0.3, 1), 5.7171585443605e-05) # was negative with naive sat. model
  expect_equal(dev_beta_binom(1, 5, 0.3, 0.5), 0.0274314034251604) # was negative with naive sat. model
  expect_identical(dev_beta_binom(1, 1, 1, 0.5), 0)
  expect_identical(dev_beta_binom(1, 1, 1), 0)
  expect_identical(dev_beta_binom(1, 1, 1, 1), 0)
  expect_identical(dev_beta_binom(0, 0), 0)
  expect_identical(dev_beta_binom(0, 0, 1), 0)
  expect_identical(dev_beta_binom(0, 0, 1, 1), 0)
  expect_identical(dev_beta_binom(1, 2), 0)
  expect_identical(dev_beta_binom(1, 2, 1), Inf)
  expect_equal(dev_beta_binom(1, 2, 0, 1), Inf)
  expect_equal(dev_beta_binom(0, 1), 1.38629436111989)
  expect_equal(dev_beta_binom(0, 1, 1), Inf)
  expect_equal(dev_beta_binom(0, 1, 1, 0.5), Inf)
  expect_equal(dev_beta_binom(0, 1, 1, 1), Inf)
  expect_equal(dev_beta_binom(0, 2), 2.77258872223978)
  expect_equal(dev_beta_binom(0, 2, 1), Inf)
  expect_equal(dev_beta_binom(0, 2, 0.5), 2.77258872223978)
  expect_equal(dev_beta_binom(0, 2, 0.5, 0.1), 2.67954869096997)
  expect_equal(dev_beta_binom(0, 2, 0.5, 0.5), 2.40794560865187)
  expect_equal(dev_beta_binom(0, 2, 0.1), 0.421442062631305)
  expect_equal(dev_beta_binom(0, 2, 0.1, 0.1), 0.410887933834857)
  expect_equal(dev_beta_binom(0, 2, 0.1, 0.5), 0.377484235738089)
  expect_equal(dev_beta_binom(1, 2, 1, 10), Inf)
  expect_equal(dev_beta_binom(2, 2, 1, 10), 0)
  expect_equal(dev_beta_binom(0, 2, 0.5, 10), 1.56031711509915)
  expect_equal(dev_beta_binom(0, 2, 0, 10), 0)
})

test_that("beta_binom vectorized", {
  expect_equal(dev_beta_binom(0:3, 5, 0, 0), dev_binom(0:3, 5, 0))
  expect_equal(dev_beta_binom(c(0, 1, 3, 0), 3, 0.5, 0.5), c(3.21887580642895, 0.179750127270295, 3.2188756770985, 3.21887580642895))
  expect_equal(dev_beta_binom(0:3, 0:3, rep(1, 4), 0), rep(0, 4))
  expect_equal(dev_beta_binom(0:3, 1:4, seq(0, 1, length.out = 4), 0:3), c(0, 0.235566071312767, 0.076961041136129, Inf))
})

test_that("beta_binom vectorized missing values", {
  expect_equal(dev_beta_binom(c(NA, 1), 0:1, 0:1, 0:1), c(NA, 0))
  expect_equal(dev_beta_binom(c(0, NA), 0:1, 0:1, 0:1), c(0, NA))
  expect_equal(dev_beta_binom(c(0:1), c(NA, 1), 0:1, 0:1), c(NA, 0))
  expect_equal(dev_beta_binom(c(0:1), c(0, NA), 0:1, 0:1), c(0, NA))
  expect_equal(dev_beta_binom(c(0:1), c(0:1), c(NA, 1), 0:1), c(0, 0))
  expect_equal(dev_beta_binom(c(0:1), c(0:1), c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_beta_binom(c(0:1), c(0:1), 0:1, c(NA, 1)), c(0, 0))
  expect_equal(dev_beta_binom(c(0:1), c(0:1), 0:1, c(0, NA)), c(0, NA))
})

test_that("beta_binom res", {
  expect_equal(dev_beta_binom(1, 3, 0.5, 0.5), dev_beta_binom(1, 3, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_beta_binom(0:1, c(2, 4), 0.5, 5), dev_beta_binom(0:1, c(2, 4), 0.5, 5, res = TRUE)^2)
})

test_that("beta_binom ran", {
  set.seed(101)
  samples <- ran_beta_binom(100000, 100, 0.5, 0.01)
  expect_equal(mean(samples), 49.99347)
  expect_equal(var(samples), 37.4491218503185)
  res <- dev_beta_binom(samples, 100, 0.5, 0.01, res = TRUE)
  expect_equal(mean(res), -0.00107466576791911, tolerance = 1e-04) # for M1
  expect_equal(sd(res), 1.0040052194768)
})

test_that("deviance beta_binom", {
  samples <- ran_beta_binom(100, size = 3, prob = 0.5, theta = 0) # binomial case.
  mod <- glm(cbind(samples, 3 - samples) ~ 1, family = binomial)
  deviance <- sum(dev_beta_binom(samples, size = 3, ilogit(coef(mod)[1])), theta = 0)
  expect_equal(deviance, deviance(mod))
  # no packages that calculate deviance using binomial saturated model.
})

test_that("bern missing values", {
  expect_identical(dev_bern(logical(0), integer(0)), numeric(0))
  expect_identical(dev_bern(NA, 1), NA_real_)
  expect_identical(dev_bern(1, NA), NA_real_)
})

test_that("bern known values", {
  expect_identical(dev_bern(1, 1), 0)
  expect_identical(dev_bern(0, 0), 0)
  expect_identical(dev_bern(1, 0), Inf)
  expect_identical(dev_bern(0, 1), Inf)
  expect_equal(dev_bern(1, 0.5), 1.38629436111989)
  expect_equal(dev_bern(0, 0.5), 1.38629436111989)
  expect_equal(dev_bern(1, 0.7), 0.713349887877465)
  expect_equal(dev_bern(0, 0.7), 2.40794560865187)
})

test_that("bern vectorized", {
  expect_equal(dev_bern(0:1, 0:1), c(0, 0))
  expect_equal(dev_bern(0:1, 1:0), c(Inf, Inf))
  expect_equal(dev_bern(0:1, c(0.3, 0.6)), c(0.713349887877465, 1.02165124753198))
})

test_that("bern vectorized missing values", {
  expect_equal(dev_bern(c(NA, 1), 0:1), c(NA, 0))
  expect_equal(dev_bern(c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_bern(c(0:1), c(NA, 1)), c(NA, 0))
  expect_equal(dev_bern(c(0:1), c(0, NA)), c(0, NA))
})

test_that("dev_bern res", {
  expect_equal(dev_bern(0, 0.5), dev_bern(0, 0.5, res = TRUE)^2)
  expect_equal(dev_bern(0:1, c(0.3, 0.6)), dev_bern(0:1, c(0.3, 0.6), res = TRUE)^2)
})

test_that("bern log_lik", {
  expect_equal(
    dev_bern(0:1, 0.5),
    2 * (log_lik_bern(0:1, 0:1) - log_lik_bern(0:1, 0.5))
  )
  expect_equal(
    dev_bern(0:1, 0.7),
    2 * (log_lik_bern(0:1, 0:1) - log_lik_bern(0:1, 0.7))
  )
})

test_that("bern deviance", {
  samples <- ran_bern(1000)
  mod <- glm(cbind(samples, 1 - samples) ~ 1, family = binomial)
  deviance <- sum(dev_bern(samples, ilogit(coef(mod)[1])))
  expect_equal(deviance, deviance(mod))
})

test_that("bern ran", {
  set.seed(101)
  samples <- ran_bern(1000)
  res <- dev_bern(samples, res = TRUE)
  expect_equal(mean(res), -0.0494512209456499)
  expect_equal(sd(res), 1.17695971555483)
})

test_that("dev_binom", {
  expect_identical(dev_binom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_binom(NA, 1, 1), NA_real_)
  expect_identical(dev_binom(1, NA, 1), NA_real_)
  expect_identical(dev_binom(1, 1, NA), NA_real_)
  expect_equal(dev_binom(1, 3, 0.5), dev_binom(1, 3, 0.5, res = TRUE)^2)
  expect_equal(dev_binom(0, 1, 0.5, res = TRUE), -1.17741002251547)
  expect_equal(dev_binom(1, 1, 0.5, res = TRUE), 1.17741002251547)
  expect_equal(dev_binom(0, 1, 0.7, res = TRUE), -1.55175565365552)
  expect_equal(dev_binom(1, 1, 0.7, res = TRUE), 0.844600430900592)
  expect_identical(dev_binom(1, 2, 0.5), 0)
  expect_identical(dev_binom(5, 10, 0.5), 0)
  expect_equal(dev_binom(1, 10, 0.5, res = TRUE), -2.71316865369073)
  expect_equal(
    dev_binom(1:9, 10, 0.5, res = TRUE),
    c(
      -2.71316865369073, -1.96338868806845, -1.28283185573988, -0.634594572159089,
      0, 0.634594572159089, 1.28283185573988, 1.96338868806845, 2.71316865369073
    )
  )
})

test_that("deviance binom log_lik", {
  expect_equal(
    dev_binom(0:3, 3, 0.5),
    2 * (log_lik_binom(0:3, 3, 0:3 / 3) - log_lik_binom(0:3, 3, 0.5))
  )
})

test_that("deviance binom", {
  samples <- ran_binom(100, 3)
  mod <- glm(cbind(samples, 3 - samples) ~ 1, family = binomial)
  deviance <- sum(dev_binom(samples, size = 3, ilogit(coef(mod)[1])))
  expect_equal(deviance, deviance(mod))
})

test_that("gamma_pois missing values", {
  expect_identical(dev_gamma_pois(logical(0), integer(0), numeric(0)), numeric(0))
  expect_identical(dev_gamma_pois(NA, 1), NA_real_)
  expect_identical(dev_gamma_pois(1, NA), NA_real_)
  expect_identical(dev_gamma_pois(1, 1, NA), NA_real_)
})

test_that("gamma_pois known values", {
  expect_identical(dev_gamma_pois(1, 1), 0)
  expect_identical(dev_gamma_pois(1, 1, 1), 0)
  expect_identical(dev_gamma_pois(0, 0), 0)
  expect_identical(dev_gamma_pois(0, 0, 1), 0)
  expect_identical(dev_gamma_pois(1, 0), Inf)
  expect_identical(dev_gamma_pois(1, 0, 1), Inf)
  expect_identical(dev_gamma_pois(0, 1), 2)
  expect_equal(dev_gamma_pois(0, 1, 1), 1.38629436111989)
  expect_identical(dev_gamma_pois(0, 2), 4)
  expect_equal(dev_gamma_pois(0, 2, 1), 2.19722457733622)
  expect_equal(dev_gamma_pois(0, 2, 2), 1.6094379124341)
})

test_that("gamma_pois vectorized", {
  expect_equal(dev_gamma_pois(0:3, 2, 0), c(4, 0.613705638880109, 0, 0.432790648648986))
  expect_equal(dev_gamma_pois(0:3, 0:3, rep(1, 4)), rep(0, 4))
  expect_equal(dev_gamma_pois(0:3, 3:0, 0:3), c(6, 0.235566071312767, 0.218460603409828, Inf))
})

test_that("gamma_pois vectorized missing values", {
  expect_equal(dev_gamma_pois(c(NA, 1), 0:1, 0:1), c(NA, 0))
  expect_equal(dev_gamma_pois(c(0, NA), 0:1, 0:1), c(0, NA))
  expect_equal(dev_gamma_pois(c(0:1), c(NA, 1), 0:1), c(NA, 0))
  expect_equal(dev_gamma_pois(c(0:1), c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_gamma_pois(c(0:1), c(0:1), c(NA, 0)), c(NA, 0))
  expect_equal(dev_gamma_pois(c(0:1), c(0:1), c(0, NA)), c(0, NA))
})

test_that("gamma_pois res", {
  expect_equal(dev_gamma_pois(0, 0.5), dev_gamma_pois(0, 0.5, res = TRUE)^2)
  expect_equal(dev_gamma_pois(0:1, c(0.3, 0.6)), dev_gamma_pois(0:1, c(0.3, 0.6), res = TRUE)^2)
})

test_that("gamma_pois log_lik", {
  expect_equal(
    dev_gamma_pois(0:1, 0.5),
    2 * (log_lik_gamma_pois(0:1, 0:1) - log_lik_gamma_pois(0:1, 0.5))
  )
  expect_equal(
    dev_gamma_pois(0:1, 0.7),
    2 * (log_lik_gamma_pois(0:1, 0:1) - log_lik_gamma_pois(0:1, 0.7))
  )
  expect_equal(
    dev_gamma_pois(0:1, 0.7, 1),
    2 * (log_lik_gamma_pois(0:1, 0:1, 1) - log_lik_gamma_pois(0:1, 0.7, 1))
  )
})

test_that("gamma_pois deviance", {
  skip_if_not_installed("MASS")
  samples <- ran_gamma_pois(10000, 3, 0.5)
  mod <- MASS::glm.nb(samples ~ 1)
  deviance <- sum(dev_gamma_pois(samples, exp(coef(mod)[1]), theta = 1 / mod$theta))
  expect_equal(deviance, deviance(mod))
})

test_that("gamma_pois ran", {
  set.seed(100)
  samples <- ran_gamma_pois(100000, 3, 0.5)
  expect_equal(mean(samples), 3.00867)
  expect_equal(var(samples), 7.50806991179912)
  res <- dev_gamma_pois(samples, 3, 0.5, res = TRUE)
  expect_equal(mean(res), -0.260848965857529)
  expect_equal(sd(res), 1.0243876393499)
})

test_that("gamma_pois_zi missing values", {
  expect_identical(dev_gamma_pois_zi(logical(0), integer(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(dev_gamma_pois_zi(NA, 1, 1, 1), NA_real_)
  expect_identical(dev_gamma_pois_zi(1, NA, 1, 1), NA_real_)
  expect_identical(dev_gamma_pois_zi(1, 1, NA, 1), NA_real_)
  expect_identical(dev_gamma_pois_zi(1, 1, 1, NA), NA_real_)
})

test_that("gamma_pois_zi known values", {
  expect_identical(dev_gamma_pois_zi(1, 1), 0)
  expect_identical(dev_gamma_pois_zi(1, 1, 1), 0)
  expect_identical(dev_gamma_pois_zi(1, 1, 1, 1), Inf)
  expect_identical(dev_gamma_pois_zi(0, 0), 0)
  expect_identical(dev_gamma_pois_zi(0, 0, 1), 0)
  expect_identical(dev_gamma_pois_zi(0, 0, 1, 1), 0)
  expect_identical(dev_gamma_pois_zi(1, 0), Inf)
  expect_identical(dev_gamma_pois_zi(1, 0, 1), Inf)
  expect_identical(dev_gamma_pois_zi(1, 0, 1, 1), Inf)
  expect_identical(dev_gamma_pois_zi(0, 1), 2)
  expect_equal(dev_gamma_pois_zi(0, 1, 1), 1.38629436111989)
  expect_equal(dev_gamma_pois_zi(0, 1, 1, 0.5), 0.575364144903562)
  expect_equal(dev_gamma_pois_zi(0, 1, 1, 1), 0)
  expect_identical(dev_gamma_pois_zi(0, 2), 4)
  expect_equal(dev_gamma_pois_zi(0, 2, 1), 2.19722457733622)
  expect_equal(dev_gamma_pois_zi(0, 2, 1, 0.1), 1.83258146374831)
  expect_equal(dev_gamma_pois_zi(0, 2, 1, 0.5), 0.810930216216329)
  expect_equal(dev_gamma_pois_zi(0, 2, 2), 1.6094379124341)
  expect_equal(dev_gamma_pois_zi(0, 2, 2, 0.1), 1.37635018002824)
  expect_equal(dev_gamma_pois_zi(0, 2, 2, 0.5), 0.647014262314894)
})

test_that("gamma_pois_zi vectorized", {
  expect_equal(dev_gamma_pois_zi(0:3, 2, 0, 0), c(4, 0.613705638880109, 0, 0.432790648648986))
  expect_equal(dev_gamma_pois_zi(c(0, 1, 3, 0), 3, 0.5, 0.5), c(1.08945435088334, 2.25402352637961, 1.38629436111989, 1.08945435088334))
  expect_equal(dev_gamma_pois_zi(0:3, 0:3, rep(1, 4), 0), rep(0, 4))
  expect_equal(dev_gamma_pois_zi(0:3, 3:0, 0:3, seq(0, 1, length.out = 4)), c(6, 1.0464962875291, 2.41568518074605, Inf))
})

test_that("gamma_pois_zi vectorized missing values", {
  expect_equal(dev_gamma_pois_zi(c(NA, 1), 0:1, 0:1, 0:1), c(NA, Inf))
  expect_equal(dev_gamma_pois_zi(c(0, NA), 0:1, 0:1, 0:1), c(0, NA))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(NA, 1), 0:1, 0:1), c(NA, Inf))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(0, NA), 0:1, 0:1), c(0, NA))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(0:1), c(NA, 1), 0:1), c(NA, Inf))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(0:1), c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(0:1), 0:1, c(NA, 1)), c(NA, Inf))
  expect_equal(dev_gamma_pois_zi(c(0:1), c(0:1), 0:1, c(0, NA)), c(0, NA))
})

test_that("gamma_pois_zi res", {
  expect_equal(dev_gamma_pois_zi(0, 0.5, 0.5), dev_gamma_pois_zi(0, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_gamma_pois_zi(0:1, c(0.3, 0.6), 0.5), dev_gamma_pois_zi(0:1, c(0.3, 0.6), 0.5, res = TRUE)^2)
})

test_that("gamma_pois_zi log_lik", {
  expect_equal(
    dev_gamma_pois_zi(0:1, 0.5),
    2 * (log_lik_gamma_pois_zi(0:1, 0:1) - log_lik_gamma_pois_zi(0:1, 0.5))
  )
  expect_equal(
    dev_gamma_pois_zi(0:1, 0.7),
    2 * (log_lik_gamma_pois_zi(0:1, 0:1) - log_lik_gamma_pois_zi(0:1, 0.7))
  )
  expect_equal(
    dev_gamma_pois_zi(0:1, 0.7, 1),
    2 * (log_lik_gamma_pois_zi(0:1, 0:1, 1) - log_lik_gamma_pois_zi(0:1, 0.7, 1))
  )
  expect_equal(
    dev_gamma_pois_zi(0:1, 0.7, 1, 0.5),
    2 * (log_lik_gamma_pois_zi(0:1, 0:1, 1) - log_lik_gamma_pois_zi(0:1, 0.7, 1, 0.5))
  )
})

test_that("gamma_pois_zi ran", {
  set.seed(100)
  samples <- ran_gamma_pois_zi(100000, 3, 0.5, 0.5)
  expect_equal(mean(samples), 1.51352)
  expect_equal(var(samples), 6.07855799517995)
  res <- dev_gamma_pois_zi(samples, 3, 0.5, 0.5, res = TRUE)
  expect_equal(mean(res), -0.299429507479032)
  expect_equal(sd(res), 1.18261741535124)
})

test_that("dev_lnorm", {
  expect_equal(
    dev_lnorm(3, 4, 5),
    2 * (log_lik_lnorm(3, log(3), 5) - log_lik_lnorm(3, 4, 5))
  )

  expect_identical(dev_lnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_lnorm(exp(0)), 0)
  expect_identical(dev_lnorm(1), 0)
  expect_identical(dev_lnorm(0, res = TRUE), -Inf)
  expect_identical(dev_lnorm(0), Inf)
  expect_identical(dev_lnorm(-1, res = TRUE), -Inf)
  expect_identical(dev_lnorm(NA, 1, 1), NA_real_)
  expect_identical(dev_lnorm(1, NA, 1), NA_real_)
  expect_identical(dev_lnorm(1, 1, NA), NA_real_)
  expect_equal(dev_lnorm(-2), dev_lnorm(-2, res = TRUE)^2)
  expect_equal(dev_lnorm(exp(-2:2), res = TRUE), c(-2, -1, 0, 1, 2))
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 2, res = TRUE), dev_norm(-2:2, res = TRUE) / 2)
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 1 / 2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_lnorm(exp(-2:2), meanlog = -2:2), rep(0, 5))
  expect_equal(
    dev_lnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5, res = TRUE),
    c(-1, -0.5, -0.333333333333333, -0.25, -0.2)
  )
})

test_that("deviance lnorm", {
  samples <- ran_lnorm(100)
  mod <- lm(log(samples) ~ 1)
  deviance <- sum(dev_lnorm(samples, coef(mod)[1]))
  expect_equal(deviance, deviance(mod))
})

test_that("dev_neg_bin", {
  expect_identical(dev_neg_binom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_neg_binom(1, 1, 0), 0)
  expect_identical(dev_neg_binom(1, 1, 1), 0)
  expect_identical(dev_neg_binom(0, 1, 0), 2)
  expect_equal(dev_neg_binom(0, 1, 2), 1.09861228866811)
  expect_equal(dev_neg_binom(0, 1, 1), 1.386294361119891)

  expect_identical(dev_neg_binom(NA, 1, 1), NA_real_)
  expect_identical(dev_neg_binom(1, NA, 1), NA_real_)
  expect_identical(dev_neg_binom(1, 1, NA), NA_real_)
  expect_equal(dev_neg_binom(1, 3, 1), dev_neg_binom(1, 3, 1, res = TRUE)^2)

  expect_equal(
    dev_neg_binom(c(1, 2, 5), 4, 1 / 2, res = TRUE),
    c(-1.177410022515, -0.686390663271, 0.270787731555)
  )
  expect_equal(
    dev_neg_binom(c(1, 2, 5), c(1, 3, 5), 1 / 2, res = TRUE),
    c(0, -0.404089071964, 0)
  )
})


test_that("dev_norm", {
  expect_equal(
    dev_norm(3, 4, 5),
    2 * (log_lik_norm(3, 3, 5) - log_lik_norm(3, 4, 5))
  )
  expect_identical(dev_norm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_norm(0), 0)
  expect_identical(dev_norm(NA, 1, 1), NA_real_)
  expect_identical(dev_norm(1, NA, 1), NA_real_)
  expect_identical(dev_norm(1, 1, NA), NA_real_)
  expect_equal(dev_norm(-2), dev_norm(-2, res = TRUE)^2)
  expect_equal(dev_norm(-2:2, res = TRUE), c(-2, -1, 0, 1, 2))
  expect_equal(dev_norm(-2:2, sd = 2, res = TRUE), dev_norm(-2:2, res = TRUE) / 2)
  expect_equal(dev_norm(-2:2, sd = 1 / 2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_norm(-2:2, mean = -2:2, res = TRUE), rep(0, 5))
  expect_equal(
    dev_norm(-2:2, mean = -1:3, sd = 1:5, res = TRUE),
    c(-1, -0.5, -0.333333333333333, -0.25, -0.2)
  )
})

test_that("deviance norm", {
  samples <- ran_norm(100)
  mod <- lm(samples ~ 1)
  deviance <- sum(dev_norm(samples, coef(mod)[1]))
  expect_equal(deviance, deviance(mod))
})

test_that("pois missing values", {
  expect_identical(dev_pois(logical(0), integer(0)), numeric(0))
  expect_identical(dev_pois(NA, 1), NA_real_)
  expect_identical(dev_pois(1, NA), NA_real_)
})

test_that("pois known values", {
  expect_identical(dev_pois(1, 1), 0)
  expect_identical(dev_pois(0, 0), 0)
  expect_identical(dev_pois(1, 0), Inf)
  expect_identical(dev_pois(0, 1), 2)
  expect_identical(dev_pois(0, 2), 4)
  expect_equal(dev_pois(1, 2), 0.613705638880109)
  expect_identical(dev_pois(2, 2), 0)
  expect_equal(dev_pois(3, 2), 0.432790648648986)
  expect_equal(dev_pois(3, 2.5), 0.0939293407637276)
})

test_that("pois vectorized", {
  expect_equal(dev_pois(0:3, 2), c(4, 0.613705638880109, 0, 0.432790648648986))
  expect_equal(dev_pois(0:3, 0:3), rep(0, 4))
  expect_equal(dev_pois(0:3, 3:0), c(6, 0.613705638880109, 0.772588722239781, Inf))
})

test_that("pois vectorized missing values", {
  expect_equal(dev_pois(c(NA, 1), 0:1), c(NA, 0))
  expect_equal(dev_pois(c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_pois(c(0:1), c(NA, 1)), c(NA, 0))
  expect_equal(dev_pois(c(0:1), c(0, NA)), c(0, NA))
})

test_that("pois res", {
  expect_equal(dev_pois(0, 0.5), dev_pois(0, 0.5, res = TRUE)^2)
  expect_equal(dev_pois(0:1, c(0.3, 0.6)), dev_pois(0:1, c(0.3, 0.6), res = TRUE)^2)
})

test_that("pois log_lik", {
  expect_equal(
    dev_pois(0:1, 0.5),
    2 * (log_lik_pois(0:1, 0:1) - log_lik_pois(0:1, 0.5))
  )
  expect_equal(
    dev_pois(0:1, 0.7),
    2 * (log_lik_pois(0:1, 0:1) - log_lik_pois(0:1, 0.7))
  )
})

test_that("pois deviance", {
  samples <- ran_pois(1000)
  mod <- glm(samples ~ 1, family = poisson)
  deviance <- sum(dev_pois(samples, exp(coef(mod)[1])))
  expect_equal(deviance, deviance(mod))
})

test_that("pois ran", {
  set.seed(101)
  samples <- ran_pois(1000)
  expect_equal(mean(samples), 0.984)
  expect_equal(var(samples), 1.02476876876877)
  res <- dev_pois(samples, 1, res = TRUE)
  expect_equal(mean(res), -0.235540962010425)
  expect_equal(sd(res), 1.05758754072283)
})

test_that("pois_zi missing values", {
  expect_identical(dev_pois_zi(logical(0), integer(0), numeric(0)), numeric(0))
  expect_identical(dev_pois_zi(NA, 1, 1), NA_real_)
  expect_identical(dev_pois_zi(1, NA, 1), NA_real_)
  expect_identical(dev_pois_zi(1, 1, NA), NA_real_)
})

test_that("pois_zi known values", {
  expect_identical(dev_pois_zi(1, 1), 0)
  expect_equal(dev_pois_zi(1, 1, 0.5), 1.38629436111989)
  expect_identical(dev_pois_zi(1, 1, 1), Inf)
  expect_identical(dev_pois_zi(0, 1), 2)
  expect_identical(dev_pois_zi(0, 1, 0.5), 0.759770986083445)
  expect_identical(dev_pois_zi(0, 1, 1), 0)
  expect_identical(dev_pois_zi(0, 1000), Inf)
  expect_equal(dev_pois_zi(0, 1000, 0.5), 1.38629436111989)
  expect_equal(dev_pois_zi(0, 1000, 1), 0)
  expect_identical(dev_pois_zi(0, 0), 0)
  expect_identical(dev_pois_zi(0, 0, 1), 0)
  expect_identical(dev_pois_zi(1, 0), Inf)
  expect_identical(dev_pois_zi(1, 0, 1), Inf)
  expect_identical(dev_pois_zi(0, 2), 4)
  expect_equal(dev_pois_zi(0, 2, 0.5), 1.13243833903395)
  expect_equal(dev_pois_zi(1, 2), 0.613705638880109)
  expect_equal(dev_pois_zi(1, 2, 0.5), 2)
  expect_equal(
    dev_pois_zi(3, 3.5, res = TRUE),
    -0.274036349845144
  )
  expect_equal(
    dev_pois_zi(3, 3.5, 0.1, res = TRUE),
    -0.534618511045121
  )
  expect_equal(
    dev_pois_zi(3, 3.5, 0.2, res = TRUE),
    0.72206857268882
  )
  expect_equal(
    dev_pois_zi(3, 3.5, 0.90, res = TRUE),
    2.16339226841194
  )
})

test_that("pois_zi vectorized", {
  expect_equal(dev_pois_zi(0:3, 2, 0), c(4, 0.613705638880109, 0, 0.432790648648986))
  expect_equal(dev_pois_zi(0:3, 0:3, c(0, 0.1, 0.5, 1)), c(0, 0.210721031315653, 1.38629436111989, Inf))
  expect_equal(dev_pois_zi(0:3, 3:0, c(0, 0.1, 0.5, 1)), c(6, 0.824426670195762, 2.15888308335967, Inf))
})

test_that("pois_zi vectorized missing values", {
  expect_equal(dev_pois_zi(c(NA, 1), 0:1, 0:1), c(NA, Inf))
  expect_equal(dev_pois_zi(c(0, NA), 0:1, 0:1), c(0, NA))
  expect_equal(dev_pois_zi(c(0:1), c(NA, 1), 0:1), c(NA, Inf))
  expect_equal(dev_pois_zi(c(0:1), c(0, NA), 0:1), c(0, NA))
  expect_equal(dev_pois_zi(c(0:1), 0:1, c(NA, 1)), c(NA, Inf))
  expect_equal(dev_pois_zi(c(0:1), 0:1, c(1, NA)), c(0, NA))
})

test_that("pois_zi res", {
  expect_equal(dev_pois_zi(0, 0.5, 0.5), dev_pois_zi(0, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_pois_zi(0:1, c(0.3, 0.6), 0.5), dev_pois_zi(0:1, c(0.3, 0.6), 0.5, res = TRUE)^2)
})

test_that("pois_zi log_lik", {
  expect_equal(
    dev_pois_zi(0, 0.5),
    2 * (log_lik_pois_zi(0, 0) - log_lik_pois_zi(0, 0.5))
  )
  expect_equal(
    dev_pois_zi(0, 0.5, 0.5),
    2 * (log_lik_pois_zi(0, 0, 0.5) - log_lik_pois_zi(0, 0.5, 0.5))
  )
  expect_equal(
    dev_pois_zi(1, 0.5),
    2 * (log_lik_pois_zi(1, 1) - log_lik_pois_zi(1, 0.5))
  )
  expect_equal(
    dev_pois_zi(1, 0.5, 0.5),
    2 * (log_lik_pois_zi(1, 1, 0) - log_lik_pois_zi(1, 0.5, 0.5))
  )
})

test_that("pois_zi ran", {
  set.seed(101)
  samples <- ran_pois_zi(1000, 3, 0.5)
  expect_equal(mean(samples), 1.565)
  expect_equal(sd(samples), 1.94462247147269)
  res <- dev_pois_zi(samples, 3, 0.5, res = TRUE)
  expect_equal(mean(res), -0.119914019357374)
  expect_equal(sd(res), 1.31269320210938)
})

test_that("gamma missing values", {
  expect_identical(dev_gamma(logical(0), integer(0), numeric(0)), numeric(0))
  expect_identical(dev_gamma(NA, 1, 1), NA_real_)
  expect_identical(dev_gamma(1, NA, 1), NA_real_)
  expect_identical(dev_gamma(1, 1, NA), NA_real_)
})

test_that("gamma known values", {
  expect_identical(dev_gamma(1, 1), 0)
  expect_equal(dev_gamma(1, 1, 0.5), 0.386294361119891)
  expect_identical(dev_gamma(1, 1, 1), 0)
  expect_identical(dev_gamma(0, 1), Inf)
  expect_identical(dev_gamma(0, 1, 0.5), Inf)
  expect_equal(dev_gamma(1, 1, 0.5), 0.386294361119891)
  expect_equal(dev_gamma(1, 1000, 1), 11.8175105579643)
  expect_equal(dev_gamma(0, 2, 0.5), Inf)
  expect_equal(dev_gamma(1, 2), 0.386294361119891)
  expect_equal(dev_gamma(1, 2, 0.5), 1.27258872223978)
  expect_equal(
    dev_gamma(3, 3.5, res = TRUE),
    -0.150289966199447
  )
  expect_equal(
    dev_gamma(3, 3.5, 0.1, res = TRUE),
    -1.75638837307447
  )
  expect_equal(
    dev_gamma(3, 3.5, 0.2, res = TRUE),
    -1.36749198439328
  )
  expect_equal(
    dev_gamma(3, 3.5, 0.90, res = TRUE),
    -0.248755972445511
  )
})

test_that("gamma vectorized", {
  expect_equal(dev_gamma(0:3, 2, 1), c(Inf, 0.386294361119891, 0, 0.189069783783671))
  expect_equal(
    dev_gamma(0:3, 1:4, c(0.1, 0.5, 1, 2)),
    c(Inf, 1.27258872223978, 0.144263549549662, 0.189069783783671)
  )
  expect_equal(
    dev_gamma(0:3, 4:1, c(0.1, 0.5, 1, 2)),
    c(Inf, 1.91685227178944, 0, 6.41648106154389)
  )
})

test_that("gamma vectorized missing values", {
  expect_equal(dev_gamma(c(NA, 1), 1:2, 1:2), c(NA, 0))
  expect_equal(dev_gamma(c(0, NA), 1:2, 1:2), c(Inf, NA))
  expect_equal(dev_gamma(c(1:2), c(NA, 1), 1:2), c(NA, 3.22741127776022))
  expect_equal(dev_gamma(c(1:2), c(1, NA), 1:2), c(0, NA))
  expect_equal(dev_gamma(c(1:2), 1:2, c(NA, 1)), c(NA, 0))
  expect_equal(dev_gamma(c(1:2), 1:2, c(1, NA)), c(0, NA))
})

test_that("gamma res", {
  expect_equal(dev_gamma(0, 0.5, 0.5), dev_gamma(0, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_gamma(1:2, c(0.3, 0.6), 0.5), dev_gamma(1:2, c(0.3, 0.6), 0.5, res = TRUE)^2)
})

test_that("gamma log_lik", { # couldn't determine exact parameter values for saturated log likelihood
  expect_equal(
    dev_gamma(1, 1, 1),
    2 * (log_lik_gamma(1, 1, 1) - log_lik_gamma(1, 1, 1))
  )
  expect_equal(
    dev_gamma(2, 1, 1),
    2 * (log_lik_gamma(2, 1, 0.5) - log_lik_gamma(2, 1, 1))
  )
  # hack to divide multiply by 1 / shape
  expect_equal(
    dev_gamma(5, 3, 2),
    2 * (1 / 3) * (log_lik_gamma(5, 3, 3 / 5) - log_lik_gamma(5, 3, 2))
  )
})

test_that("gamma ran", {
  set.seed(101)
  samples <- ran_gamma(10000, 3, 1)
  expect_equal(mean(samples), 2.98927441565774)
  expect_equal(sd(samples), 1.71676055992005)
  res <- dev_gamma(samples, 3, 1, res = TRUE)
  expect_equal(mean(res), -0.115132947326996)
  expect_equal(sd(res), 0.579070230993174)
})

test_that("gamma deviance", {
  samples <- ran_gamma(1000, 3, 1 / 2)
  mod <- glm(samples ~ 1, family = Gamma(link = "identity"))
  deviance <- sum(dev_gamma(samples, coef(mod)))
  expect_equal(deviance, deviance(mod))
})

test_that("student missing values", {
  expect_identical(dev_student(logical(0), integer(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(dev_student(NA, 1, 1, 1), NA_real_)
  expect_identical(dev_student(1, NA, 1, 1), NA_real_)
  expect_identical(dev_student(1, 1, NA, 1), NA_real_)
  expect_identical(dev_student(1, 1, 1, NA), NA_real_)
})

test_that("student known values", {
  expect_identical(dev_student(1, 1), 0)
  expect_identical(dev_student(1, 1, 1), 0)
  expect_identical(dev_student(1, 1, 1, 1), 0)
  expect_identical(dev_student(0, 0), 0)
  expect_identical(dev_student(0, 0, 1), 0)
  expect_identical(dev_student(0, 0, 1, 1), 0)
  expect_identical(dev_student(0, 0, 0), dev_norm(0, 0, 0))
  expect_identical(dev_student(0, 0, 0, 10), dev_norm(0, 0, 0))
  expect_equal(dev_student(1, 0), 1)
  expect_equal(dev_student(1, 0, 1), 1)
  expect_equal(dev_student(1, 0, 1, 1), 1.38629436111989)
  expect_equal(dev_student(0, 1), 1)
  expect_equal(dev_student(0, 1, 1), 1)
  expect_equal(dev_student(0, 1, 1, 0.5), 1.21639532432449)
  expect_equal(dev_student(0, 1, 1, 1), 1.38629436111989)
  expect_identical(dev_student(0, 2), 4)
  expect_identical(dev_student(0, 2, 1), 4)
  expect_equal(dev_student(0, 2, 1, 0.1), 3.70119460283334)
  expect_equal(dev_student(0, 2, 1, 0.5), 3.29583686600433)
  expect_equal(dev_student(0, 2, 2), 1)
  expect_equal(dev_student(0, 2, 2, 0.1), 1.04841197784757)
  expect_equal(dev_student(0, 2, 2, 0.5), 1.21639532432449)
})

test_that("student vectorized", {
  expect_equal(dev_student(0:3, 2, 0.1, 0), c(400, 100, 0, 100))
  expect_equal(dev_student(c(0, 1, 3, 0), 3, 0.5, 0.5), c(8.83331693749932, 6.59167373200866, 0, 8.83331693749932))
  expect_equal(dev_student(0:3, 0:3, rep(1, 4), 0), rep(0, 4))
  expect_equal(dev_student(0:3, 3:0, 0:3, seq(0, 1, length.out = 4)), c(Inf, 1.15072828980712, 0.385376699568146, 1.38629436111989))
})

test_that("student vectorized missing values", {
  expect_equal(dev_student(c(NA, 1), 0:1, 0:1, 0:1), c(NA, 0))
  expect_equal(dev_student(c(0, NA), 0:1, 1:2, 0:1), c(0, NA))
  expect_equal(dev_student(c(0:1), c(NA, 1), 1:2, 0:1), c(NA, 0))
  expect_equal(dev_student(c(0:1), c(0, NA), 1:2, 0:1), c(0, NA))
  expect_equal(dev_student(c(0:1), c(0:1), c(NA, 1), 0:1), c(NA, 0))
  expect_equal(dev_student(c(0:1), c(0:1), c(1, NA), 0:1), c(0, NA))
  expect_equal(dev_student(c(0:1), c(0:1), 0:1, c(NA, 1)), c(NaN, 0))
  expect_equal(dev_student(c(0:1), c(0:1), 1:2, c(0, NA)), c(0, NA))
})

test_that("student res", {
  expect_equal(dev_student(10, 0.5, 0.5), dev_student(10, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_student(0:1, c(0.3, 0.6), 0.5), dev_student(0:1, c(0.3, 0.6), 0.5, res = TRUE)^2)
})

test_that("student log_lik", {
  expect_equal(
    dev_student(0:1, 1:2, 2:3, theta = 0),
    2 * (log_lik_student(0:1, 0:1, 2:3, theta = 0) - log_lik_student(0:1, 1:2, 2:3, theta = 0))
  )
  expect_equal(
    dev_student(0:1, 1:2, 2:3, theta = 0.7),
    2 * (log_lik_student(0:1, 0:1, 2:3, theta = 0.7) - log_lik_student(0:1, 1:2, 2:3, theta = 0.7))
  )
  expect_equal(
    dev_student(1, 0.7, 1, 5),
    2 * (log_lik_student(1, 1, 1, 5) - log_lik_student(1, 0.7, 1, 5))
  )
})

test_that("student ran", {
  set.seed(101)
  samples <- ran_student(100000, 3, 0.5, 0.5)
  expect_equal(mean(samples), 3.00401173552349)
  expect_equal(var(samples), 2.47432193782075)
  res <- dev_student(samples, 3, 0.5, 0.5, res = TRUE)
  expect_equal(mean(res), -0.00261197674374733)
  expect_equal(sd(res), 1.35218891518458)
})

test_that("student deviance", {
  samples <- ran_student(1000, 3, 0.5, 0)
  mod <- glm(samples ~ 1, family = gaussian)
  deviance <- sum(dev_student(samples, coef(mod)[1]))
  expect_equal(deviance, deviance(mod))
})

test_that("skewnorm missing values", {
  skip_if_not_installed("sn")
  expect_identical(dev_skewnorm(logical(0), integer(0), numeric(0), numeric(0)), numeric(0))
  expect_identical(dev_skewnorm(NA, 1, 1, 1), NA_real_)
  expect_identical(dev_skewnorm(1, NA, 1, 1), NA_real_)
  expect_identical(dev_skewnorm(1, 1, NA, 1), NA_real_)
  expect_identical(dev_skewnorm(1, 1, 1, NA), NA_real_)
})

test_that("skewnorm known values", {
  skip_if_not_installed("sn")
  expect_identical(dev_skewnorm(1, 1), 0)
  expect_identical(dev_skewnorm(1, 1, 1), 0)
  expect_equal(dev_skewnorm(1, 1, 1, 1), 0.398456311678723)
  expect_equal(dev_skewnorm(0, 0), 0)
  expect_equal(dev_skewnorm(0, 0, 1), 0)
  expect_equal(dev_skewnorm(0, 0, 1, 1), 0.398456311678723)
  expect_identical(dev_skewnorm(0, 0, 0), dev_norm(0, 0, 0))
  expect_equal(dev_skewnorm(0, 1, 10, 0), dev_norm(0, 1, 10))
  expect_equal(dev_skewnorm(1, 0), 1)
  expect_equal(dev_skewnorm(1, 0, 1), 1)
  expect_equal(dev_skewnorm(1, 0, 1, 10), 0.925959042346759)
  expect_equal(dev_skewnorm(0, 1, 5, -10), 0.0119848610046867)
  expect_equal(dev_skewnorm(0, 1, 1, 100), 10012.0431099887)
  expect_equal(dev_skewnorm(20, 1, 1, 0), 361)
  expect_equal(dev_skewnorm(10, 1, 5, 15), 3.20046804194821)
  expect_equal(dev_skewnorm(0, 2, 5, -10), 0.0860223858335147)
  expect_equal(dev_skewnorm(0, 2, 1, -0.3), 3.30902744735532)
  expect_equal(dev_skewnorm(3, 2, 1, 0.1), 0.853041620995908)
  expect_equal(dev_skewnorm(13, 2, 1, 0.5), 119.751389326937)
  expect_equal(dev_skewnorm(0, 2, 2, 10), 107.388529343372)
  expect_equal(dev_skewnorm(0, 2, 2, -2), 0.452806719152662)
  expect_equal(dev_skewnorm(0, 2, 2, 13), 176.930038215879)
})

test_that("skewnorm vectorized", {
  skip_if_not_installed("sn")
  expect_equal(dev_skewnorm(0:3, 2, 0.1, 0), c(400, 100, 0, 100))
  expect_equal(dev_skewnorm(c(0, 1, 3, 0), 3, 0.5, 0.5), c(47.9668417319782, 22.3177579563216, 0.137683650077409, 47.9668417319782))
  expect_equal(dev_skewnorm(0:3, 0:3, rep(1, 4), 0), rep(0, 4))
  expect_equal(dev_skewnorm(0:3, 3:0, 0:3, seq(0, 1, length.out = 4)), c(Inf, 1.67133656780082, 0.00816257443560442, 0.357669508605733))
})

test_that("skewnorm vectorized missing values", {
  skip_if_not_installed("sn")
  expect_equal(dev_skewnorm(c(NA, 1), 0:1, 0:1, 0:1), c(NA, 0.398456311678723))
  expect_equal(dev_skewnorm(c(0, NA), 0:1, 1:2, 0:1), c(0, NA))
  expect_equal(dev_skewnorm(c(0:1), c(NA, 1), 1:2, 0:1), c(NA, 0.398456311678724))
  expect_equal(dev_skewnorm(c(0:1), c(0, NA), 1:2, 0:1), c(0, NA))
  expect_equal(dev_skewnorm(c(0:1), c(0:1), c(NA, 1), 0:1), c(NA, 0.398456311678723))
  expect_equal(dev_skewnorm(c(0:1), c(0:1), c(1, NA), 0:1), c(0, NA))
  expect_equal(dev_skewnorm(c(0:1), c(0:1), 0:1, c(NA, 1)), c(NA, 0.398456311678723))
  expect_equal(dev_skewnorm(c(0:1), c(0:1), 1:2, c(0, NA)), c(0, NA))
})

test_that("skewnorm res", {
  skip_if_not_installed("sn")
  expect_equal(dev_skewnorm(10, 0.5, 0.5), dev_skewnorm(10, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_skewnorm(0:1, c(0.3, 0.6), 0.5), dev_skewnorm(0:1, c(0.3, 0.6), 0.5, res = TRUE)^2)
})

test_that("skewnorm log_lik", {
  skip_if_not_installed("sn")
  expect_equal(
    dev_skewnorm(0:1, 1:2, 2:3, shape = 0),
    2 * (log_lik_skewnorm(0:1, 0:1, 2:3, shape = 0) - log_lik_skewnorm(0:1, 1:2, 2:3, shape = 0))
  )
})

test_that("skewnorm ran", {
  skip_if_not_installed("sn")
  set.seed(101)
  samples <- ran_skewnorm(100000, 3, 0.5, 0.5)
  expect_equal(mean(samples), 3.17851201086154)
  expect_equal(var(samples), 0.215894351753986)
  res <- dev_skewnorm(samples, 3, 0.5, 0.5, res = TRUE)
  expect_equal(mean(res), 0.00818696278884169)
  expect_equal(sd(res), 0.995441552601596)
})

test_that("skewnorm deviance", {
  skip_if_not_installed("sn")
  samples <- ran_skewnorm(1000, 3, 0.5, 0)
  mod <- glm(samples ~ 1, family = gaussian)
  deviance <- sum(dev_skewnorm(samples, coef(mod)[1]))
  expect_equal(deviance, deviance(mod))
})
