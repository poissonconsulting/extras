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
  expect_equal(dev_bern(0:1, 0:1), c(0,0))
  expect_equal(dev_bern(0:1, 1:0), c(Inf,Inf))
  expect_equal(dev_bern(0:1, c(0.3,0.6)), c(0.713349887877465, 1.02165124753198))
})

test_that("bern vectorized missing values", {
  expect_equal(dev_bern(c(NA,1), 0:1), c(NA,0))
  expect_equal(dev_bern(c(0,NA), 0:1), c(0,NA))
  expect_equal(dev_bern(c(0:1), c(NA,1)), c(NA,0))
  expect_equal(dev_bern(c(0:1), c(0,NA)), c(0,NA))
})

test_that("dev_bern res", {
  expect_equal(dev_bern(0, 0.5), dev_bern(0, 0.5, res = TRUE)^2)
  expect_equal(dev_bern(0:1, c(0.3,0.6)), dev_bern(0:1, c(0.3,0.6), res = TRUE)^2)
})

test_that("bern log_lik", {
  expect_equal(dev_bern(0:1, 0.5),
               2 * (log_lik_bern(0:1, 0:1) - log_lik_bern(0:1, 0.5)))
  expect_equal(dev_bern(0:1, 0.7),
               2 * (log_lik_bern(0:1, 0:1) - log_lik_bern(0:1, 0.7)))
})

test_that("bern deviance", {
  samples <- ran_bern(1000)
  mod <- glm(cbind(samples,1-samples)~1, family = binomial)
  deviance <- sum(dev_bern(samples, ilogit(coef(mod)[1])))
  expect_equal(deviance, deviance(mod))
})

test_that("bern ran", {
  set.seed(101)
  samples <- ran_bern(1000)
  res <- dev_bern(samples, res = TRUE)
  expect_equal(mean(res),-0.0494512209456499)
  expect_equal(sd(res),1.17695971555483)
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
  expect_equal(dev_binom(1:9, 10, 0.5, res = TRUE),
               c(-2.71316865369073, -1.96338868806845, -1.28283185573988, -0.634594572159089,
                 0, 0.634594572159089, 1.28283185573988, 1.96338868806845, 2.71316865369073
               ))
})

test_that("deviance binom log_lik", {
  expect_equal(dev_binom(0:3, 3, 0.5),
               2 * (log_lik_binom(0:3, 3, 0:3/3) - log_lik_binom(0:3, 3, 0.5)))
})

test_that("deviance binom", {
  samples <- ran_binom(100, 3)
  mod <- glm(cbind(samples,3-samples)~1, family = binomial)
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
  expect_equal(dev_gamma_pois(c(NA,1), 0:1, 0:1), c(NA,0))
  expect_equal(dev_gamma_pois(c(0,NA), 0:1, 0:1), c(0,NA))
  expect_equal(dev_gamma_pois(c(0:1), c(NA,1), 0:1), c(NA,0))
  expect_equal(dev_gamma_pois(c(0:1), c(0,NA), 0:1), c(0,NA))
  expect_equal(dev_gamma_pois(c(0:1), c(0:1), c(NA,0)), c(NA,0))
  expect_equal(dev_gamma_pois(c(0:1), c(0:1), c(0,NA)), c(0,NA))
})

test_that("gamma_pois res", {
  expect_equal(dev_gamma_pois(0, 0.5), dev_gamma_pois(0, 0.5, res = TRUE)^2)
  expect_equal(dev_gamma_pois(0:1, c(0.3,0.6)), dev_gamma_pois(0:1, c(0.3,0.6), res = TRUE)^2)
})

test_that("gamma_pois log_lik", {
  expect_equal(dev_gamma_pois(0:1, 0.5),
               2 * (log_lik_gamma_pois(0:1, 0:1) - log_lik_gamma_pois(0:1, 0.5)))
  expect_equal(dev_gamma_pois(0:1, 0.7),
               2 * (log_lik_gamma_pois(0:1, 0:1) - log_lik_gamma_pois(0:1, 0.7)))
  expect_equal(dev_gamma_pois(0:1, 0.7, 1),
               2 * (log_lik_gamma_pois(0:1, 0:1, 1) - log_lik_gamma_pois(0:1, 0.7, 1)))
})

test_that("gamma_pois deviance", {
  samples <- ran_gamma_pois(10000, 3, 0.5)
  mod <- MASS::glm.nb(samples~1)
  deviance <- sum(dev_gamma_pois(samples, exp(coef(mod)[1]), theta = 1/mod$theta))
  expect_equal(deviance, deviance(mod))
})

test_that("gamma_pois ran", {
  set.seed(100)
  samples <- ran_gamma_pois(100000, 3, 0.5)
  expect_equal(mean(samples), 3.00867)
  expect_equal(var(samples), 7.50806991179912)
  res <- dev_gamma_pois(samples, 3, 0.5, res = TRUE)
  expect_equal(mean(res),-0.260848965857529)
  expect_equal(sd(res), 1.0243876393499)
})
#
# test_that("dev_gamma_pois_zi", {
#   expect_equal(dev_gamma_pois_zi(1,2),
#                2 * (log_lik_gamma_pois_zi(1, 1) - log_lik_pois(1, 2)))
#   expect_equal(dev_gamma_pois_zi(1,2,1),
#                2 * (log_lik_gamma_pois_zi(1, 1, 1) - log_lik_gamma_pois(1, 2, 1)))
#   expect_equal(dev_gamma_pois_zi(1,2,0,0.5),
#                2 * (log_lik_gamma_pois_zi(1, 1, 0, 0.5) - log_lik_pois_zi(1, 2, 0.5)))
#   expect_identical(dev_gamma_pois_zi(integer(0), integer(0), integer(0), integer(0)), numeric(0))
#   expect_identical(dev_gamma_pois_zi(1, 1, 0, 0), 0)
#   expect_identical(dev_gamma_pois_zi(1, 1, 0, 0.5), 0)
#   expect_identical(dev_gamma_pois_zi(0, 1, 0, 0), 2)
#   expect_equal(dev_gamma_pois_zi(0, 1, 0, 0.5), 0.759770986083445)
#   expect_equal(dev_gamma_pois_zi(0, 1, 0, 0.99), 0.0126825380059972)
#   expect_identical(dev_gamma_pois_zi(0, 1, 0, 1), 0)
#   expect_identical(dev_gamma_pois_zi(1, 1, 1, 0), 0)
#   expect_identical(dev_gamma_pois_zi(1, 1, 1, 0.5), 0)
#   expect_identical(dev_gamma_pois_zi(1, 1, 1, 0.99), 0)
#   expect_equal(dev_gamma_pois_zi(0, 1, 1, 0.99), 0.0100250836470886)
#   expect_equal(dev_gamma_pois_zi(0, 1, 0.5, 0.99), 0.0111420900989107)
#   expect_identical(dev_gamma_pois_zi(1, 1, 0, 1), 0)
#   expect_identical(dev_gamma_pois_zi(0, 1, 1, 1), 0)
#   expect_equal(dev_gamma_pois_zi(0, 1, 0, 0), 2)
#   expect_equal(dev_gamma_pois_zi(0, 1, 1, 0), 1.38629436111989)
#   expect_identical(dev_gamma_pois(0, 0, 0, 0), 0)
#   expect_identical(dev_gamma_pois(0, 0, 1, 0), 0)
#   expect_identical(dev_gamma_pois(0, 0, 0, 0.5), 0)
#   expect_identical(dev_gamma_pois(0, 0, 1, 0.5), 0)
#   expect_identical(dev_gamma_pois(0, 0, 0, 1), 0)
#   expect_identical(dev_gamma_pois(0, 0, 1, 1), 0)
#
#   expect_identical(dev_gamma_pois_zi(NA, 1, 1, 0), NA_real_)
#   expect_identical(dev_gamma_pois_zi(1, NA, 1, 0), NA_real_)
#   expect_identical(dev_gamma_pois_zi(1, 1, NA, 0), NA_real_)
#   expect_identical(dev_gamma_pois_zi(1, 1, 1, NA), NA_real_)
#   expect_equal(dev_gamma_pois_zi(1, 3, 1, 0), dev_gamma_pois_zi(1, 3, 1, 0, res = TRUE)^2)
#   expect_equal(dev_gamma_pois_zi(1, 3, 1, 0.5), dev_gamma_pois_zi(1, 3, 1, 0.5, res = TRUE)^2)
#
#   expect_equal(dev_gamma_pois_zi(c(0, 1, 3.5), 3, 0, 0, res = TRUE),
#                c(-2.44948974278318, -1.34267472705186, 0.281166781094084))
#   expect_equal(dev_gamma_pois_zi(c(0, 1, 3.5), 3, 0, 0.5, res = TRUE),
#                c(-1.13539405405014, -1.34267472705186, 0.281166781094084))
#   expect_equal(dev_gamma_pois_zi(c(1,3,4), c(1, 3.5, 4), 0, 0, res = TRUE),
#                c(0, -0.274036349845144, 0))
#
#   expect_equal(dev_gamma_pois_zi(3, 3.5, res = TRUE),
#                dev_pois(3, 3.5, res = TRUE))
#   expect_equal(dev_gamma_pois_zi(3, 3.5, 0, 0.1, res = TRUE),
#                dev_pois_zi(3, 3.5, 0.1, res = TRUE))
#   expect_equal(dev_gamma_pois_zi(3, 3.5, 0, 0.2, res = TRUE),
#                dev_pois_zi(3, 3.5, 0.2, res = TRUE))
# })

test_that("dev_lnorm", {
  expect_equal(dev_lnorm(3,4,5),
               2 * (log_lik_lnorm(3, log(3), 5) - log_lik_lnorm(3, 4, 5)))

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
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 2, res = TRUE), dev_norm(-2:2, res = TRUE)/2)
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 1/2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_lnorm(exp(-2:2), meanlog = -2:2), rep(0, 5))
  expect_equal(dev_lnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5, res = TRUE),
               c(-1, -0.5, -0.333333333333333, -0.25, -0.2))
})

test_that("deviance lnorm", {
  samples <- ran_lnorm(100)
  mod <- lm(log(samples)~1)
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

  expect_equal(dev_neg_binom(c(1, 2, 5), 4, 1/2, res = TRUE),
               c(-1.177410022515, -0.686390663271, 0.270787731555))
  expect_equal(dev_neg_binom(c(1, 2, 5), c(1, 3, 5), 1/2, res = TRUE),
               c(0, -0.404089071964, 0))
})


test_that("dev_norm", {
  expect_equal(dev_norm(3,4,5),
               2 * (log_lik_norm(3, 3, 5) - log_lik_norm(3, 4, 5)))
  expect_identical(dev_norm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_norm(0), 0)
  expect_identical(dev_norm(NA, 1, 1), NA_real_)
  expect_identical(dev_norm(1, NA, 1), NA_real_)
  expect_identical(dev_norm(1, 1, NA), NA_real_)
  expect_equal(dev_norm(-2), dev_norm(-2, res = TRUE)^2)
  expect_equal(dev_norm(-2:2, res = TRUE), c(-2, -1, 0, 1, 2))
  expect_equal(dev_norm(-2:2, sd = 2, res = TRUE), dev_norm(-2:2, res = TRUE)/2)
  expect_equal(dev_norm(-2:2, sd = 1/2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_norm(-2:2, mean = -2:2, res = TRUE), rep(0, 5))
  expect_equal(dev_norm(-2:2, mean = -1:3, sd = 1:5, res = TRUE),
               c(-1, -0.5, -0.333333333333333, -0.25, -0.2))
})

test_that("deviance norm", {
  samples <- ran_norm(100)
  mod <- lm(samples~1)
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
  expect_equal(dev_pois(c(NA,1), 0:1), c(NA,0))
  expect_equal(dev_pois(c(0,NA), 0:1), c(0,NA))
  expect_equal(dev_pois(c(0:1), c(NA,1)), c(NA,0))
  expect_equal(dev_pois(c(0:1), c(0,NA)), c(0,NA))
})

test_that("pois res", {
  expect_equal(dev_pois(0, 0.5), dev_pois(0, 0.5, res = TRUE)^2)
  expect_equal(dev_pois(0:1, c(0.3,0.6)), dev_pois(0:1, c(0.3,0.6), res = TRUE)^2)
})

test_that("pois log_lik", {
  expect_equal(dev_pois(0:1, 0.5),
               2 * (log_lik_pois(0:1, 0:1) - log_lik_pois(0:1, 0.5)))
  expect_equal(dev_pois(0:1, 0.7),
               2 * (log_lik_pois(0:1, 0:1) - log_lik_pois(0:1, 0.7)))
})

test_that("pois deviance", {
  samples <- ran_pois(1000)
  mod <- glm(samples~1, family = poisson)
  deviance <- sum(dev_pois(samples, exp(coef(mod)[1])))
  expect_equal(deviance, deviance(mod))
})

test_that("pois ran", {
  set.seed(101)
  samples <- ran_pois(1000)
  expect_equal(mean(samples), 0.984)
  expect_equal(var(samples), 1.02476876876877)
  res <- dev_pois(samples, 1, res = TRUE)
  expect_equal(mean(res),-0.235540962010425)
  expect_equal(sd(res),1.05758754072283)
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
})

test_that("pois_zi vectorized", {
  expect_equal(dev_pois_zi(0:3, 2, 0), c(4, 0.613705638880109, 0, 0.432790648648986))
  expect_equal(dev_pois_zi(0:3, 0:3, c(0, 0.1, 0.5, 1)), c(0, 0.210721031315653, 1.38629436111989, Inf))
  expect_equal(dev_pois_zi(0:3, 3:0, c(0, 0.1, 0.5, 1)), c(6, 0.824426670195762, 2.15888308335967, Inf))
})

test_that("pois_zi vectorized missing values", {
  expect_equal(dev_pois_zi(c(NA,1), 0:1, 0:1), c(NA,Inf))
  expect_equal(dev_pois_zi(c(0,NA), 0:1, 0:1), c(0,NA))
  expect_equal(dev_pois_zi(c(0:1), c(NA,1), 0:1), c(NA,Inf))
  expect_equal(dev_pois_zi(c(0:1), c(0,NA), 0:1), c(0,NA))
  expect_equal(dev_pois_zi(c(0:1), 0:1, c(NA,1)), c(NA,Inf))
  expect_equal(dev_pois_zi(c(0:1), 0:1, c(1,NA)), c(0,NA))
})

test_that("pois_zi res", {
  expect_equal(dev_pois_zi(0, 0.5, 0.5), dev_pois_zi(0, 0.5, 0.5, res = TRUE)^2)
  expect_equal(dev_pois_zi(0:1, c(0.3,0.6), 0.5), dev_pois_zi(0:1, c(0.3,0.6), 0.5, res = TRUE)^2)
})

test_that("pois_zi log_lik", {
  expect_equal(dev_pois_zi(0, 0.5),
               2 * (log_lik_pois_zi(0, 0) - log_lik_pois_zi(0, 0.5)))
  expect_equal(dev_pois_zi(0, 0.5, 0.5),
               2 * (log_lik_pois_zi(0, 0, 0.5) - log_lik_pois_zi(0, 0.5, 0.5)))
  expect_equal(dev_pois_zi(1, 0.5),
               2 * (log_lik_pois_zi(1, 1) - log_lik_pois_zi(1, 0.5)))
  expect_equal(dev_pois_zi(1, 0.5, 0.5),
               2 * (log_lik_pois_zi(1, 1, 0) - log_lik_pois_zi(1, 0.5, 0.5)))
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


# test_that("dev_pois_zi", {
#   expect_equal(dev_pois_zi(1,2),
#                2 * (log_lik_pois_zi(1, 1) - log_lik_pois_zi(1, 2)))
#   expect_equal(dev_pois_zi(3,2),
#                2 * (log_lik_pois_zi(3, 3) - log_lik_pois_zi(3, 2)))
#   expect_equal(dev_pois_zi(0,2),
#                2 * (log_lik_pois_zi(0, 0) - log_lik_pois_zi(0, 2)))
#   expect_equal(dev_pois_zi(0,2,1),
#                2 * (log_lik_pois_zi(0, 0, 1) - log_lik_pois_zi(0, 2, 1)))
#   expect_equal(dev_pois_zi(0,2,0.5),
#                2 * (log_lik_pois_zi(0, 0, 0.5) - log_lik_pois_zi(0, 2, 0.5)))
#   expect_equal(dev_pois_zi(0,0),
#                2 * (log_lik_pois_zi(0, 0) - log_lik_pois_zi(0, 0)))
#   expect_equal(dev_pois_zi(1,0,1),
#                2 * (log_lik_pois_zi(0, 0, 1) - log_lik_pois_zi(1, 0, 1)))
#   expect_identical(dev_pois_zi(integer(0), integer(0), numeric(0)), numeric(0))
#   expect_identical(dev_pois_zi(1, 1), 0)
#   expect_identical(dev_pois_zi(0, 0), 0)
#   expect_identical(dev_pois_zi(0, 1), 2)
#   expect_identical(dev_pois_zi(NA, 1), NA_real_)
#   expect_identical(dev_pois_zi(1, NA), NA_real_)
#   expect_equal(dev_pois_zi(1, 3), dev_pois_zi(1, 3, res = TRUE)^2)
#   expect_equal(dev_pois_zi(c(1,3.5,4), 3, res = TRUE),
#                c(-1.34267472705186, 0.281166781094084, 0.549050616623137))
#   expect_equal(dev_pois_zi(c(1,3,4), c(1, 3.5, 4), res = TRUE),
#                c(0, -0.274036349845144, 0))
#   expect_equal(dev_pois_zi(c(1,3,4), c(1, 3.5, 4), 0.5, res = TRUE),
#                c(0, 0.274036349845144, 0))
#
#   expect_equal(dev_pois_zi(3, 3.5, 0, res = TRUE),
#                dev_pois(3, 3.5, res = TRUE))
#   expect_equal(dev_pois_zi(3,3.5,0.1),
#                2 * (log_lik_pois_zi(3,3,0.1) - log_lik_pois_zi(3,3.5,0.1)))
#   expect_equal(dev_pois_zi(3,3.5,0.2),
#                2 * (log_lik_pois_zi(3,3,0.2) - log_lik_pois_zi(3,3.5,0.2)))
#   expect_equal(dev_pois_zi(3, 3, 0, res = TRUE),
#                0)
#   expect_equal(dev_pois_zi(3, 3, 1, res = TRUE),
#                0)
#   expect_equal(dev_pois(3, 3.5, res = TRUE),
#                -0.274036349845144)
#   expect_equal(dev_pois_zi(3, 3.5, 0, res = TRUE),
#                -0.274036349845144)
#   expect_equal(sign(3-3.5)*sqrt(dev_pois(3, 3.5)),
#                -0.274036349845144)
#
#   skip("why not changing and becoming positive??")
#   expect_equal(dev_pois_zi(3, 3.5, 0.1, res = TRUE),
#                -0.274036349845144)
#   expect_equal(dev_pois_zi(3, 3.5, 0.2, res = TRUE),
#                0.274036349845144)
#   expect_equal(dev_pois_zi(3, 3.5, 0.90, res = TRUE),
#                0.274036349845144)
# })
