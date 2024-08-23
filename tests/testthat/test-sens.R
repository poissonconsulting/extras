# Normal ----
test_that("sens_norm returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_norm(mean = 2, sd = 10, sd_mult = 2))
})

test_that("sens_norm errors with NULL input for mean", {
  expect_chk_error(
    sens_norm(mean = NULL, sd = 10, sd_mult = 2)
  )
})

test_that("sens_norm errors with NULL input for sd", {
  expect_chk_error(
    sens_norm(mean = 0, sd = NULL, sd_mult = 2)
  )
})

test_that("sens_norm errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_norm(mean = 10, sd = 10, sd_mult = NULL)
  )
})

test_that("sens_norm errors with empty numeric input for mean", {
  expect_chk_error(
    sens_norm(mean = numeric(0), sd = 10, sd_mult = 2)
  )
})

test_that("sens_norm errors with empty numeric input for sd", {
  expect_chk_error(
    sens_norm(mean = 0, sd = numeric(0), sd_mult = 2)
  )
})

test_that("sens_norm errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_norm(mean = 10, sd = 10, sd_mult = numeric(0))
  )
})

test_that("sens_norm errors when provided with a vector of values for mean", {
  expect_chk_error(
    sens_norm(mean = 10:13, sd = 10, sd_mult = 2),
    "`mean` must be a number"
  )
})

test_that("sens_norm errors when provided with a vector of values for sd", {
  expect_chk_error(
    sens_norm(mean = 10, sd = 10:13, sd_mult = 2)
  )
})

test_that("sens_norm errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_norm(mean = 10, sd = 10, sd_mult = 2:3)
  )
})

test_that("normal sensitivity expands sd by multiplier", {
  mean <- 10
  sd <- 10
  sd_mult <- 2
  new_pars <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_norm(10000, mean, sd)
      ran_new <- ran_norm(10000, new_pars$mean, new_pars$sd)
      expect_equal(sd(ran_new) / sd(ran_original), 1.9998362953948)
    }
  )
})

test_that("normal sensitivity reduces sd by multiplier", {
  mean <- 10
  sd <- 10
  sd_mult <- 0.6
  new_pars <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_norm(10000, mean, sd)
      ran_new <- ran_norm(10000, new_pars$mean, new_pars$sd)
      expect_equal(sd(ran_new) / sd(ran_original), 0.59995088861844)
    }
  )
})

test_that("normal sensitivity keeps the same mean", {
  mean <- 10
  sd <- 18
  sd_mult <- 0.6
  new_pars <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_norm(10000, mean, sd)
      ran_new <- ran_norm(10000, new_pars$mean, new_pars$sd)
      expect_equal(mean(ran_new) - mean(ran_original), -0.0516658531583953)
    }
  )
})

test_that("normal sensitivity errors with a character mean", {
  mean <- "10"
  sd <- 18
  sd_mult <- 0.6
  expect_chk_error(
    sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  )
})

test_that("normal sensitivity errors with a logical SD", {
  mean <- 10
  sd <- TRUE
  sd_mult <- 0.6
  expect_chk_error(
    sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  )
})

test_that("normal sensitivity errors with a sd < 0", {
  mean <- 10
  sd <- -1
  sd_mult <- 0.6
  expect_chk_error(
    sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  )
})

test_that("normal sensitivity errors with sd_mult < 0", {
  mean <- 10
  sd <- 1
  sd_mult <- -1
  expect_chk_error(
    sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  )
})

test_that("normal sensitivity errors with a character sd_mult", {
  mean <- 10
  sd <- 1
  sd_mult <- "ten"
  expect_chk_error(
    sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  )
})

test_that("normal random deviates have the expected mean", {
  mean <- 10
  sd <- 20
  sd_mult <- 2
  new_pars <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  withr::with_seed(
    seed = 101,
    {
      ran <- ran_norm(10000, mean = new_pars$mean, sd = new_pars$sd)
      expect_equal(mean(ran), 10.2111157132372)
    }
  )
})

test_that("normal random deviates have the expected sd", {
  mean <- 10
  sd <- 20
  sd_mult <- 2
  new_pars <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  withr::with_seed(
    seed = 101,
    {
      ran <- ran_norm(10000, mean = new_pars$mean, sd = new_pars$sd)
      expect_equal(sd(ran), 39.726965186315)
    }
  )
})

# Student's t ----
test_that("sens_student returns a numeric vector of length 3 with correct names", {
  expect_snapshot(sens_student(mean = 2, sd = 10, theta = 0.1, sd_mult = 2))
})

test_that("sens_student errors with NULL input for mean", {
  expect_chk_error(
    sens_student(mean = NULL, sd = 10, theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors with NULL input for sd", {
  expect_chk_error(
    sens_student(mean = 0, sd = NULL, theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors with NULL input for theta", {
  expect_chk_error(
    sens_student(mean = 0, sd = 0.3, theta = NULL, sd_mult = 2)
  )
})

test_that("sens_student errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_student(mean = 10, sd = 10, theta = 0.1, sd_mult = NULL)
  )
})

test_that("sens_student errors with empty numeric input for mean", {
  expect_chk_error(
    sens_student(mean = numeric(0), sd = 10, theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors with empty numeric input for sd", {
  expect_chk_error(
    sens_student(mean = 0, sd = numeric(0), theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors with empty numeric input for theta", {
  expect_chk_error(
    sens_student(mean = 0, sd = 12, theta = numeric(0), sd_mult = 2)
  )
})

test_that("sens_student errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_student(mean = 10, sd = 10, theta = 0.1, sd_mult = numeric(0))
  )
})

test_that("sens_student errors when provided with a vector of values for mean", {
  expect_chk_error(
    sens_student(mean = 10:13, sd = 10, theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors when provided with a vector of values for sd", {
  expect_chk_error(
    sens_student(mean = 10, sd = 10:13, theta = 0.1, sd_mult = 2)
  )
})

test_that("sens_student errors when provided with a vector of values for theta", {
  expect_chk_error(
    sens_student(mean = 10, sd = 10, theta = 1:2, sd_mult = 2)
  )
})

test_that("sens_student errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_student(mean = 10, sd = 10, theta = 0.1, sd_mult = 2:3)
  )
})

test_that("student's sensitivity expands the sd by the multiplier", {
  mean <- 10
  sd <- 5
  theta <- 0.1
  sd_mult <- 5
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars$sd) / sd, sd_mult)
})

test_that("student's sensitivity reduces the sd by the multiplier", {
  mean <- 10
  sd <- 0.6
  theta <- 0.1
  sd_mult <- 5
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars$sd) / sd, sd_mult)
})

test_that("student's sensitivity keeps the same mean", {
  mean <- 10
  sd <- 0.6
  theta <- 0.1
  sd_mult <- 5
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars$mean), mean)
})

test_that("student's sensitivity has the same results as the normal with theta = 0", {
  mean <- 10
  sd <- 5
  theta <- 0
  sd_mult <- 5
  new_pars_student <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  new_pars_norm <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  expect_equal(new_pars_student$mean, new_pars_norm$mean)
  expect_equal(new_pars_student$sd, new_pars_norm$sd)
})

test_that("student's sensitivity errors with character mean", {
  mean <- "ten"
  sd <- 0.6
  theta <- 0.1
  sd_mult <- 5
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity errors with logical sd", {
  mean <- 10
  sd <- FALSE
  theta <- 0.1
  sd_mult <- 5
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity errors with sd < 0", {
  mean <- 10
  sd <- -1
  theta <- 0.1
  sd_mult <- 5
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity errors with character theta", {
  mean <- 10
  sd <- 10
  theta <- "theta"
  sd_mult <- 5
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity errors with theta < 0", {
  mean <- 10
  sd <- 10
  theta <- -1
  sd_mult <- 5
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity expands sd by multiplier", {
  mean <- 10
  sd <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_student(10000, mean, sd, theta)
      ran_new <- ran_student(10000, new_pars$mean, new_pars$sd, new_pars$theta)
      expect_equal(sd(ran_new) / sd(ran_original), 1.96503686143518)
    }
  )
})

test_that("student's sensitivity expands sd by multiplier", {
  mean <- 10
  sd <- 10
  theta <- 0.1
  sd_mult <- 0.6
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_student(10000, mean, sd, theta)
      ran_new <- ran_student(10000, new_pars$mean, new_pars$sd, new_pars$theta)
      expect_equal(sd(ran_new) / sd(ran_original), 0.589511058430551)
    }
  )
})

test_that("student's sensitivity keeps the same mean", {
  mean <- 10
  sd <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_student(10000, mean, sd, theta)
      ran_new <- ran_student(10000, new_pars$mean, new_pars$sd, new_pars$theta)
      expect_equal(mean(ran_new) - mean(ran_original), -0.182266562671183)
    }
  )
})

test_that("student's sensitivity works with large theta", {
  mean <- 10
  sd <- 10
  theta <- 10
  sd_mult <- 5
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars[1]), mean)
  expect_equal(as.numeric(new_pars[2]), (sd * sd_mult))
  expect_equal(as.numeric(new_pars[3]), theta)
})

test_that("student's sensitivity works with small theta", {
  mean <- 10
  sd <- 10
  theta <- 0.01
  sd_mult <- 5
  new_pars <- sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars[1]), mean)
  expect_equal(as.numeric(new_pars[2]), (sd * sd_mult))
  expect_equal(as.numeric(new_pars[3]), theta)
})

test_that("student's sensitivity errors with character sd_mult", {
  mean <- 10
  sd <- 1
  theta <- 0.1
  sd_mult <- "five"
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

test_that("student's sensitivity errors with sd_mult = 0", {
  mean <- 10
  sd <- 0.1
  theta <- 0.1
  sd_mult <- 0
  expect_chk_error(
    sens_student(mean = mean, sd = sd, theta = theta, sd_mult = sd_mult)
  )
})

# Exponential ----
test_that("sens_exp returns a numeric vector of length 1 with correct names", {
  expect_snapshot(sens_exp(rate = 2, sd_mult = 2))
})

test_that("sens_exp errors with NULL input for rate", {
  expect_chk_error(
    sens_exp(rate = NULL, sd_mult = 2)
  )
})

test_that("sens_exp errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_exp(rate = 10, sd_mult = NULL)
  )
})

test_that("sens_exp errors with empty numeric input for rate", {
  expect_chk_error(
    sens_exp(rate = numeric(0), sd_mult = 2)
  )
})

test_that("sens_exp errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_exp(rate = 10, sd_mult = numeric(0))
  )
})

test_that("sens_exp errors when provided with a vector of values for rate", {
  expect_chk_error(
    sens_exp(rate = 10:13, sd_mult = 2)
  )
})

test_that("sens_exp errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_exp(rate = 10, sd_mult = 2:3)
  )
})

test_that("sd of exponential deviates expands as expected", {
  rate <- 10
  sd_mult <- 5
  new_pars <- sens_exp(rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rexp(10000, rate)
      ran_new <- rexp(10000, new_pars$rate)
      expect_equal(sd(ran_new) / sd(ran_original), 4.95814995228506)
    }
  )
})

test_that("sd of exponential deviates decreases as expected", {
  rate <- 10
  sd_mult <- 0.5
  new_pars <- sens_exp(rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rexp(10000, rate)
      ran_new <- rexp(10000, new_pars$rate)
      expect_equal(sd(ran_new) / sd(ran_original), 0.495814995228506)
    }
  )
})

test_that("mean of exponential deviates shifts as expected", {
  rate <- 1
  sd_mult <- 0.5
  new_pars <- sens_exp(rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rexp(10000, rate)
      ran_new <- rexp(10000, new_pars$rate)
      expect_equal(mean(ran_new) - mean(ran_original), -0.506655908545402)
    }
  )
})

test_that("sens_exp errors with character rate", {
  rate <- "ten"
  sd_mult <- 2
  expect_chk_error(
    sens_exp(rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_exp errors with negative rate", {
  rate <- -1
  sd_mult <- 2
  expect_chk_error(
    sens_exp(rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_exp errors with character sd_mult", {
  rate <- 18
  sd_mult <- "2"
  expect_chk_error(
    sens_exp(rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_exp errors with negative sd_mult", {
  rate <- 10
  sd_mult <- -10
  expect_chk_error(
    sens_exp(rate = rate, sd_mult = sd_mult)
  )
})

# skew normal ----
test_that("sens_skewnorm returns a numeric vector of length 3 with correct names", {
  expect_snapshot(sens_skewnorm(mean = 2, sd = 10, shape = -1, sd_mult = 2))
})

test_that("sens_skewnorm errors with NULL input for mean", {
  expect_chk_error(
    sens_skewnorm(mean = NULL, sd = 10, shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with NULL input for sd", {
  expect_chk_error(
    sens_skewnorm(mean = 0, sd = NULL, shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with NULL input for shape", {
  expect_chk_error(
    sens_skewnorm(mean = 0, sd = 0.3, shape = NULL, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_skewnorm(mean = 10, sd = 10, shape = 0.1, sd_mult = NULL)
  )
})

test_that("sens_skewnorm errors with empty numeric input for mean", {
  expect_chk_error(
    sens_skewnorm(mean = numeric(0), sd = 10, shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with empty numeric input for sd", {
  expect_chk_error(
    sens_skewnorm(mean = 0, sd = numeric(0), shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with empty numeric input for shape", {
  expect_chk_error(
    sens_skewnorm(mean = 0, sd = 12, shape = numeric(0), sd_mult = 2)
  )
})

test_that("sens_skewnorm errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_skewnorm(mean = 10, sd = 10, shape = 0.1, sd_mult = numeric(0))
  )
})

test_that("sens_skewnorm errors when provided with a vector of values for mean", {
  expect_chk_error(
    sens_skewnorm(mean = 10:13, sd = 10, shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors when provided with a vector of values for sd", {
  expect_chk_error(
    sens_skewnorm(mean = 10, sd = 10:13, shape = 0.1, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors when provided with a vector of values for shape", {
  expect_chk_error(
    sens_skewnorm(mean = 10, sd = 10, shape = 1:2, sd_mult = 2)
  )
})

test_that("sens_skewnorm errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_skewnorm(mean = 10, sd = 10, shape = 0.1, sd_mult = 2:3)
  )
})

test_that("sd of skewnorm deviates expands as expected", {
  mean <- 10
  sd <- 20
  shape <- -1
  sd_mult <- 5
  new_pars <- sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_skewnorm(10000, mean, sd, shape)
      ran_new <- ran_skewnorm(10000, new_pars$mean, new_pars$sd, new_pars$shape)
      expect_equal(sd(ran_new) / sd(ran_original), 4.98784228891484)
    }
  )
})

test_that("sd of skewnorm deviates decreases as expected", {
  mean <- 10
  sd <- 20
  shape <- 2
  sd_mult <- 0.4
  new_pars <- sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_skewnorm(10000, mean, sd, shape)
      ran_new <- ran_skewnorm(10000, new_pars$mean, new_pars$sd, new_pars$shape)
      expect_equal(sd(ran_new) / sd(ran_original), 0.399731342111579)
    }
  )
})

test_that("mean of skewnorm deviates changes expectedly with expanding sd", {
  mean <- 100
  sd <- 20
  shape <- 2
  sd_mult <- 2
  new_pars <- sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_skewnorm(10000, mean, sd, shape)
      ran_new <- ran_skewnorm(10000, new_pars$mean, new_pars$sd, new_pars$shape)
      expect_equal(mean(ran_new) - mean(ran_original), -0.169525559258474)
    }
  )
})

test_that("mean of skewnorm deviates changes expectedly with reducing sd", {
  mean <- 100
  sd <- 20
  shape <- 2
  sd_mult <- 0.4
  new_pars <- sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_skewnorm(10000, mean, sd, shape)
      ran_new <- ran_skewnorm(10000, new_pars$mean, new_pars$sd, new_pars$shape)
      expect_equal(mean(ran_new) - mean(ran_original), -0.0428204928375919)
    }
  )
})

test_that("sens_skewnorm errors with character mean", {
  mean <- "100"
  sd <- 20
  shape <- 2
  sd_mult <- 0.4
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  )
})

test_that("sens_skewnorm errors with character sd", {
  mean <- 100
  sd <- "twenty"
  shape <- 2
  sd_mult <- 0.4
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  )
})


test_that("sens_skewnorm errors when sd < 0", {
  mean <- 100
  sd <- -1
  shape <- 5
  sd_mult <- 0.4
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  )
})

test_that("sens_skewnorm errors with character shape", {
  mean <- 100
  sd <- 10
  shape <- "five"
  sd_mult <- 0.4
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult),
    "`shape` must be a number"
  )
})

test_that("sens_skewnorm errors with character sd_mult", {
  mean <- 100
  sd <- 10
  shape <- 5
  sd_mult <- "ten"
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  )
})

test_that("sens_skewnorm errors when sd_mult < 0", {
  mean <- 100
  sd <- 1
  shape <- 5
  sd_mult <- -2
  expect_chk_error(
    sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  )
})

test_that("sens_skewnorm has same behaviour as normal when shape = 0", {
  mean <- 100
  sd <- 10
  shape <- 0
  sd_mult <- 2
  new_pars_skewnorm <- sens_skewnorm(mean = mean, sd = sd, shape = shape, sd_mult = sd_mult)
  new_pars_norm <- sens_norm(mean = mean, sd = sd, sd_mult = sd_mult)
  expect_equal(new_pars_skewnorm$mean, new_pars_norm$mean)
  expect_equal(new_pars_skewnorm$sd, new_pars_norm$sd)
})

# Log-normal ----
test_that("sens_lnorm returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_lnorm(meanlog = 2, sdlog = 10, sd_mult = 2))
})

test_that("sens_lnorm errors with NULL input for meanlog", {
  expect_chk_error(
    sens_lnorm(meanlog = NULL, sdlog = 10, sd_mult = 2)
  )
})

test_that("sens_lnorm errors with NULL input for sdlog", {
  expect_chk_error(
    sens_lnorm(meanlog = 0, sdlog = NULL, sd_mult = 2)
  )
})

test_that("sens_lnorm errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_lnorm(meanlog = 10, sdlog = 10, sd_mult = NULL)
  )
})

test_that("sens_lnorm errors with empty numeric input for meanlog", {
  expect_chk_error(
    sens_lnorm(meanlog = numeric(0), sdlog = 10, sd_mult = 2)
  )
})

test_that("sens_lnorm errors with empty numeric input for sdlog", {
  expect_chk_error(
    sens_lnorm(meanlog = 0, sdlog = numeric(0), sd_mult = 2)
  )
})

test_that("sens_lnorm errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_lnorm(meanlog = 10, sdlog = 10, sd_mult = numeric(0))
  )
})

test_that("sens_lnorm errors when provided with a vector of values for meanlog", {
  expect_chk_error(
    sens_lnorm(meanlog = 10:13, sdlog = 10, sd_mult = 2)
  )
})

test_that("sens_lnorm errors when provided with a vector of values for sdlog", {
  expect_chk_error(
    sens_lnorm(meanlog = 10, sdlog = 10:13, sd_mult = 2)
  )
})

test_that("sens_lnorm errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_lnorm(meanlog = 10, sdlog = 10, sd_mult = 2:3)
  )
})

test_that("sd of log-normal deviates expands as expected", {
  meanlog <- 0
  sdlog <- 1
  sd_mult <- 2
  new_pars <- sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_lnorm(10000, meanlog, sdlog)
      ran_new <- ran_lnorm(10000, new_pars$meanlog, new_pars$sdlog)
      expect_equal(sd(ran_new) / sd(ran_original), 2.48484604109396)
    }
  )
})

test_that("sd of log-normal deviates decreases as expected", {
  meanlog <- 0
  sdlog <- 1
  shape <- 2
  sd_mult <- 0.4
  new_pars <- sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_lnorm(10000, meanlog, sdlog)
      ran_new <- ran_lnorm(10000, new_pars$meanlog, new_pars$sdlog)
      expect_equal(sd(ran_new) / sd(ran_original), 0.413093471380755)
    }
  )
})

test_that("mean of log-normal deviates stays approximately the same with expanding sd", {
  meanlog <- 0
  sdlog <- 1
  sd_mult <- 2
  new_pars <- sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  withr::with_seed(
    103,
    {
      ran_original <- ran_lnorm(10000, meanlog, sdlog)
      ran_new <- ran_lnorm(10000, new_pars$meanlog, new_pars$sdlog)
      expect_equal(mean(ran_new) - mean(ran_original), 0.0742085317541252)
    }
  )
})

test_that("mean of log-normal deviates stays approximately the same with reducing sd", {
  meanlog <- 0
  sdlog <- 1
  sd_mult <- 0.2
  new_pars <- sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  withr::with_seed(
    103,
    {
      ran_original <- ran_lnorm(10000, meanlog, sdlog)
      ran_new <- ran_lnorm(10000, new_pars$meanlog, new_pars$sdlog)
      expect_equal(mean(ran_new) - mean(ran_original), 0.0347894804650233)
    }
  )
})

test_that("sens_lnorm errors with character meanlog", {
  meanlog <- "zero"
  sdlog <- 1
  sd_mult <- 2
  expect_chk_error(
    sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  )
})

test_that("sens_lnorm errors with character sdlog", {
  meanlog <- 0
  sdlog <- "one"
  sd_mult <- 2
  expect_chk_error(
    sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  )
})

test_that("sens_lnorm errors with sdlog < 0", {
  meanlog <- 0
  sdlog <- -1
  sd_mult <- 2
  expect_chk_error(
    sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  )
})

test_that("sens_lnorm errors with character sd_mult", {
  meanlog <- 0
  sdlog <- 1
  sd_mult <- "two"
  expect_chk_error(
    sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  )
})

test_that("sens_lnorm errors with sd_mult = 0", {
  meanlog <- 0
  sdlog <- 1
  sd_mult <- 0
  expect_chk_error(
    sens_lnorm(meanlog = meanlog, sdlog = sdlog, sd_mult = sd_mult)
  )
})

# Beta ----
test_that("sens_beta returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_beta(alpha = 2, beta = 10, sd_mult = 2))
})

test_that("sens_beta errors with NULL input for alpha", {
  expect_chk_error(
    sens_beta(alpha = NULL, beta = 10, sd_mult = 2)
  )
})

test_that("sens_beta errors with NULL input for beta", {
  expect_chk_error(
    sens_beta(alpha = 1, beta = NULL, sd_mult = 2)
  )
})

test_that("sens_beta errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_beta(alpha = 10, beta = 10, sd_mult = NULL)
  )
})

test_that("sens_beta errors with empty numeric input for alpha", {
  expect_chk_error(
    sens_beta(alpha = numeric(0), beta = 10, sd_mult = 2)
  )
})

test_that("sens_beta errors with empty numeric input for beta", {
  expect_chk_error(
    sens_beta(alpha = 0.1, beta = numeric(0), sd_mult = 2)
  )
})

test_that("sens_beta errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_beta(alpha = 10, beta = 10, sd_mult = numeric(0))
  )
})

test_that("sens_beta errors when provided with a vector of values for alpha", {
  expect_chk_error(
    sens_beta(alpha = 10:13, beta = 10, sd_mult = 2)
  )
})

test_that("sens_beta errors when provided with a vector of values for beta", {
  expect_chk_error(
    sens_beta(alpha = 10, beta = 10:13, sd_mult = 2)
  )
})

test_that("sens_beta errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_beta(alpha = 10, beta = 10, sd_mult = 2:3)
  )
})

test_that("sd of beta deviates expands as expected (works for large alpha and beta)", {
  alpha <- 50
  beta <- 50
  sd_mult <- 2
  new_pars <- sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rbeta(10000, alpha, beta)
      ran_new <- rbeta(10000, new_pars$alpha, new_pars$beta)
      expect_equal(sd(ran_new) / sd(ran_original), 1.99408427740544)
    }
  )
})

test_that("sens_beta expands sd only until the uniform distribution for smaller alpha and beta", {
  alpha <- 2
  beta <- 3
  sd_mult <- 2
  new_pars <- sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  expect_equal(as.numeric(new_pars[1]), 1)
  expect_equal(as.numeric(new_pars[2]), 1)
})

test_that("sd of beta deviates decreases as expected", {
  alpha <- 2
  beta <- 3
  sd_mult <- 0.5
  new_pars <- sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rbeta(10000, alpha, beta)
      ran_new <- rbeta(10000, new_pars$alpha, new_pars$beta)
      expect_equal(sd(ran_new) / sd(ran_original), 0.500171061858751)
    }
  )
})

test_that("sens_beta errors with character alpha", {
  alpha <- "two"
  beta <- 3
  sd_mult <- 0.5
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

test_that("sens_beta errors with alpha < 0", {
  alpha <- -1
  beta <- 3
  sd_mult <- 0.5
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

test_that("sens_beta errors with character beta", {
  alpha <- 2
  beta <- "three"
  sd_mult <- 0.5
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

test_that("sens_beta errors with beta = 0", {
  alpha <- 2
  beta <- 0
  sd_mult <- 0.5
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

test_that("sens_beta errors with character sd_mult", {
  alpha <- 2
  beta <- 3
  sd_mult <- "ten"
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

test_that("sens_beta errors with sd_mult < 0", {
  alpha <- 2
  beta <- 1
  sd_mult <- -10
  expect_chk_error(
    sens_beta(alpha = alpha, beta = beta, sd_mult = sd_mult)
  )
})

# Poisson ----
test_that("sens_pois returns a numeric vector of length 1 with correct names", {
  expect_snapshot(sens_pois(lambda = 10, sd_mult = 2))
})

test_that("sens_pois errors with NULL input for lambda", {
  expect_chk_error(
    sens_pois(lambda = NULL, sd_mult = 2)
  )
})

test_that("sens_pois errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_pois(lambda = 10, sd_mult = NULL)
  )
})

test_that("sens_pois errors with empty numeric input for lambda", {
  expect_chk_error(
    sens_pois(lambda = numeric(0), sd_mult = 2)
  )
})

test_that("sens_pois errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_pois(lambda = 10, sd_mult = numeric(0))
  )
})

test_that("sens_pois errors when provided with a vector of values for lambda", {
  expect_chk_error(
    sens_pois(lambda = 10:13, sd_mult = 2)
  )
})

test_that("sens_pois errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_pois(lambda = 10, sd_mult = 2:3)
  )
})

test_that("sd of poisson random deviates expands with sd_mult > 1", {
  lambda <- 10
  sd_mult <- 2
  new_pars <- sens_pois(lambda = lambda, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rpois(10000, lambda)
      ran_new <- rpois(10000, new_pars$lambda)
      expect_equal(sd(ran_new) / sd(ran_original), 2.01596189810105)
    }
  )
})

test_that("sd of poisson random deviates decreases with sd_mult < 1", {
  lambda <- 10
  sd_mult <- 0.5
  new_pars <- sens_pois(lambda = lambda, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rpois(10000, lambda)
      ran_new <- rpois(10000, new_pars$lambda)
      expect_equal(sd(ran_new) / sd(ran_original), 0.501606529117708)
    }
  )
})

test_that("mean of poisson random deviates also expands with sd_mult", {
  lambda <- 10
  sd_mult <- 5
  new_pars <- sens_pois(lambda = lambda, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rpois(10000, lambda)
      ran_new <- rpois(10000, new_pars$lambda)
      expect_equal(mean(ran_new) - mean(ran_original), 239.8269)
    }
  )
})

test_that("mean of poisson random deviates also decreases with sd_mult", {
  lambda <- 10
  sd_mult <- 0.5
  new_pars <- sens_pois(lambda = lambda, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rpois(10000, lambda)
      ran_new <- rpois(10000, new_pars$lambda)
      expect_equal(mean(ran_new) - mean(ran_original), -7.4308)
    }
  )
})

test_that("sens_pois errors with character lambda", {
  lambda <- "ten"
  sd_mult <- 2
  expect_chk_error(
    sens_pois(lambda = lambda, sd_mult = sd_mult)
  )
})

test_that("sens_pois errors with lambda < 0", {
  lambda <- -1
  sd_mult <- 2
  expect_chk_error(
    sens_pois(lambda = lambda, sd_mult = sd_mult)
  )
})

test_that("sens_pois errors with character sd_mult", {
  lambda <- 10
  sd_mult <- "two"
  expect_chk_error(
    sens_pois(lambda = lambda, sd_mult = sd_mult)
  )
})

test_that("sens_pois errors with sd_mult < 0", {
  lambda <- 1
  sd_mult <- -2
  expect_chk_error(
    sens_pois(lambda = lambda, sd_mult = sd_mult)
  )
})

# Gamma ----
test_that("sens_gamma returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_gamma(shape = 2, rate = 10, sd_mult = 2))
})

test_that("sens_gamma errors with NULL input for shape", {
  expect_chk_error(
    sens_gamma(shape = NULL, rate = 10, sd_mult = 2)
  )
})

test_that("sens_gamma errors with NULL input for rate", {
  expect_chk_error(
    sens_gamma(shape = 1, rate = NULL, sd_mult = 2)
  )
})

test_that("sens_gamma errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_gamma(shape = 10, rate = 10, sd_mult = NULL)
  )
})

test_that("sens_gamma errors with empty numeric input for shape", {
  expect_chk_error(
    sens_gamma(shape = numeric(0), rate = 10, sd_mult = 2)
  )
})

test_that("sens_gamma errors with empty numeric input for rate", {
  expect_chk_error(
    sens_gamma(shape = 0.1, rate = numeric(0), sd_mult = 2)
  )
})

test_that("sens_gamma errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_gamma(shape = 10, rate = 10, sd_mult = numeric(0))
  )
})

test_that("sens_gamma errors when provided with a vector of values for shape", {
  expect_chk_error(
    sens_gamma(shape = 10:13, rate = 10, sd_mult = 2)
  )
})

test_that("sens_gamma errors when provided with a vector of values for rate", {
  expect_chk_error(
    sens_gamma(shape = 10, rate = 10:13, sd_mult = 2)
  )
})

test_that("sens_gamma errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_gamma(shape = 10, rate = 10, sd_mult = 2:3)
  )
})

test_that("sd of gamma random deviates expands with sd_mult > 1", {
  shape <- 10
  rate <- 2
  sd_mult <- 2
  new_pars <- sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rgamma(10000, shape, rate)
      ran_new <- rgamma(10000, new_pars$shape, new_pars$rate)
      expect_equal(sd(ran_new) / sd(ran_original), 1.96694615990638)
    }
  )
})

test_that("sd of gamma random deviates reduces with sd_mult < 1", {
  shape <- 10
  rate <- 2
  sd_mult <- 0.4
  new_pars <- sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rgamma(10000, shape, rate)
      ran_new <- rgamma(10000, new_pars$shape, new_pars$rate)
      expect_equal(sd(ran_new) / sd(ran_original), 0.397632673370798)
    }
  )
})

test_that("mean of gamma random deviates stays the same with expanding sd", {
  shape <- 10
  rate <- 2
  sd_mult <- 5
  new_pars <- sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rgamma(10000, shape, rate)
      ran_new <- rgamma(10000, new_pars$shape, new_pars$rate)
      expect_equal(mean(ran_new) - mean(ran_original), -0.0573524039088076)
    }
  )
})

test_that("mean of gamma random deviates stays the same with reducing sd", {
  shape <- 10
  rate <- 2
  sd_mult <- 0.4
  new_pars <- sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- rgamma(10000, shape, rate)
      ran_new <- rgamma(10000, new_pars$shape, new_pars$rate)
      expect_equal(mean(ran_new) - mean(ran_original), -0.0152565649421614)
    }
  )
})

test_that("sens_gamma errors with character shape", {
  shape <- "ten"
  rate <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_gamma errors with shape < 0", {
  shape <- -1
  rate <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_gamma errors with character rate", {
  shape <- 10
  rate <- "two"
  sd_mult <- 2
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_gamma errors with rate = 0", {
  shape <- 10
  rate <- 0
  sd_mult <- 2
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_gamma errors with character sd_mult", {
  shape <- 10
  rate <- 2
  sd_mult <- "two"
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

test_that("sens_gamma errors with sd_mult < 0", {
  shape <- 1
  rate <- 2
  sd_mult <- -2
  expect_chk_error(
    sens_gamma(shape = shape, rate = rate, sd_mult = sd_mult)
  )
})

# Gamma-Poisson ----
test_that("sens_gamma_pois returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_gamma_pois(lambda = 10, theta = 0.1, sd_mult = 2))
})

test_that("sens_gamma_pois errors with NULL input for lambda", {
  expect_chk_error(
    sens_gamma_pois(lambda = NULL, theta = 10, sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors with NULL input for theta", {
  expect_chk_error(
    sens_gamma_pois(lambda = 1, theta = NULL, sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_gamma_pois(lambda = 10, theta = 10, sd_mult = NULL)
  )
})

test_that("sens_gamma_pois errors with empty numeric input for lambda", {
  expect_chk_error(
    sens_gamma_pois(lambda = numeric(0), theta = 10, sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors with empty numeric input for theta", {
  expect_chk_error(
    sens_gamma_pois(lambda = 0.1, theta = numeric(0), sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_gamma_pois(lambda = 10, theta = 10, sd_mult = numeric(0))
  )
})

test_that("sens_gamma_pois errors when provided with a vector of values for lambda", {
  expect_chk_error(
    sens_gamma_pois(lambda = 10:13, theta = 10, sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors when provided with a vector of values for theta", {
  expect_chk_error(
    sens_gamma_pois(lambda = 10, theta = 10:13, sd_mult = 2)
  )
})

test_that("sens_gamma_pois errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_gamma_pois(lambda = 10, theta = 10, sd_mult = 2:3)
  )
})

test_that("sd of gamma-poisson random deviates expands with sd_mult > 1", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_gamma_pois(10000, lambda, theta)
      ran_new <- ran_gamma_pois(10000, new_pars$lambda, new_pars$theta)
      expect_equal(sd(ran_new) / sd(ran_original), 2.00033360241786)
    }
  )
})

test_that("sens_gamma_pois errors when trying to reduce sd", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 0.2
  expect_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult),
    "This function does not currently have the option to reduce the standard deviation"
  )
})

test_that("mean of gamma-poisson random deviates stays the same when expanding sd", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_gamma_pois(10000, lambda, theta)
      ran_new <- ran_gamma_pois(10000, new_pars$lambda, new_pars$theta)
      expect_equal(mean(ran_new) - mean(ran_original), 0.0875000000000004)
    }
  )
})

test_that("sens_gamma_pois errors with character lambda", {
  lambda <- "ten"
  theta <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois errors with lambda < 0", {
  lambda <- -1
  theta <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois errors with character theta", {
  lambda <- 10
  theta <- "two"
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois errors with theta < 0", {
  lambda <- 10
  theta <- -1
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois errors with character sd_mult", {
  lambda <- 10
  theta <- 2
  sd_mult <- "two"
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois errors with sd_mult < 0", {
  lambda <- 1
  theta <- 2
  sd_mult <- -2
  expect_chk_error(
    sens_gamma_pois(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

# Negative Binomial ----
test_that("sens_neg_binom returns a numeric vector of length 2 with correct names", {
  expect_snapshot(sens_neg_binom(lambda = 10, theta = 0.2, sd_mult = 2))
})

test_that("sens_neg_binom errors with NULL input for lambda", {
  expect_chk_error(
    sens_neg_binom(lambda = NULL, theta = 10, sd_mult = 2)
  )
})

test_that("sens_neg_binom errors with NULL input for theta", {
  expect_chk_error(
    sens_neg_binom(lambda = 1, theta = NULL, sd_mult = 2)
  )
})

test_that("sens_neg_binom errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_neg_binom(lambda = 10, theta = 10, sd_mult = NULL)
  )
})

test_that("sens_neg_binom errors with empty numeric input for lambda", {
  expect_chk_error(
    sens_neg_binom(lambda = numeric(0), theta = 10, sd_mult = 2)
  )
})

test_that("sens_neg_binom errors with empty numeric input for theta", {
  expect_chk_error(
    sens_neg_binom(lambda = 0.1, theta = numeric(0), sd_mult = 2)
  )
})

test_that("sens_neg_binom errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_neg_binom(lambda = 10, theta = 10, sd_mult = numeric(0))
  )
})

test_that("sens_neg_binom errors when provided with a vector of values for lambda", {
  expect_chk_error(
    sens_neg_binom(lambda = 10:13, theta = 10, sd_mult = 2)
  )
})

test_that("sens_neg_binom errors when provided with a vector of values for theta", {
  expect_chk_error(
    sens_neg_binom(lambda = 10, theta = 10:13, sd_mult = 2)
  )
})

test_that("sens_neg_binom errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_neg_binom(lambda = 10, theta = 10, sd_mult = 2:3)
  )
})

test_that("sd of negative binomial random deviates expands with sd_mult > 1", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_neg_binom(10000, lambda, theta)
      ran_new <- ran_neg_binom(10000, new_pars$lambda, new_pars$theta)
      expect_equal(sd(ran_new) / sd(ran_original), 2.00033360241786)
    }
  )
})

test_that("sens_neg_binom errors when trying to reduce sd", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 0.2
  expect_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult),
    "This function does not currently have the option to reduce the standard deviation"
  )
})

test_that("mean of negative binomial random deviates stays the same when expanding sd", {
  lambda <- 10
  theta <- 0.1
  sd_mult <- 2
  new_pars <- sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_neg_binom(10000, lambda, theta)
      ran_new <- ran_neg_binom(10000, new_pars$lambda, new_pars$theta)
      expect_equal(mean(ran_new) - mean(ran_original), 0.0875000000000004)
    }
  )
})

test_that("sens_neg_binom errors with character lambda", {
  lambda <- "ten"
  theta <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_neg_binom errors with lambda < 0", {
  lambda <- -1
  theta <- 2
  sd_mult <- 2
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_neg_binom errors with character theta", {
  lambda <- 10
  theta <- "two"
  sd_mult <- 2
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_neg_binom errors with theta < 0", {
  lambda <- 10
  theta <- -1
  sd_mult <- 2
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_neg_binom errors with character sd_mult", {
  lambda <- 10
  theta <- 2
  sd_mult <- "two"
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

test_that("sens_neg_binom errors with sd_mult < 0", {
  lambda <- 1
  theta <- 2
  sd_mult <- -2
  expect_chk_error(
    sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
  )
})

# Zero-inflated Gamma-Poisson ----
test_that("sens_gamma_pois_zi returns a numeric vector of length 3 with correct names", {
  expect_snapshot(sens_gamma_pois_zi(lambda = 2, theta = 0.2, prob = 0.3, sd_mult = 2))
})

test_that("sens_gamma_pois_zi errors with NULL input for lambda", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = NULL, theta = 10, prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with NULL input for theta", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 1, theta = NULL, prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with NULL input for prob", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 1, theta = 0.1, prob = NULL, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with NULL input for sd_mult", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10, theta = 10, prob = 0.1, sd_mult = NULL)
  )
})

test_that("sens_gamma_pois_zi errors with empty numeric input for lambda", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = numeric(0), theta = 10, prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with empty numeric input for theta", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 0.1, theta = numeric(0), prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with empty numeric input for prob", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 0.1, theta = 0.2, prob = numeric(0), sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors with empty numeric input for sd_mult", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10, theta = 10, prob = 0.1, sd_mult = numeric(0))
  )
})

test_that("sens_gamma_pois_zi errors when provided with a vector of values for lambda", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10:13, theta = 10, prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors when provided with a vector of values for theta", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10, theta = 10:13, prob = 0.1, sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors when provided with a vector of values for prob", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10, theta = 10, prob = seq(0.1, 0.3, 0.1), sd_mult = 2)
  )
})

test_that("sens_gamma_pois_zi errors when provided with a vector of values for sd_mult", {
  expect_chk_error(
    sens_gamma_pois_zi(lambda = 10, theta = 10, prob = 0.1, sd_mult = 2:3)
  )
})

test_that("sd of gamma-poisson random deviates expands with sd_mult > 1", {
  lambda <- 100
  theta <- 0.1
  prob <- 0.1
  sd_mult <- 2
  new_pars <- sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_gamma_pois_zi(10000, lambda, theta, prob)
      ran_new <- ran_gamma_pois_zi(10000, new_pars$lambda, new_pars$theta, new_pars$prob)
      expect_equal(sd(ran_new) / sd(ran_original), 2.01617303374106)
    }
  )
})

test_that("sens_gamma_pois_zi errors when trying to reduce sd", {
  lambda <- 10
  theta <- 0.1
  prob <- 0.3
  sd_mult <- 0.2
  expect_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult),
    "This function does not currently have the option to reduce the standard deviation"
  )
})

test_that("mean of gamma-poisson random deviates stays the same when expanding sd", {
  lambda <- 10
  theta <- 0.1
  prob <- 0.3
  sd_mult <- 2
  new_pars <- sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  withr::with_seed(
    101,
    {
      ran_original <- ran_gamma_pois_zi(10000, lambda, theta, prob)
      ran_new <- ran_gamma_pois_zi(10000, new_pars$lambda, new_pars$theta, new_pars$prob)
      expect_equal(mean(ran_new) - mean(ran_original), -0.0522999999999998)
    }
  )
})

test_that("sens_gamma_pois_zi errors with character lambda", {
  lambda <- "ten"
  theta <- 2
  prob <- 0.3
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with lambda < 0", {
  lambda <- -1
  theta <- 2
  prob <- 0.3
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with character theta", {
  lambda <- 10
  theta <- "two"
  prob <- 0.3
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with theta < 0", {
  lambda <- 10
  theta <- -1
  prob <- 0.3
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with character prob", {
  lambda <- 10
  theta <- 2
  prob <- "one"
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with prob < 0", {
  lambda <- 10
  theta <- 0.1
  prob <- -0.2
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with prob > 1", {
  lambda <- 10
  theta <- 0.1
  prob <- 1.1
  sd_mult <- 2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with character sd_mult", {
  lambda <- 10
  theta <- 2
  prob <- 0.3
  sd_mult <- "two"
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})

test_that("sens_gamma_pois_zi errors with sd_mult < 0", {
  lambda <- 1
  theta <- 2
  prob <- 0.3
  sd_mult <- -2
  expect_chk_error(
    sens_gamma_pois_zi(lambda = lambda, theta = theta, prob = prob, sd_mult = sd_mult)
  )
})
