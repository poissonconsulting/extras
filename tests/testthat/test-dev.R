test_that("dev_pois", {
  expect_identical(dev_pois(integer(0), integer(0)), numeric(0))
  expect_identical(dev_pois(1, 1), 0)
  expect_identical(dev_pois(0, 1), 0)
  expect_identical(dev_pois(NA, 1), NA_real_)
  expect_identical(dev_pois(1, NA), NA_real_)
  expect_equal(dev_pois(1, 3), dev_pois(1, 3, res = TRUE)^2)
  expect_equal(dev_pois(c(1,3.5,4), 3, res = TRUE),
               c(-1.34267472705186, 0.281166781094084, 0.549050616623137))
  expect_equal(dev_pois(c(1,3,4), c(1, 3.5, 4), res = TRUE),
                   c(0, -0.274036349845144, 0))
})

test_that("dev_norm", {
  expect_identical(dev_norm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_norm(0), 0)
  expect_identical(dev_norm(NA, 1, 1), NA_real_)
  expect_identical(dev_norm(1, NA, 1), NA_real_)
  expect_identical(dev_norm(1, 1, NA), NA_real_)
  expect_equal(dev_norm(-2), dev_norm(-2, res = TRUE)^2)
  expect_equal(dev_norm(-2:2, res = TRUE), c(-2.82842712474619, -1.4142135623731, 0, 1.4142135623731, 2.82842712474619
  ))
  expect_equal(dev_norm(-2:2, sd = 2, res = TRUE), dev_norm(-2:2, res = TRUE)/2)
  expect_equal(dev_norm(-2:2, sd = 1/2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_norm(-2:2, mean = -2:2, res = TRUE), rep(0, 5))
  expect_equal(dev_norm(-2:2, mean = -1:3, sd = 1:5, res = TRUE),
               c(-1.4142135623731, -0.707106781186548, -0.471404520791032, -0.353553390593274,
                 -0.282842712474619))
})

test_that("dev_lnorm", {
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
  expect_equal(dev_lnorm(exp(-2:2), res = TRUE), c(-2.82842712474619, -1.4142135623731, 0, 1.4142135623731, 2.82842712474619
  ))
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 2, res = TRUE), dev_norm(-2:2, res = TRUE)/2)
  expect_equal(dev_lnorm(exp(-2:2), sdlog = 1/2, res = TRUE), dev_norm(-2:2, res = TRUE) * 2)
  expect_equal(dev_lnorm(exp(-2:2), meanlog = -2:2), rep(0, 5))
  expect_equal(dev_lnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5, res = TRUE),
               c(-1.4142135623731, -0.707106781186548, -0.471404520791032, -0.353553390593274,
                 -0.282842712474619))
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
  expect_equal(dev_binom(1, 10, 0.5, res = TRUE), -3.25271578350125)
  expect_equal(dev_binom(1:9, 10, 0.5, res = TRUE),
               c(-3.25271578350125, -2.74227242773795, -2.17039427586256, -1.47914119729235,
                 0, 1.47914119729235, 2.17039427586256, 2.74227242773795, 3.25271578350125
               ))
})

test_that("dev_bern", {
  expect_identical(dev_bern(logical(0), integer(0)), numeric(0))
  expect_identical(dev_bern(NA, 1), NA_real_)
  expect_identical(dev_bern(1, NA), NA_real_)
  expect_identical(dev_bern(1, 1), 0)
  expect_identical(dev_bern(0, 0), 0)
  expect_identical(dev_bern(1, 0), Inf)
  expect_identical(dev_bern(0, 1), Inf)
  expect_equal(dev_bern(0, 0.5), dev_bern(0, 0.5, res = TRUE)^2)
  expect_identical(dev_bern(0, 1, res = TRUE), -Inf)
  expect_identical(dev_bern(c(1, 1, 0, 0), c(0, 1, 0, 1), res = TRUE),
                   c(Inf, 0, 0, -Inf))
  expect_equal(dev_bern(c(1,0), 0.5, res = TRUE),
               c(1.17741002251547, -1.17741002251547))
  expect_equal(dev_bern(c(1,0), 0.7, res = TRUE),
               c(0.844600430900592, -1.55175565365552))
  expect_equal(dev_bern(c(1,0), c(0.7, 0.5), res = TRUE),
               c(0.844600430900592,  -1.17741002251547))
})

test_that("dev_gamma_pois", {
  expect_identical(dev_gamma_pois(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(dev_gamma_pois(1, 1, 0), 0)
  expect_identical(dev_gamma_pois(0, 1, 0), 0)
  expect_identical(dev_gamma_pois(1, 1, 1), 0)
  expect_equal(dev_gamma_pois(0, 1, 0), dev_pois(0, 1))

  expect_identical(dev_gamma_pois(NA, 1, 1), NA_real_)
  expect_identical(dev_gamma_pois(1, NA, 1), NA_real_)
  expect_identical(dev_gamma_pois(1, 1, NA), NA_real_)
  expect_equal(dev_gamma_pois(1, 3, 1), dev_gamma_pois(1, 3, 1, res = TRUE)^2)

  expect_equal(dev_gamma_pois(c(1, 3.5 , 4), 3, 0, res = TRUE),
               c(-1.34267472705186, 0.281166781094084, 0.549050616623137))
  expect_equal(dev_gamma_pois(c(1,3,4), c(1, 3.5, 4), 0, res = TRUE),
               c(0, -0.274036349845144, 0))
})
