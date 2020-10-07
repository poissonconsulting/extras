test_that("devpois", {
  expect_identical(devpois(integer(0), integer(0)), numeric(0))
  expect_identical(devpois(1, 1), 0)
  expect_identical(devpois(0, 1), 0)
  expect_identical(devpois(NA, 1), NA_real_)
  expect_identical(devpois(1, NA), NA_real_)
  expect_equal(devpois(1, 3), devpois(1, 3, residual = TRUE)^2)
  expect_equal(devpois(c(1,3.5,4), 3, residual = TRUE),
               c(-1.34267472705186, 0.281166781094084, 0.549050616623137))
  expect_equal(devpois(c(1,3,4), c(1, 3.5, 4), residual = TRUE),
                   c(0, -0.274036349845144, 0))
})

test_that("devnorm", {
  expect_identical(devnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(devnorm(0), 0)
  expect_identical(devnorm(NA, 1, 1), NA_real_)
  expect_identical(devnorm(1, NA, 1), NA_real_)
  expect_identical(devnorm(1, 1, NA), NA_real_)
  expect_equal(devnorm(-2), devnorm(-2, residual = TRUE)^2)
  expect_equal(devnorm(-2:2, residual = TRUE), c(-2.82842712474619, -1.4142135623731, 0, 1.4142135623731, 2.82842712474619
  ))
  expect_equal(devnorm(-2:2, sd = 2, residual = TRUE), devnorm(-2:2, residual = TRUE)/2)
  expect_equal(devnorm(-2:2, sd = 1/2, residual = TRUE), devnorm(-2:2, residual = TRUE) * 2)
  expect_equal(devnorm(-2:2, mean = -2:2, residual = TRUE), rep(0, 5))
  expect_equal(devnorm(-2:2, mean = -1:3, sd = 1:5, residual = TRUE),
               c(-1.4142135623731, -0.707106781186548, -0.471404520791032, -0.353553390593274,
                 -0.282842712474619))
})

test_that("devlnorm", {
  expect_identical(devlnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(devlnorm(exp(0)), 0)
  expect_identical(devlnorm(1), 0)
  expect_identical(devlnorm(0, residual = TRUE), -Inf)
  expect_identical(devlnorm(0), Inf)
  expect_identical(devlnorm(-1, residual = TRUE), -Inf)
  expect_identical(devlnorm(NA, 1, 1), NA_real_)
  expect_identical(devlnorm(1, NA, 1), NA_real_)
  expect_identical(devlnorm(1, 1, NA), NA_real_)
  expect_equal(devlnorm(-2), devlnorm(-2, residual = TRUE)^2)
  expect_equal(devlnorm(exp(-2:2), residual = TRUE), c(-2.82842712474619, -1.4142135623731, 0, 1.4142135623731, 2.82842712474619
  ))
  expect_equal(devlnorm(exp(-2:2), sdlog = 2, residual = TRUE), devnorm(-2:2, residual = TRUE)/2)
  expect_equal(devlnorm(exp(-2:2), sdlog = 1/2, residual = TRUE), devnorm(-2:2, residual = TRUE) * 2)
  expect_equal(devlnorm(exp(-2:2), meanlog = -2:2), rep(0, 5))
  expect_equal(devlnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5, residual = TRUE),
               c(-1.4142135623731, -0.707106781186548, -0.471404520791032, -0.353553390593274,
                 -0.282842712474619))
})

test_that("devbinom", {
  expect_identical(devbinom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(devbinom(NA, 1, 1), NA_real_)
  expect_identical(devbinom(1, NA, 1), NA_real_)
  expect_identical(devbinom(1, 1, NA), NA_real_)
  expect_equal(devbinom(1, 3, 0.5), devbinom(1, 3, 0.5, residual = TRUE)^2)
  expect_equal(devbinom(0, 1, 0.5, residual = TRUE), -1.17741002251547)
  expect_equal(devbinom(1, 1, 0.5, residual = TRUE), 1.17741002251547)
  expect_equal(devbinom(0, 1, 0.7, residual = TRUE), -1.55175565365552)
  expect_equal(devbinom(1, 1, 0.7, residual = TRUE), 0.844600430900592)
  expect_identical(devbinom(1, 2, 0.5), 0)
  expect_identical(devbinom(5, 10, 0.5), 0)
  expect_equal(devbinom(1, 10, 0.5, residual = TRUE), -3.25271578350125)
  expect_equal(devbinom(1:9, 10, 0.5, residual = TRUE),
               c(-3.25271578350125, -2.74227242773795, -2.17039427586256, -1.47914119729235,
                 0, 1.47914119729235, 2.17039427586256, 2.74227242773795, 3.25271578350125
               ))
})

test_that("devbern", {
  expect_identical(devbern(logical(0), integer(0)), numeric(0))
  expect_identical(devbern(NA, 1), NA_real_)
  expect_identical(devbern(1, NA), NA_real_)
  expect_identical(devbern(1, 1), 0)
  expect_identical(devbern(0, 0), 0)
  expect_identical(devbern(1, 0), Inf)
  expect_identical(devbern(0, 1), Inf)
  expect_equal(devbern(0, 0.5), devbern(0, 0.5, residual = TRUE)^2)
  expect_identical(devbern(0, 1, residual = TRUE), -Inf)
  expect_identical(devbern(c(1, 1, 0, 0), c(0, 1, 0, 1), residual = TRUE),
                   c(Inf, 0, 0, -Inf))
  expect_equal(devbern(c(1,0), 0.5, residual = TRUE),
               c(1.17741002251547, -1.17741002251547))
  expect_equal(devbern(c(1,0), 0.7, residual = TRUE),
               c(0.844600430900592, -1.55175565365552))
  expect_equal(devbern(c(1,0), c(0.7, 0.5), residual = TRUE),
               c(0.844600430900592,  -1.17741002251547))
})
