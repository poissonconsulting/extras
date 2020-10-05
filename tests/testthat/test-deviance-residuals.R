test_that("devpois", {
  expect_identical(devpois(integer(0), integer(0)), numeric(0))
  expect_identical(devpois(1, 1), 0)
  expect_identical(devpois(0, 1), 0)
  expect_identical(devpois(NA, 1), NA_real_)
  expect_identical(devpois(1, NA), NA_real_)
  expect_equal(devpois(c(1,3.5,4), 3),
               c(-0.900516638500549, 0.474701921800054, 0.758527616440932))
  expect_identical(devpois(c(1,3,4), c(1, 3.5, 4)),
                   c(0, 0, 0))
})

test_that("devnorm", {
  expect_identical(devnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(devnorm(0), 0)
  expect_identical(devnorm(NA, 1, 1), NA_real_)
  expect_identical(devnorm(1, NA, 1), NA_real_)
  expect_identical(devnorm(1, 1, NA), NA_real_)
  expect_equal(devnorm(-2:2), -2:2)
  expect_equal(devnorm(-2:2, sd = 2), -2:2/2)
  expect_equal(devnorm(-2:2, sd = 1/2), -2:2 * 2)
  expect_equal(devnorm(-2:2, mean = -2:2), rep(0, 5))
  expect_equal(devnorm(-2:2, mean = -1:3, sd = 1:5),
               c(-1, -0.5, -1/3, -0.25, -0.2))
})

test_that("devlnorm", {
  expect_identical(devlnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(devlnorm(exp(0)), 0)
  expect_identical(devlnorm(1), 0)
  expect_identical(devlnorm(0), -Inf)
  expect_identical(devlnorm(-1), -Inf)
  expect_identical(devlnorm(NA, 1, 1), NA_real_)
  expect_identical(devlnorm(1, NA, 1), NA_real_)
  expect_identical(devlnorm(1, 1, NA), NA_real_)
  expect_equal(devlnorm(exp(-2:2)), -2:2)
  expect_equal(devlnorm(exp(-2:2), sdlog = 2), -2:2/2)
  expect_equal(devlnorm(exp(-2:2), sdlog = 1/2), -2:2 * 2)
  expect_equal(devlnorm(exp(-2:2), meanlog = -2:2), rep(0, 5))
  expect_equal(devlnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5),
               c(-1, -0.5, -1/3, -0.25, -0.2))
})

test_that("devbinom", {
#   expect_identical(devbinom(integer(0), integer(0), integer(0)), numeric(0))
#   expect_identical(devbinom(NA, 1, 1), NA_real_)
#   expect_identical(devbinom(1, NA, 1), NA_real_)
#   expect_identical(devbinom(1, 1, NA), NA_real_)
# #  expect_identical(devbinom(0, 1, 0.5), -1.17741002251547) # should be same as devbern?
# #  expect_identical(devbinom(1, 1, 0.5), 1.17741002251547) # should be same as devbern?
# #  expect_identical(devbinom(0, 1, 0.7), 0.844600430900592) # should be same as devbern?
# #  expect_identical(devbinom(1, 1, 0.7), -1.55175565365552) # should be same as devbern?
#   expect_identical(devbinom(1, 2, 0.5), 0)
#   expect_identical(devbinom(5, 10, 0.5), 0)
#   expect_equal(devbinom(1, 10, 0.5), -2.71316865369073)
#   expect_equal(devbinom(1:9, 10, 0.5),
#                c(-2.71316865369073, -1.96338868806845,
#                  -1.28283185573988, -0.634594572159089,
#                  0, 0.634594572159089, 1.28283185573988,
#                  1.96338868806845, 2.71316865369073))
})

test_that("devbern", {
  expect_identical(devbern(logical(0), integer(0)), numeric(0))
  expect_identical(devbern(NA, 1), NA_real_)
  expect_identical(devbern(1, NA), NA_real_)
  expect_identical(devbern(1, 1), 0)
  expect_identical(devbern(0, 0), 0)
  expect_identical(devbern(1, 0), Inf)
  expect_identical(devbern(0, 1), -Inf)
  expect_identical(devbern(c(1, 1, 0, 0), c(0, 1, 0, 1)),
                   c(Inf, 0, 0, -Inf))
  expect_equal(devbern(c(1,0), 0.5),
               c(1.17741002251547, -1.17741002251547))
  expect_equal(devbern(c(1,0), 0.7),
               c(0.844600430900592, -1.55175565365552))
  expect_equal(devbern(c(1,0), c(0.7, 0.5)),
               c(0.844600430900592,  -1.17741002251547))
})

