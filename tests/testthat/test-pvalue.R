test_that("pvalue() returns correct two-sided p-values.", {
  expect_identical(pvalue(NA_integer_, skeptical = TRUE), NA_real_)
  expect_identical(pvalue(integer(0), skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(1, NA), skeptical = TRUE), NA_real_)
  expect_equal(pvalue(1, skeptical = TRUE), 1 / 2)
  expect_equal(pvalue(c(1, 1), skeptical = TRUE), 1 / 3)
  expect_equal(pvalue(c(1, 1, 1), skeptical = TRUE), 1 / 4)
  expect_identical(pvalue(1:9, skeptical = TRUE), 1 / 10)
  expect_identical(pvalue(-(1:9), skeptical = TRUE), 1 / 10)
  expect_equal(pvalue(c(-1, 1, 1), skeptical = TRUE), 1 / 3 * 2)
  expect_equal(pvalue(c(-1, 1, 1, 1), skeptical = TRUE), 1 / 4 * 2)
  expect_equal(pvalue(c(-1, -1, 1, 1), skeptical = TRUE), 2 / 4 * 2)
  expect_equal(pvalue(0, skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0), skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0, 0), skeptical = TRUE), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25)), skeptical = TRUE), 25 / 1000 * 2)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24)), skeptical = TRUE), 24 / 1000 * 2)
  expect_equal(pvalue(c(rep(-1, 25), rep(0, 1000 - 25)), skeptical = TRUE), (1000 - 25) / 1000)
  expect_equal(pvalue(-9:10, skeptical = TRUE), 19/20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1, skeptical = TRUE), 19/20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), na_rm = TRUE, skeptical = TRUE), 1 / 4 * 2)

  expect_equal(pvalue(rnorm(1e7, mean = qnorm(0.95)), skeptical = TRUE),
               0.05 * 2, tolerance = 0.01)
  expect_equal(pvalue(rnorm(1e7, mean = qnorm(0.05)), skeptical = TRUE),
               0.05 * 2, tolerance = 0.01)
  expect_equal(pvalue(rnorm(1e6, mean = 1e3), skeptical = TRUE), 1 / (1e6 + 1), tolerance = 0.01)
})

test_that("pvalue() returns the correct one-sided p-values", {
  expect_identical(pvalue(NA_integer_, side = "left", skeptical = TRUE), NA_real_)
  expect_identical(pvalue(integer(0), side = "left", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(1, NA), side = "left", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(1, side = "left", skeptical = TRUE), 1 / 2)
  expect_equal(pvalue(c(1, 1), side = "left", skeptical = TRUE), 1 / 3)
  expect_equal(pvalue(c(1, 1, 1), side = "left", skeptical = TRUE), 1 / 4)
  expect_identical(pvalue(1:9, side = "left", skeptical = TRUE), 1 / 10)
  expect_identical(pvalue(-(1:9), side = "left", skeptical = TRUE), 1)
  expect_equal(pvalue(c(-1, 1, 1), side = "left", skeptical = TRUE), 1 / 3)
  expect_equal(pvalue(c(-1, 1, 1, 1), side = "left", skeptical = TRUE), 1 / 4)
  expect_equal(pvalue(c(-1, -1, 1, 1), side = "left", skeptical = TRUE), 2 / 4)
  expect_equal(pvalue(0, side = "left", skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0), side = "left", skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0, 0), side = "left", skeptical = TRUE), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "left", skeptical = TRUE), 25 / 1000)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24)), side = "left", skeptical = TRUE), 24 / 1000)
  expect_equal(pvalue(c(rep(-1, 25), rep(0, 1000 - 25)), side = "left", skeptical = TRUE), 1)
  expect_equal(pvalue(-9:10, side = "left", skeptical = TRUE), (9 + 1) / 20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1, side = "left", skeptical = TRUE), (9 + 1) / 20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "left", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "left", na_rm = TRUE, skeptical = TRUE), 1 / 4)

  expect_identical(pvalue(NA_integer_, side = "right", skeptical = TRUE), NA_real_)
  expect_identical(pvalue(integer(0), side = "right", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(1, NA), side = "right", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(-1, side = "right", skeptical = TRUE), 1 / 2)
  expect_equal(pvalue(-c(1, 1), side = "right", skeptical = TRUE), 1 / 3)
  expect_equal(pvalue(-c(1, 1, 1), side = "right", skeptical = TRUE), 1 / 4)
  expect_identical(pvalue(1:9, side = "right", skeptical = TRUE), 1)
  expect_identical(pvalue(-(1:9), side = "right", skeptical = TRUE), 1 / 10)
  expect_equal(pvalue(c(-1, 1, 1), side = "right", skeptical = TRUE), 2 / 3)
  expect_equal(pvalue(c(-1, 1, 1, 1), side = "right", skeptical = TRUE), 3 / 4)
  expect_equal(pvalue(c(-1, -1, 1, 1), side = "right", skeptical = TRUE), 2 / 4)
  expect_equal(pvalue(0, side = "right", skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0), side = "right", skeptical = TRUE), 1)
  expect_equal(pvalue(c(0, 0, 0), side = "right", skeptical = TRUE), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "right", skeptical = TRUE), 975 / 1000)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24)), side = "right", skeptical = TRUE), 976 / 1000)
  expect_equal(pvalue(c(rep(1, 25), rep(0, 1000 - 25)), side = "right", skeptical = TRUE), 1)
  expect_equal(pvalue(-9:10, side = "right", skeptical = TRUE), (10 + 1) / 20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1, side = "right", skeptical = TRUE), (10 + 1) / 20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "right", skeptical = TRUE), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "right", na_rm = TRUE, skeptical = TRUE), 3 / 4)

  expect_equal(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            side = "left", skeptical = TRUE),
               0.05, tolerance = 0.01)
  expect_equal(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            side = "right", skeptical = TRUE),
               0.95, tolerance = 0.01)

  expect_equal(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            side = "left", skeptical = TRUE),
               0.05, tolerance = 0.01)
  expect_equal(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            side = "right", skeptical = TRUE),
               0.95, tolerance = 0.01)

  # NOTE: poisson distribution is discrete, so p-values are more complicated
  expect_equal(pvalue(x = rpois(1e7, lambda = 3), skeptical = TRUE),
               dpois(0, 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "left", skeptical = TRUE),
               dpois(0, 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "right", skeptical = TRUE),
               1, tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "left", threshold = qpois(0.05, 3), skeptical = TRUE),
               ppois(qpois(0.05, 3), 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "right",
                            threshold = qpois(0.05, 3), skeptical = TRUE),
               ppois(qpois(0.05, 3) - 1, # correct for boundary inclusion
                           3, lower.tail = FALSE), tolerance = 0.01)

})

test_that("pvalue() errors with unused arguments", {
  expect_error(pvalue(1, 0))
  expect_error(pvalue(1, foo = TRUE))
})

test_that("pvalue() warns when skeptical is not supplied", {
  lifecycle::expect_deprecated(pvalue(1))
  expect_no_warning(pvalue(1, skeptical = TRUE))
  expect_no_warning(pvalue(1, skeptical = FALSE))
})

test_that("pvalue() skeptical argument controls sample-size correction", {
  expect_identical(pvalue(1:9, skeptical = TRUE), 1 / 10)
  expect_identical(pvalue(1:9, skeptical = FALSE), 0)
  expect_identical(pvalue(-(1:9), skeptical = TRUE), 1 / 10)
  expect_identical(pvalue(-(1:9), skeptical = FALSE), 0)
  expect_identical(pvalue(c(-1, 1, 1), skeptical = TRUE),
                   pvalue(c(-1, 1, 1), skeptical = FALSE))
  expect_identical(pvalue(c(-1, 1, 1), side = "left", skeptical = TRUE),
                   pvalue(c(-1, 1, 1), side = "left", skeptical = FALSE))
  expect_identical(pvalue(c(-1, 1, 1), side = "right", skeptical = TRUE),
                   pvalue(c(-1, 1, 1), side = "right", skeptical = FALSE))
})

test_that("pvalue() only accepts `side` as left, right, or both.", {
  expect_no_error(pvalue(1, skeptical = TRUE))
  expect_no_error(pvalue(1, side = "both", skeptical = TRUE))
  expect_no_error(pvalue(1, side = "left", skeptical = TRUE))
  expect_no_error(pvalue(1, side = "right", skeptical = TRUE))
  expect_error(pvalue(1, side = "aaa", skeptical = TRUE),
               "`side` must match 'both', 'left' or 'right', not 'aaa'.")
})
