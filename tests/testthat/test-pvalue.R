test_that("pvalue() returns correct two-sided p-values.", {
  expect_identical(pvalue(NA_integer_), NA_real_)
  expect_identical(pvalue(integer(0)), NA_real_)
  expect_equal(pvalue(c(1, NA)), NA_real_)
  expect_equal(pvalue(1), 1 / 2)
  expect_equal(pvalue(c(1, 1)), 1 / 3)
  expect_equal(pvalue(c(1, 1, 1)), 1 / 4)
  expect_identical(pvalue(1:9), 1 / 10)
  expect_identical(pvalue(-(1:9)), 1 / 10)
  expect_equal(pvalue(c(-1, 1, 1)), 1 / 3 * 2)
  expect_equal(pvalue(c(-1, 1, 1, 1)), 1 / 4 * 2)
  expect_equal(pvalue(c(-1, -1, 1, 1)), 2 / 4 * 2)
  expect_equal(pvalue(0), 1)
  expect_equal(pvalue(c(0, 0)), 1)
  expect_equal(pvalue(c(0, 0, 0)), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25))), 25 / 1000 * 2)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24))), 24 / 1000 * 2)
  expect_equal(pvalue(c(rep(-1, 25), rep(0, 1000 - 25))), (1000 - 25) / 1000)
  expect_equal(pvalue(-9:10), 19/20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1), 19/20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA)), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), na_rm = TRUE), 1 / 4 * 2)

  expect_equal(pvalue(rnorm(1e7, mean = qnorm(0.95))),
               0.05 * 2, tolerance = 0.01)
  expect_equal(pvalue(rnorm(1e7, mean = qnorm(0.05))),
               0.05 * 2, tolerance = 0.01)
  expect_equal(pvalue(rnorm(1e6, mean = 1e3)), 1 / (1e6 + 1), tolerance = 0.01)
})

test_that("pvalue() returns the correct one-sided p-values", {
  expect_identical(pvalue(NA_integer_, side = "left"), NA_real_)
  expect_identical(pvalue(integer(0), side = "left"), NA_real_)
  expect_equal(pvalue(c(1, NA), side = "left"), NA_real_)
  expect_equal(pvalue(1, side = "left"), 1 / 2)
  expect_equal(pvalue(c(1, 1), side = "left"), 1 / 3)
  expect_equal(pvalue(c(1, 1, 1), side = "left"), 1 / 4)
  expect_identical(pvalue(1:9, side = "left"), 1 / 10)
  expect_identical(pvalue(-(1:9), side = "left"), 1)
  expect_equal(pvalue(c(-1, 1, 1), side = "left"), 1 / 3)
  expect_equal(pvalue(c(-1, 1, 1, 1), side = "left"), 1 / 4)
  expect_equal(pvalue(c(-1, -1, 1, 1), side = "left"), 2 / 4)
  expect_equal(pvalue(0, side = "left"), 1)
  expect_equal(pvalue(c(0, 0), side = "left"), 1)
  expect_equal(pvalue(c(0, 0, 0), side = "left"), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "left"), 25 / 1000)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24)), side = "left"), 24 / 1000)
  expect_equal(pvalue(c(rep(-1, 25), rep(0, 1000 - 25)), side = "left"), 1)
  expect_equal(pvalue(-9:10, side = "left"), (9 + 1) / 20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1, side = "left"), (9 + 1) / 20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "left"), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "left", na_rm = TRUE), 1 / 4)

  expect_identical(pvalue(NA_integer_, side = "right"), NA_real_)
  expect_identical(pvalue(integer(0), side = "right"), NA_real_)
  expect_equal(pvalue(c(1, NA), side = "right"), NA_real_)
  expect_equal(pvalue(-1, side = "right"), 1 / 2)
  expect_equal(pvalue(-c(1, 1), side = "right"), 1 / 3)
  expect_equal(pvalue(-c(1, 1, 1), side = "right"), 1 / 4)
  expect_identical(pvalue(1:9, side = "right"), 1)
  expect_identical(pvalue(-(1:9), side = "right"), 1 / 10)
  expect_equal(pvalue(c(-1, 1, 1), side = "right"), 2 / 3)
  expect_equal(pvalue(c(-1, 1, 1, 1), side = "right"), 3 / 4)
  expect_equal(pvalue(c(-1, -1, 1, 1), side = "right"), 2 / 4)
  expect_equal(pvalue(0, side = "right"), 1)
  expect_equal(pvalue(c(0, 0), side = "right"), 1)
  expect_equal(pvalue(c(0, 0, 0), side = "right"), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "right"), 975 / 1000)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24)), side = "right"), 976 / 1000)
  expect_equal(pvalue(c(rep(1, 25), rep(0, 1000 - 25)), side = "right"), 1)
  expect_equal(pvalue(-9:10, side = "right"), (10 + 1) / 20)
  expect_equal(pvalue(-9:10 + 1, threshold = 1, side = "right"), (10 + 1) / 20)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "right"), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), side = "right", na_rm = TRUE), 3 / 4)

  expect_equal(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            side = "left"),
               0.05, tolerance = 0.01)
  expect_equal(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            side = "right"),
               0.95, tolerance = 0.01)

  expect_equal(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            side = "left"),
               0.05, tolerance = 0.01)
  expect_equal(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            side = "right"),
               0.95, tolerance = 0.01)

  # NOTE: poisson distribution is discrete, so p-values are more complicated
  expect_equal(pvalue(x = rpois(1e7, lambda = 3)),
               dpois(0, 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "left"),
               dpois(0, 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "right"),
               1, tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "left", threshold = qpois(0.05, 3)),
               ppois(qpois(0.05, 3), 3), tolerance = 0.01)
  expect_equal(pvalue(x = rpois(1e7, lambda = 3),
                            side = "right",
                            threshold = qpois(0.05, 3)),
               ppois(qpois(0.05, 3) - 1, # correct for boundary inclusion
                           3, lower.tail = FALSE), tolerance = 0.01)

})

test_that("pvalue() only accepts `side` as left, right, or both.", {
  expect_no_error(pvalue(1))
  expect_no_error(pvalue(1, side = "both"))
  expect_no_error(pvalue(1, side = "left"))
  expect_no_error(pvalue(1, side = "right"))
  expect_error(pvalue(1, side = "aaa"),
               "`side` must match 'both', 'left' or 'right', not 'aaa'.")
})
