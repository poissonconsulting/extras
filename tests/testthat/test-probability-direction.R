test_that("probability_direction() returns correct left-sided p-values.", {
  expect_identical(probability_direction(NA_integer_, side = "left"), NA_real_)
  expect_identical(probability_direction(NA_real_, side = "left"), NA_real_)
  expect_identical(probability_direction(NaN, side = "left"), NA_real_)
  expect_identical(probability_direction(integer(0), side = "left"), NA_real_)
  expect_equal(probability_direction(c(1, NA), side = "left"), NA_real_)
  expect_equal(probability_direction(1, side = "left"), 1 / (1 + 1))
  expect_equal(probability_direction(c(1, 1), side = "left"), 1 / (2 + 1))
  expect_equal(probability_direction(c(1, 1, 1), side = "left"), 1 / (3 + 1))
  expect_identical(probability_direction(1:9, side = "left"), 1 / (9 + 1))
  expect_identical(probability_direction(-(1:9), side = "left"), 9 / (9 + 1))
  expect_equal(probability_direction(c(-1, 1, 1), side = "left"), 1 / 3)
  expect_equal(probability_direction(c(-1, 1, 1, 1), side = "left"), 1 / 4)
  expect_equal(probability_direction(c(-1, -1, 1, 1), side = "left"), 2 / 4)
  expect_equal(probability_direction(0, side = "left"), 1 / (1 + 1))
  expect_equal(probability_direction(c(0, 0), side = "left"), 1 / (2 + 1))
  expect_equal(probability_direction(c(0, 0, 0), side = "left"), 1 / (3 + 1))
  expect_equal(probability_direction(c(rep(-1, 25), rep(1, 1000 - 25)), side = "left"), 25 / 1000)
  expect_equal(probability_direction(c(rep(-1, 24), rep(1, 1000 - 24)), side = "left"), 24 / 1000)
  expect_equal(probability_direction(c(rep(-1, 25), rep(0, 1000 - 25)), side = "left"), 25/1000)
  expect_equal(probability_direction(-9:10, side = "left"), 9 / 20)
  expect_equal(probability_direction(-9:10 + 1, threshold = 1, side = "left"), 9 / 20)
  expect_equal(probability_direction(c(-1, 1, 1, 1, NA), side = "left"), NA_real_)
  expect_equal(probability_direction(c(-1, 1, 1, 1, NA), side = "left", na_rm = TRUE), 1 / 4)
  expect_equal(probability_direction(rep(1, 1e6), side = "left"),
               1 / (1e6 + 1))
  expect_equal(probability_direction(x = rnorm(1e7, mean = qnorm(0.95)),
                                           side = "left"),
               0.05, tolerance = 0.01)
  expect_equal(probability_direction(x = rcauchy(1e7, location = qcauchy(0.95)),
                                           side = "left"),
               0.05, tolerance = 0.01)

  # NOTE: poisson distribution is discrete, so p-values are more complicated
  expect_equal(probability_direction(x = rpois(1e7, lambda = 3), threshold = 3,
                                     side = "left"),
               ppois(2, 3), tolerance = 0.01)
  expect_equal(probability_direction(x = rpois(1e7, lambda = 3),
                                     side = "left", threshold = qpois(0.05, 3)),
               ppois(qpois(0.05, 3) - 1, 3), tolerance = 0.01)
})

test_that("probability_direction() returns correct right-sided p-values.", {
  expect_identical(probability_direction(NA_integer_, side = "right"), NA_real_)
  expect_identical(probability_direction(integer(0), side = "right"), NA_real_)
  expect_equal(probability_direction(c(1, NA), side = "right"), NA_real_)
  expect_equal(probability_direction(-1, side = "right"), 1 / (1 + 1))
  expect_equal(probability_direction(-c(1, 1), side = "right"), 1 / (2 + 1))
  expect_equal(probability_direction(-c(1, 1, 1), side = "right"), 1 / (3 + 1))
  expect_identical(probability_direction(-(1:9), side = "right"), 1 / (9 + 1))
  expect_identical(probability_direction(1:9, side = "right"), 9 / (9 + 1))
  expect_equal(probability_direction(c(-1, 1, 1), side = "right"), 2 / 3)
  expect_equal(probability_direction(c(-1, 1, 1, 1), side = "right"), 3 / 4)
  expect_equal(probability_direction(c(-1, -1, 1, 1), side = "right"), 2 / 4)
  expect_equal(probability_direction(0, side = "right"), 1 / (1 + 1))
  expect_equal(probability_direction(c(0, 0), side = "right"), 1 / (2 + 1))
  expect_equal(probability_direction(c(0, 0, 0), side = "right"), 1 / (3 + 1))
  expect_equal(probability_direction(c(rep(-1, 25), rep(1, 1000 - 25)),
                                     side = "right"), (1000 - 25) / 1000)
  expect_equal(probability_direction(c(rep(-1, 24), rep(1, 1000 - 24)),
                                     side = "right"), (1000 - 24) / 1000)
  expect_equal(probability_direction(c(rep(-1, 25), rep(0, 1000 - 25)),
                                     side = "right"), 1 / (1000 + 1))
  expect_equal(probability_direction(-9:10, side = "right"), 10 / 20)
  expect_equal(probability_direction(-9:10 + 1, threshold = 1, side = "right"),
               10 / 20)
  expect_equal(probability_direction(c(-1, 1, 1, 1, NA), side = "right"),
               NA_real_)
  expect_equal(probability_direction(c(-1, 1, 1, 1, NA), side = "right",
                                     na_rm = TRUE), 3 / 4)
  expect_equal(probability_direction(rep(-10, 1e6), side = "right"),
               1 / (1e6 + 1))
  expect_equal(probability_direction(x = rnorm(1e7, mean = qnorm(0.95)),
                                     side = "right"),
               0.95, tolerance = 0.01)
  expect_equal(probability_direction(x = rcauchy(1e7, location = qcauchy(0.95)),
                                     side = "right"),
               0.95, tolerance = 0.01)

  # NOTE: poisson distribution is discrete, so p-values are more complicated
  expect_equal(probability_direction(x = rpois(1e7, lambda = 3), threshold = 3,
                                     side = "right"),
               ppois(3, 3, lower.tail = FALSE), tolerance = 0.01)
  expect_equal(probability_direction(x = rpois(1e7, lambda = 3),
                                     side = "right", threshold = qpois(0.05, 3)),
               ppois(qpois(0.05, 3), 3, lower.tail = FALSE), tolerance = 0.1)
})

test_that("probability_direction() only accepts `side` as left, right, or both.", {
  expect_no_error(probability_direction(1, side = "left"))
  expect_no_error(probability_direction(1, side = "right"))
  expect_error(probability_direction(1, side = "aaa"),
               "`side` must match 'left' or 'right', not 'aaa'.")
})
