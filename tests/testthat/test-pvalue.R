test_that("pvalue() returns correct two-sided p-values.", {
  expect_identical(pvalue(NA_integer_), NA_real_)
  expect_identical(pvalue(integer(0)), NA_real_)
  expect_equal(pvalue(c(1, NA)), NA_real_)
  expect_equal(pvalue(1), 1 / 2)
  expect_equal(pvalue(c(1, 1)), 1 / 3)
  expect_equal(pvalue(c(1, 1, 1)), 1 / 4)
  expect_identical(pvalue(1:9), 1 / 10)
  expect_identical(pvalue(-(1:9)), 1 / 10)
  expect_equal(pvalue(c(-1, 1, 1)), 3 / 4)
  expect_equal(pvalue(c(-1, 1, 1, 1)), 3 / 5)
  expect_equal(pvalue(c(-1, -1, 1, 1)), 1)
  expect_equal(pvalue(0), 1)
  expect_equal(pvalue(c(0, 0)), 1)
  expect_equal(pvalue(c(0, 0, 0)), 1)
  expect_equal(pvalue(c(rep(-1, 25), rep(1, 1000 - 25))), 51 / 1001)
  expect_equal(pvalue(c(rep(-1, 24), rep(1, 1000 - 24))), 49 / 1001)
  expect_equal(pvalue(c(rep(-1, 25), rep(0, 1000 - 25))), 976 / 1001)
  expect_equal(pvalue(-9:10), 0.952380952380952)
  expect_equal(pvalue(-9:10 + 1, threshold = 1), 0.952380952380952)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA)), NA_real_)
  expect_equal(pvalue(c(-1, 1, 1, 1, NA), na_rm = TRUE), 3 / 5)

  expect_equal(round(pvalue(rnorm(1e7, mean = qnorm(0.95))), 2),
               0.05 * 2)
  expect_equal(round(pvalue(rnorm(1e7, mean = qnorm(0.05))), 2),
               0.05 * 2)
})

test_that("pvalue() returns the correct one-sided p-values", {
  expect_equal(round(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            two_sided = FALSE, side = "left"), 2),
               0.05)
  expect_equal(round(pvalue(x = rnorm(1e7, mean = qnorm(0.95)),
                            two_sided = FALSE, side = "right"), 2),
               0.95)

  expect_equal(round(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            two_sided = FALSE, side = "left"), 2),
               0.05)
  expect_equal(round(pvalue(x = rcauchy(1e7, location = qcauchy(0.95)),
                            two_sided = FALSE, side = "right"), 2),
               0.95)

  # NOTE: poisson distribution is discrete, so p-values are more complicated
  expect_equal(round(pvalue(x = rpois(1e7, lambda = 3), two_sided = TRUE), 2),
               round(dpois(0, 3), 2))
  expect_equal(round(pvalue(x = rpois(1e7, lambda = 3), two_sided = FALSE,
                            side = "left"), 2),
               round(dpois(0, 3), 2))
  expect_equal(round(pvalue(x = rpois(1e7, lambda = 3), two_sided = FALSE,
                            side = "right"), 2),
               1)
  expect_equal(round(pvalue(x = rpois(1e7, lambda = 3), two_sided = FALSE,
                            side = "left", threshold = qpois(0.05, 3)), 2),
               round(ppois(qpois(0.05, 3), 3), 2))
  expect_equal(round(pvalue(x = rpois(1e7, lambda = 3), two_sided = FALSE,
                            side = "right",
                            threshold = qpois(0.05, 3)), 2),
               round(ppois(qpois(0.05, 3) - 1, # correct for boundary inclusion
                           3, lower.tail = FALSE), 2))

})

test_that("pvalue() ignores `side` if `two_side = TRUE` but throws a warning", {
  expect_no_error(pvalue(1))
  expect_no_error(pvalue(1, two_sided = TRUE))
  expect_warning(pvalue(1, two_sided = TRUE, side = "left"))
  expect_warning(pvalue(1, two_sided = TRUE, side = "right"))
  expect_warning(pvalue(1, two_sided = TRUE, side = "aaa"))
})

test_that("pvalue() requires a valid `side` if `two_side = FALSE` and throws an error otherwise", {
  expect_error(pvalue(1, two_sided = FALSE))
  expect_no_error(pvalue(1, two_sided = FALSE, side = "left"))
  expect_no_error(pvalue(1, two_sided = FALSE, side = "right"))
  expect_error(pvalue(1, two_sided = FALSE, side = "aaa"))
})
