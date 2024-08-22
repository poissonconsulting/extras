test_that("ilogit works", {
  expect_equal(logit(0.5), 0)
  expect_equal(logit(1), Inf)
  x <- 0.75
  expect_equal(ilogit(logit(0.67)), 0.67)
  expect_equal(invlogit(logit(0.67)), 0.67)
  expect_equal(inv_logit(logit(0.67)), 0.67)
  logit(x) <- c(0.5, 1)
  expect_identical(x, ilogit(c(0.5, 1)))
  x <- seq(0, 1, by = 0.25)
  expect_identical(logit(x), qlogis(x))
})
