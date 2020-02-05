context("zscore")

test_that("zscore", {
  expect_identical(zscore(NA_integer_), NA_real_)
  expect_identical(zscore(integer(0)), NA_real_)
  expect_equal(zscore(1), NA_real_)
  expect_equal(zscore(c(1, 1)), Inf)
  expect_equal(zscore(c(0, 0)), NA_real_)
  expect_equal(zscore(0), NA_real_)
  set.seed(101)
  expect_equal(zscore(rnorm(1000, 2)), 2.04891535423035)
})
