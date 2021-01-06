test_that("xtr_mean", {
  expect_identical(xtr_mean(NA_integer_), NA_real_)
  expect_identical(xtr_mean(integer(0)), NA_real_)
  expect_equal(xtr_mean(1), 1)
  expect_equal(xtr_mean(c(1, 1)), 1)
  expect_equal(xtr_mean(c(1, 2)), 1.5)
  expect_equal(xtr_mean(c(1, 2, NA_real_)), NA_real_)
  expect_equal(xtr_mean(c(1, 2, NA_real_), na_rm = TRUE), 1.5)
})
