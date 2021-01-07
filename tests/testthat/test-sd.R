test_that("xtr_sd", {
  expect_identical(xtr_sd(NA_integer_), NA_real_)
  expect_identical(xtr_sd(integer(0)), NA_real_)
  expect_equal(xtr_sd(1), NA_real_)
  expect_equal(xtr_sd(c(1, 1)), 0)
  expect_equal(xtr_sd(c(1, 2)), 0.707106781186548)
  expect_equal(xtr_sd(c(1, 2, NA_real_)), NA_real_)
  expect_equal(xtr_sd(c(1, 2, NA_real_), na_rm = TRUE), 0.707106781186548)
})
