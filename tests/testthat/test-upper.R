test_that("upper", {
  expect_identical(upper(NA_integer_), NA_real_)
  expect_identical(upper(integer(0)), NA_real_)
  expect_equal(upper(1), 1)
  expect_equal(upper(c(1, 1)), 1)
  expect_equal(upper(0:100), 97.5)
  expect_equal(upper(c(0:100, NA)), NA_real_)
  expect_equal(upper(c(0:100, NA), na_rm = TRUE), 97.5)
})
