test_that("lower", {
  expect_identical(lower(NA_integer_), NA_real_)
  expect_identical(lower(integer(0)), NA_real_)
  expect_equal(lower(1), 1)
  expect_equal(lower(c(1, 1)), 1)
  expect_equal(lower(0:100), 2.5)
  expect_equal(lower(c(0:100, NA)), NA_real_)
  expect_equal(lower(c(0:100, NA), na_rm = TRUE), 2.5)
})
