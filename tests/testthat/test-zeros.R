test_that("zeros", {
  expect_identical(zeros(NA_integer_), NA_integer_)
  expect_identical(zeros(integer(0)), 0L)
  expect_equal(zeros(1), 0L)
  expect_equal(zeros(c(1, 1)), 0L)
  expect_equal(zeros(c(0, 0)), 2L)
  expect_equal(zeros(c(0, 1)), 1L)
  expect_equal(zeros(c(0, NA_real_)), NA_integer_)
})
