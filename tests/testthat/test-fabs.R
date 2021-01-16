test_that("fabs", {
  expect_identical(fabs(numeric(0)), numeric(0))
  expect_identical(fabs(NA), NA_integer_)
  expect_identical(fabs(-1), 1)
  expect_identical(fabs(c(-1.3, 2.777)), c(1.3, 2.777))
  expect_identical(fabs(-1:2), c(1L, 0L, 1L, 2L))
})
