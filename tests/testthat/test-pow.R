test_that("pow gives expected values", {
  expect_identical(pow(5, 2), 25)
  x <- seq(0, 1, by = 0.25)
  expect_identical(pow(3, 4), 3^4)
})
