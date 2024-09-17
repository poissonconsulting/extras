test_that("phi gives expected values", {
  expect_identical(phi(0), 0.5)
  expect_equal(phi(2), 0.9772499, tolerance = 0.0000001)
  x <- seq(0, 1, by = 0.25)
  expect_equal(phi(0:2), c(0.5, 0.8413447, 0.9772499), tolerance = 0.0000001)
})
