test_that("nsims works", {
  expect_identical(nsims(1L), 2L)
  expect_identical(nsims(3:4), 2L)
})
