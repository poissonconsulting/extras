context("nsams")

test_that("nsams works", {
  expect_identical(nsams(1L), 2L)
  expect_identical(nsams(3:4), 4L)
})
