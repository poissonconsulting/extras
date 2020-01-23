context("npars")

test_that("npars works", {
  expect_identical(npars(character(0)), 0L)
  expect_identical(npars("a"), 1L)
})
