test_that("npars_scalar works", {
  expect_identical(npars_scalar(character(0)), 0L)
  expect_identical(npars_scalar("scalar"), 1L)
})
