test_that("xtr_ci() works with all CI types.", {
  expect_no_error(xtr_ci(1:100))
  expect_no_error(xtr_ci(1:100, type = "HDI"))
  expect_no_error(xtr_ci(1:100, type = "ETI"))
  expect_no_error(xtr_ci(1:100, type = "normal"))
})
