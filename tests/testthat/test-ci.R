test_that("xtr_ci() works with all CI types.", {
  expect_no_error(xtr_ci(1:100))
  expect_no_error(xtr_ci(1:100, type = "HDI"))
  expect_no_error(xtr_ci(1:100, type = "ETI"))
  expect_error(xtr_ci(1:100, type = ""), "`type` must match 'ETI' or 'HDI', not ''.")
  expect_error(xtr_ci(1:100, type = NA_character_), "`type` must be a string \\(non-missing character scalar\\).")
  expect_error(xtr_ci(1:100, type = 1), "`type` must be a string \\(non-missing character scalar\\).")
})
