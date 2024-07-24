test_that("pextreme", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_identical(pextreme(numeric(0)), numeric(0))
  expect_identical(pextreme(NA_real_), NA_real_)
  expect_identical(pextreme(0.5), 1)
  expect_equal(
    pextreme(seq(0, 1, by = 0.1)),
    c(0, 0.2, 0.4, 0.6, 0.8, 1, 0.8, 0.6, 0.4, 0.2, 0)
  )
  expect_equal(
    pextreme(c(0, NA_real_)),
    c(0, NA_real_)
  )
})
