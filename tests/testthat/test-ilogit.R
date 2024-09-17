test_that("logit works", {
  x <- NA
  logit(x) <- logit(0.75)
  expect_equal(x, 0.75)
  x <- seq(0, 1, by = 0.25)
  expect_identical(ilogit(logit(x)), x)
  expect_identical(invlogit(logit(x)), x)
  expect_identical(inv_logit(logit(x)), x)
})
