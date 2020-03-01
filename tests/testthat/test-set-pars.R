test_that("set_pars works", {
  x <- "a"
  pars(x) <- "b"
  expect_identical(x, "b")
})
