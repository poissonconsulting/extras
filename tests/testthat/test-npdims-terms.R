context("npdims-terms")

test_that("npdims_terms works", {
  expect_identical(npdims_terms("dummy"), c(scalar = 2L, vector = 1L))
  expect_identical(npdims_terms("dummy", scalar = TRUE), c(scalar = 2L))
  expect_error(npdims_terms("dummy", scalar2 = FALSE), "`...` must be unused.",
               class = "chk_error")
})
