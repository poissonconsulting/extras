test_that("npdims works", {
  expect_identical(npdims("dummy"), c(scalar = 2L, vector = 1L))
  expect_error(npdims("dummy", scalar2 = FALSE), "`...` must be unused.",
    class = "chk_error"
  )
})
