test_that("multiplication works", {
  expect_identical(par_pattern(TRUE, TRUE), "^[[:alpha:]][[:alnum:]._]*$")
  expect_identical(par_pattern(), "[[:alpha:]][[:alnum:]._]*")
  expect_identical(par_pattern(ht = TRUE), "^[[:alpha:]][[:alnum:]._]*")
  expect_identical(par_pattern(ds = TRUE), "[[:alpha:]][[:alnum:]._]*$")
})
