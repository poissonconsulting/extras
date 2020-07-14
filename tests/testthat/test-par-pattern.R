test_that("multiplication works", {
  expect_identical(par_pattern(), "[[:alpha:]][[:alnum:]._]*")
})
