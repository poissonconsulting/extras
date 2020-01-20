context("deprecated")

test_that("check_function_template", {
  expect_null(check_function_template("1"))
  expect_invisible(check_function_template("1"))
})
