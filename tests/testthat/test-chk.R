context("chk")

test_that("chk_function template", {
  expect_null(chk_function_template(""))
  expect_invisible(chk_function_template(""))
  expect_error(chk_function_template(1), "^`1` must be a character[.]$",
    class = "chk_error"
  )
})
