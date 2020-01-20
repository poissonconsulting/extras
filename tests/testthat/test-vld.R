context("vld")

test_that("vld_function_template", {
  expect_true(vld_function_template(character(0)))
  expect_true(vld_function_template(NA_character_))
  expect_true(vld_function_template(""))
  expect_true(vld_function_template("1"))
  expect_false(vld_function_template(1))
})
