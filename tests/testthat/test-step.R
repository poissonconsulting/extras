test_that("step works", {
  expect_identical(step(1), TRUE)
  expect_identical(step(0), TRUE)
  expect_identical(step(-1), FALSE)
  expect_identical(step(numeric(0)), logical(0))
  chk::expect_chk_error(step("A"))
  chk::expect_chk_error(step(character(0)))
  chk::expect_chk_error(step(NA))
})

