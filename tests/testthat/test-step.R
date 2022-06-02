test_that("step works", {
  expect_identical(step(1), TRUE)
  expect_identical(step(0), TRUE)
  expect_identical(step(-1), FALSE)
  expect_identical(step(numeric(0)), logical(0))
})

test_that("step errors incorrect inputs", {
  chk::expect_chk_error(step("A"))
  chk::expect_chk_error(step(character(0)))
  chk::expect_chk_error(step(NA))
})

test_that("step works vector", {
  expect_identical(step(c(0, 1, -1)), c(TRUE, TRUE, FALSE))
})

test_that("step works matrix", {
  expect_identical(step(matrix(0)), matrix(TRUE))
  expect_identical(step(matrix(-2)), matrix(FALSE))
})

test_that("step works array", {
  expect_equal(step(array(0)), array(TRUE))
  expect_identical(step(array(-2)), array(FALSE))
})

