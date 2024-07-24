test_that("proportional_change errors incorrect inputs", {
  chk::expect_chk_error(proportional_change(NA, 0.5))
  chk::expect_chk_error(proportional_change("1", 0.5))
  chk::expect_chk_error(proportional_change(-0.1, 0.5))
})

test_that("proportional_change handles edge cases", {
  expect_identical(proportional_change(numeric(0), numeric(0)), numeric(0))
  expect_identical(proportional_change(NA_real_, 0.5), NA_real_)
  expect_identical(proportional_change(0.5, NA_real_), NA_real_)
})

test_that("proportional_change works scalars", {
  expect_equal(proportional_change(0, 0.5), Inf)
  expect_equal(proportional_change(0.1, 0.1), 0)
  expect_equal(proportional_change(0.5, 0.9), 0.8)
  expect_equal(proportional_change(1, 0.5), -0.5)
  expect_equal(proportional_change(0.5, 1), 1)
})

test_that("proportional_change works vector", {
  expect_equal(proportional_change(c(0.1, 0.5), c(0.5, 0.1)), c(4, -0.8))
})

test_that("proportional_change works matrix", {
  expect_equal(proportional_change(matrix(0.9), matrix(0.9)), matrix(0))
})

test_that("proportional_change works array", {
  expect_equal(proportional_change(array(0.9), array(0.9)), array(0))
})

test_that("odds_ratio errors incorrect inputs", {
  chk::expect_chk_error(proportional_difference(NA, 0.5))
  chk::expect_chk_error(proportional_difference("1", 0.5))
  chk::expect_chk_error(proportional_difference(-0.1, 0.5))
})

test_that("proportional_difference handles edge cases", {
  expect_identical(proportional_difference(numeric(0), numeric(0)), numeric(0))
  expect_identical(proportional_difference(NA_real_, 0.5), NA_real_)
  expect_identical(proportional_difference(0.5, NA_real_), NA_real_)
})

test_that("proportional_difference works scalars", {
  expect_equal(proportional_difference(0, 0.5), 2)
  expect_equal(proportional_difference(0.1, 0.1), 0)
  expect_equal(proportional_difference(1, 0.5), -2 / 3)
  expect_equal(proportional_difference(0.5, 1), 2 / 3)
})

test_that("proportional_difference works vector", {
  expect_equal(proportional_difference(c(1, 0.5), c(0.5, 1)), c(-2 / 3, 2 / 3))
})

test_that("proportional_difference works matrix", {
  expect_equal(proportional_difference(matrix(0.9), matrix(0.9)), matrix(0))
})

test_that("proportional_difference works array", {
  expect_equal(proportional_difference(array(0.9), array(0.9)), array(0))
})

test_that("proportional_change2 works", {
  expect_equal(proportional_change2(c(0.1, 0.5)), 4)
  expect_equal(proportional_change2(c(0.5, 0.1)), -0.8)
})

test_that("proportional_difference2 works", {
  expect_equal(proportional_difference2(c(0.1, 0.5)), 4 / 3)
  expect_equal(proportional_difference2(c(0.5, 0.1)), -4 / 3)
})
