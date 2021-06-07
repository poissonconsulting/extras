test_that("odds errors incorrect inputs", {
  chk::expect_chk_error(odds(NA))
  chk::expect_chk_error(odds("1"))
  chk::expect_chk_error(odds(-0.1))
  chk::expect_chk_error(odds(1.1))
})

test_that("odds handles edge cases", {
  expect_identical(odds(numeric(0)), numeric(0))
  expect_identical(odds(NA_real_), NA_real_)
  expect_equal(odds(matrix(0.9)), matrix(9))
  expect_equal(odds(array(0.9)), array(9))
})

test_that("odds works scalars", {
  expect_equal(odds(0), 0)
  expect_equal(odds(0.1), 1/9)
  expect_equal(odds(1/6), 1/5)
  expect_equal(odds(0.5), 1)
  expect_equal(odds(0.9), 9)
  expect_equal(odds(1), Inf)
})

test_that("odds works vector", {
  expect_equal(odds(c(0, 0.1, 0.5, 0.9, NA, 1)),
               c(0, 1/9, 1, 9, NA, Inf))
})
