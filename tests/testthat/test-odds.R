test_that("odds errors incorrect inputs", {
  chk::expect_chk_error(odds(NA))
  chk::expect_chk_error(odds("1"))
  chk::expect_chk_error(odds(-0.1))
  chk::expect_chk_error(odds(1.1))
})

test_that("odds handles edge cases", {
  expect_identical(odds(numeric(0)), numeric(0))
  expect_identical(odds(NA_real_), NA_real_)
})

test_that("odds works individual numbers", {
  expect_equal(odds(0), 0)
  expect_equal(odds(0.1), 1/9)
  expect_equal(odds(1/6), 1/5)
  expect_equal(odds(0.5), 1)
  expect_equal(odds(0.9), 9)
  expect_equal(odds(1), Inf)
})

test_that("odds works vector", {
  expect_equal(odds(c(0, 0.5, 1)), c(0, 1, Inf))
})

test_that("odds works matrix", {
  expect_equal(odds(matrix(0.9)), matrix(9))
})

test_that("odds works array", {
  expect_equal(odds(array(0.9)), array(9))
})

test_that("odds_ratio errors incorrect inputs", {
  chk::expect_chk_error(odds_ratio(NA, 0.5))
  chk::expect_chk_error(odds_ratio("1", 0.5))
  chk::expect_chk_error(odds_ratio(-0.1, 0.5))
  chk::expect_chk_error(odds_ratio(1.1, 0.5))
})

test_that("odds_ratio handles edge cases", {
  expect_identical(odds_ratio(numeric(0), numeric(0)), numeric(0))
  expect_identical(odds_ratio(NA_real_, 0.5), NA_real_)
  expect_identical(odds_ratio(0.5, NA_real_), NA_real_)
})

test_that("odds_ratio works scalars", {
  expect_equal(odds_ratio(0, 0.5), 0)
  expect_equal(odds_ratio(0.1, 0.1), 1)
  expect_equal(odds_ratio(0.5, 0.9), 1/9)
  expect_equal(odds_ratio(0.9, 0.5), 9)
  expect_equal(odds_ratio(1, 0.5), Inf)
  expect_equal(odds_ratio(0.5, 1), 0)
})

test_that("odds_ratio works vector", {
  expect_equal(odds_ratio(c(0.1, 0.5), c(0.5, 0.1)), c(1/9,9))
})

test_that("odds_ratio works matrix", {
  expect_equal(odds_ratio(matrix(0.9), matrix(0.9)), matrix(1))
})

test_that("odds_ratio works array", {
  expect_equal(odds_ratio(array(0.9), array(0.9)), array(1))
})

test_that("odds_ratio errors incorrect inputs", {
  chk::expect_chk_error(log_odds_ratio(NA, 0.5))
  chk::expect_chk_error(log_odds_ratio("1", 0.5))
  chk::expect_chk_error(log_odds_ratio(-0.1, 0.5))
  chk::expect_chk_error(log_odds_ratio(1.1, 0.5))
})

test_that("log_odds_ratio handles edge cases", {
  expect_identical(log_odds_ratio(numeric(0), numeric(0)), numeric(0))
  expect_identical(log_odds_ratio(NA_real_, 0.5), NA_real_)
  expect_identical(log_odds_ratio(0.5, NA_real_), NA_real_)
})

test_that("log_odds_ratio works scalars", {
  expect_equal(log_odds_ratio(0, 0.5), -Inf)
  expect_equal(log_odds_ratio(0.1, 0.1), 0)
  expect_equal(log_odds_ratio(0.5, 0.9), log(1/9))
  expect_equal(log_odds_ratio(0.9, 0.5), log(9))
  expect_equal(log_odds_ratio(1, 0.5), Inf)
  expect_equal(log_odds_ratio(0.5, 1), -Inf)
})

test_that("log_odds_ratio works vector", {
  expect_equal(log_odds_ratio(c(0.1, 0.5), c(0.5, 0.1)), log(c(1/9,9)))
})

test_that("log_odds_ratio works matrix", {
  expect_equal(log_odds_ratio(matrix(0.9), matrix(0.9)), matrix(0))
})

test_that("log_odds_ratio works array", {
  expect_equal(log_odds_ratio(array(0.9), array(0.9)), array(0))
})

test_that("odds_ratio2 works", {
  expect_equal(odds_ratio2(c(0.1, 0.5)), 1/9)
  expect_equal(odds_ratio2(c(0.5, 0.1)), 9)
})

test_that("log_odds_ratio2 works", {
  expect_equal(log_odds_ratio2(c(0.1, 0.5)), log(1/9))
  expect_equal(log_odds_ratio2(c(0.5, 0.1)), log(9))
})
