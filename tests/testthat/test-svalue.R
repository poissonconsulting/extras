test_that("svalue", {
  expect_identical(svalue(integer(0), side = "left", skeptical = TRUE), NA_real_)
  expect_identical(svalue(integer(0), side = "right", skeptical = TRUE), NA_real_)
  expect_identical(svalue(1, side = "left", skeptical = TRUE), 1)
  expect_identical(svalue(1, side = "right", skeptical = TRUE), 0)
  expect_identical(svalue(-1, side = "left", skeptical = TRUE), 0)
  expect_identical(svalue(-1, side = "right", skeptical = TRUE), 1)
  expect_equal(svalue(c(1, 1), side = "left", skeptical = TRUE), -log2(1/3))
  expect_identical(svalue(c(1, 1), side = "right", skeptical = TRUE), 0)
  expect_identical(svalue(-c(1, 1), side = "left", skeptical = TRUE), 0)
  expect_equal(svalue(-c(1, 1), side = "right", skeptical = TRUE), -log2(1/3))
  expect_identical(svalue(0, side = "left", skeptical = TRUE), 0)
  expect_identical(svalue(0, side = "right", skeptical = TRUE), 0)
  expect_equal(svalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "left", skeptical = TRUE), -log2(25 / 1000))
  expect_equal(svalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "right", skeptical = TRUE), -log2(975 / 1000))
  expect_equal(svalue(c(rep(1, 25), rep(-1, 1000 - 25)), side = "left", skeptical = TRUE), -log2(975 / 1000))
  expect_equal(svalue(c(rep(1, 25), rep(-1, 1000 - 25)), side = "right", skeptical = TRUE), -log2(25 / 1000))
  expect_equal(svalue(-9:10, side = "left", skeptical = TRUE), -log2(10 / 20))
  expect_equal(svalue(-9:10, side = "right", skeptical = TRUE), -log2(11 / 20))
  expect_equal(svalue(-9:10 + 1, side = "left", threshold = 1, skeptical = TRUE), -log2(10 / 20))
  expect_equal(svalue(-9:10 + 1, side = "right", threshold = 1, skeptical = TRUE), -log2(11 / 20))
  expect_identical(svalue(c(1, 1, NA), side = "left", skeptical = TRUE), NA_real_)
  expect_equal(svalue(c(1, 1, NA), side = "left", na_rm = TRUE, skeptical = TRUE), -log2(1 / 3))
  expect_identical(svalue(-c(1, 1, NA), side = "right", skeptical = TRUE), NA_real_)
  expect_equal(svalue(-c(1, 1, NA), side = "right", na_rm = TRUE, skeptical = TRUE), -log2(1 / 3))
})

test_that("svalue() errors with unused arguments", {
  expect_error(svalue(1, 0), "`...` must be unused.")
  expect_error(svalue(1, foo = TRUE), "`...` must be unused.")
})

test_that("svalue() skeptical argument controls sample-size correction", {
  expect_identical(svalue(1:9, skeptical = TRUE), -log2(1 / 10))
  expect_identical(svalue(1:9, skeptical = FALSE), Inf)
  expect_identical(svalue(-(1:9), skeptical = TRUE), -log2(1 / 10))
  expect_identical(svalue(-(1:9), skeptical = FALSE), Inf)
  expect_identical(svalue(c(-1, 1, 1), skeptical = TRUE),
                   svalue(c(-1, 1, 1), skeptical = FALSE))
  expect_identical(svalue(c(-1, 1, 1), side = "left", skeptical = TRUE),
                   svalue(c(-1, 1, 1), side = "left", skeptical = FALSE))
  expect_identical(svalue(c(-1, 1, 1), side = "right", skeptical = TRUE),
                   svalue(c(-1, 1, 1), side = "right", skeptical = FALSE))
})

test_that("svalue() requires side to be one of left, right, both, or NULL, and returns a warning with default side.", {
  expect_no_error(svalue(1, side = "both", skeptical = TRUE))
  expect_no_error(svalue(1, side = "left", skeptical = TRUE))
  expect_no_error(svalue(1, side = "right", skeptical = TRUE))
  expect_error(svalue(1, side = "aaa", skeptical = TRUE),
               "`side` must match 'both', 'left' or 'right', not 'aaa'.")
})

test_that("p2svalue() returns the correct values", {
  expect_equal(p2svalue(NA_real_), NA_real_)
  expect_equal(p2svalue(NA_integer_), NA_real_)
  expect_equal(p2svalue(1), 0)
  expect_equal(p2svalue(1 - .Machine$double.eps), -log2(1 - .Machine$double.eps))
  p <- runif(1e4)
  expect_equal(p2svalue(p), -log2(p))
  expect_equal(p2svalue(.Machine$double.eps), -log2(.Machine$double.eps))
  expect_equal(p2svalue(0), Inf)
})
