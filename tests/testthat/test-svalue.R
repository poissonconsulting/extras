test_that("svalue", {
  expect_identical(svalue(integer(0), side = "left"), NA_real_)
  expect_identical(svalue(integer(0), side = "right"), NA_real_)
  expect_identical(svalue(1, side = "left"), 1)
  expect_identical(svalue(1, side = "right"), 0)
  expect_identical(svalue(-1, side = "left"), 0)
  expect_identical(svalue(-1, side = "right"), 1)
  expect_equal(svalue(c(1, 1), side = "left"), -log2(1 / 3))
  expect_identical(svalue(c(1, 1), side = "right"), 0)
  expect_identical(svalue(-c(1, 1), side = "left"), 0)
  expect_equal(svalue(-c(1, 1), side = "right"), -log2(1 / 3))
  expect_identical(svalue(0, side = "left"), 0)
  expect_identical(svalue(0, side = "right"), 0)
  expect_equal(
    svalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "left"),
    -log2(25 / 1000)
  )
  expect_equal(
    svalue(c(rep(-1, 25), rep(1, 1000 - 25)), side = "right"),
    -log2(975 / 1000)
  )
  expect_equal(
    svalue(c(rep(1, 25), rep(-1, 1000 - 25)), side = "left"),
    -log2(975 / 1000)
  )
  expect_equal(
    svalue(c(rep(1, 25), rep(-1, 1000 - 25)), side = "right"),
    -log2(25 / 1000)
  )
  expect_equal(svalue(-9:10, side = "left"), -log2(10 / 20))
  expect_equal(svalue(-9:10, side = "right"), -log2(11 / 20))
  expect_equal(svalue(-9:10 + 1, side = "left", threshold = 1), -log2(10 / 20))
  expect_equal(svalue(-9:10 + 1, side = "right", threshold = 1), -log2(11 / 20))
  expect_identical(svalue(c(1, 1, NA), side = "left"), NA_real_)
  expect_equal(svalue(c(1, 1, NA), side = "left", na_rm = TRUE), -log2(1 / 3))
  expect_identical(svalue(-c(1, 1, NA), side = "right"), NA_real_)
  expect_equal(svalue(-c(1, 1, NA), side = "right", na_rm = TRUE), -log2(1 / 3))
})

test_that("svalue() requires side to be one of left, right, both, or NULL, and returns a warning with default side.", {
  expect_no_error(pvalue(1, side = "both"))
  expect_no_error(pvalue(1, side = "left"))
  expect_no_error(pvalue(1, side = "right"))
  expect_error(
    pvalue(1, side = "aaa"),
    "`side` must match 'both', 'left' or 'right', not 'aaa'."
  )
})

test_that("p2svalue() returns the correct values", {
  expect_equal(p2svalue(NA_real_), NA_real_)
  expect_equal(p2svalue(NA_integer_), NA_real_)
  expect_equal(p2svalue(1), 0)
  expect_equal(
    p2svalue(1 - .Machine$double.eps),
    -log2(1 - .Machine$double.eps)
  )
  p <- runif(1e4)
  expect_equal(p2svalue(p), -log2(p))
  expect_equal(p2svalue(.Machine$double.eps), -log2(.Machine$double.eps))
  expect_equal(p2svalue(0), Inf)
})
