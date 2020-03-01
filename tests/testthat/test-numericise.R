test_that("numericize", {
  expect_identical(numericize(TRUE), 1L)
})

test_that("numericise.logical", {
  expect_identical(numericise(logical(0)), integer(0))
  expect_identical(numericise(TRUE), 1L)
})

test_that("numericise.integer", {
  expect_identical(numericise(2L), 2L)
})

test_that("numericise.double", {
  expect_identical(numericise(2), 2)
})

test_that("numericise.factor", {
  expect_identical(numericise(factor(3:2)), c(2L, 1L))
})

test_that("numericise.character", {
  expect_error(numericise("1"))
})

test_that("numericise.Date", {
  expect_identical(numericise(as.Date("1970-01-02")), 1)
  x <- 10.9
  class(x) <- "Date"
  expect_identical(numericise(x), 10.9)
})

test_that("numericise.POSIXct", {
  expect_identical(numericise(as.POSIXct("1970-01-02", tz = "UTC")), 86400)
})

test_that("numericise.matrix", {
  expect_identical(numericise(matrix(NA)), matrix(NA_integer_))
  expect_identical(
    numericise(matrix(NA, c(2, 3, 4))),
    matrix(NA_integer_, c(2, 3, 4))
  )
  expect_identical(numericise(matrix(NA_real_)), matrix(NA_real_))
})

test_that("numericise.array", {
  expect_identical(numericise(array(1:12, c(1, 3, 4))), array(1:12, c(1, 3, 4)))
  expect_identical(numericise(array(TRUE, c(1, 1, 1))), array(1L, c(1, 1, 1)))
})

test_that("numericise.list", {
  expect_error(
    numericise(list(x = TRUE))
  )

  expect_error(
    numericise(list(
      lgl = c(TRUE, NA),
      int = 1:2,
      dbl = c(2.5, 1.5),
      dte = as.Date(c("2001-01-02", "2001-01-01")),
      fac = factor(c("b", "a"))
    )))
})

test_that("numericise.data.frame", {
  expect_identical(
    numericise(data.frame(x = TRUE)),
    structure(1L, .Dim = c(1L, 1L), .Dimnames = list(NULL, "x"))
  )
  expect_error(
    numericise(data.frame(
      x = factor(3:2), y = c(FALSE, NA), z = c("1", "2"),
      stringsAsFactors = FALSE
    ))
  )

  expect_identical(
    numericise(data.frame(
      lgl = c(TRUE, NA),
      int = 1:2,
      dbl = c(2.5, 1.5),
      dte = as.Date(c("2001-01-02", "2001-01-01")),
      fac = factor(c("b", "a"))
    )),
    structure(c(1, NA, 1, 2, 2.5, 1.5, 11324, 11323, 2, 1), .Dim = c(2L,
5L), .Dimnames = list(NULL, c("lgl", "int", "dbl", "dte", "fac"
))))
})

test_that("numericise.hms", {
  x <- structure(10.9, class = c("hms", "difftime"), units = "secs")
  expect_identical(numericise(x), 10.9)
})

