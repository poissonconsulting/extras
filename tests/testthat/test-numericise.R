context("numericise")

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
  expect_identical(numericise("1"), 1)
})

test_that("numericise.Date", {
  expect_identical(numericise(as.Date("1970-01-02")), 1L)
  x <- 10.9
  class(x) <- "Date"
  expect_identical(numericise(x), 10L)
})

test_that("numericise.POSIXct", {
  expect_identical(numericise(as.POSIXct("1970-01-02", tz = "UTC")), 86400L)
})

test_that("numericise.hms", {
  x <- structure(10.9, class = c("hms", "difftime"), units = "secs")
  expect_identical(numericise(x), 10L)
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
  expect_identical(
    numericise(list(x = TRUE)),
    list(x = 1L)
  )
  expect_identical(
    numericise(list(x = TRUE, y = list(z = factor(3)))),
    list(x = 1L, y = list(z = 1L))
  )

  expect_identical(
    numericise(list(
      lgl = c(TRUE, NA),
      int = 1:2,
      dbl = c(2.5, 1.5),
      dte = as.Date(c("2001-01-02", "2001-01-01")),
      fac = factor(c("b", "a"))
    )),
    list(
      lgl = c(1L, NA), int = 1:2, dbl = c(2.5, 1.5),
      dte = 11324:11323,
      fac = 2:1
    )
  )
})

test_that("numericise.data.frame", {
  expect_identical(
    numericise(data.frame(x = TRUE)),
    data.frame(x = 1L)
  )
  expect_identical(
    numericise(data.frame(
      x = factor(3:2), y = c(FALSE, NA), z = c("1", "2"),
      stringsAsFactors = FALSE
    )),
    data.frame(x = 2:1, y = c(0L, NA), z = c(1, 2), stringsAsFactors = FALSE)
  )

  expect_identical(
    numericise(data.frame(
      lgl = c(TRUE, NA),
      int = 1:2,
      dbl = c(2.5, 1.5),
      dte = as.Date(c("2001-01-02", "2001-01-01")),
      fac = factor(c("b", "a"))
    )),
    structure(list(
      lgl = c(1L, NA), int = 1:2, dbl = c(2.5, 1.5),
      dte = 11324:11323, fac = 2:1
    ), row.names = c(NA, -2L), class = "data.frame")
  )
})
