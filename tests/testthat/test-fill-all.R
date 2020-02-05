context("fill-all")

test_that("fill_all atomic objects", {
  expect_identical(fill_all(numeric(0)), numeric(0))
  expect_identical(fill_all(numeric(0), nas = FALSE), numeric(0))
  expect_identical(fill_all(integer(0)), integer(0))
  expect_identical(fill_all(integer(0), nas = FALSE), integer(0))
  expect_identical(fill_all(NA), FALSE)
  expect_identical(fill_all(NA, nas = FALSE), NA)
  expect_identical(fill_all(c(10L, NA)), c(0L, 0L))
  expect_identical(fill_all(c(10L, NA), nas = FALSE), c(0L, NA))
  expect_identical(
    fill_all(matrix(c(1L, 3L, 7L, NA), nrow = 2, ), value = 2L),
    matrix(c(2L, 2L, 2L, 2L), nrow = 2)
  )
  expect_identical(
    fill_all(matrix(c(1L, 3L, 7L, NA), nrow = 2), nas = FALSE),
    matrix(c(0L, 0L, 0L, NA), nrow = 2)
  )
  expect_identical(fill_all(c(10L, NA), value = 11L), c(11L, 11L))
  expect_identical(fill_all(c(10L, NA),
    value = 11,
    nas = FALSE
  ), c(11L, NA))
})

test_that("fill_all.matrix", {
  expect_identical(
    fill_all(matrix(c(TRUE, NA, FALSE, NA), nrow = 2),
      value = "TRUE"
    ),
    matrix(c(TRUE, TRUE, TRUE, TRUE), nrow = 2)
  )
  expect_identical(
    fill_all(matrix(c(TRUE, NA, FALSE, NA), nrow = 2),
      value = "TRUE", nas = FALSE
    ),
    matrix(c(TRUE, NA, TRUE, NA), nrow = 2)
  )
  expect_identical(fill_all(matrix(NA_integer_)), matrix(0L))
})

test_that("fill_all.character", {
  expect_identical(fill_all(c("a", NA)), c("0", "0"))
  expect_identical(fill_all(c("a", NA), nas = FALSE), c("0", NA))
})
