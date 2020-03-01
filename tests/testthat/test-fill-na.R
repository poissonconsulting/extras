test_that("fill_na atomic objects", {
  expect_identical(fill_na(numeric(0)), numeric(0))
  expect_identical(fill_na(integer(0)), integer(0))
  expect_identical(fill_na(NA), FALSE)
  expect_identical(fill_na(c(10L, NA)), c(10L, 0L))
  expect_identical(
    fill_na(matrix(c(1L, 3L, 7L, NA), nrow = 2)),
    matrix(c(1L, 3L, 7L, 0L), nrow = 2)
  )
  expect_identical(fill_na(c(10L, NA), value = 11L), c(10L, 11L))
  expect_identical(fill_na(c(10L, NA), value = 11), c(10L, 11L))
})

test_that("fill_na.matrix", {
  expect_identical(
    fill_na(matrix(c(TRUE, NA, FALSE, NA), nrow = 2),
      value = "TRUE"
    ),
    matrix(c(TRUE, TRUE, FALSE, TRUE), nrow = 2)
  )
  expect_identical(fill_na(matrix(NA_integer_)), matrix(0L))
})

test_that("fill_na.character", {
  expect_identical(fill_na(c("a", NA)), c("a", "0"))
})
