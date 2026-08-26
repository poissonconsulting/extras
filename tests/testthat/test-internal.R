test_that("chk_multinom_group singleton group errors", {
  expect_error(
    chk_multinom_group(size = 10, prob = 1, group = 1),
    "must contain at least 2 rows"
  )
})

test_that("chk_multinom_group size check ignores NA but catches known mismatches", {
  expect_no_error(chk_multinom_group(c(10, 10, NA), c(0.2, 0.3, 0.5), c(1, 1, 1)))
  expect_error(
    chk_multinom_group(c(10, 20, NA), c(0.2, 0.3, 0.5), c(1, 1, 1)),
    "`size` must be the same for every row belonging to the same `group`"
  )
})

test_that("chk_multinom_group prob check ignores NA but still catches known values that already break the sum", {
  # a lone NA prob shouldn't skip validation of the other, already-known values
  expect_error(
    chk_multinom_group(c(10, 10, 10), c(0.9, 0.9, NA), c(1, 1, 1)),
    "`prob` must sum to 1 for every `group`"
  )
  # known values that don't yet exceed 1 are fine to leave for the NA to complete
  expect_no_error(chk_multinom_group(c(10, 10, 10), c(0.4, 0.3, NA), c(1, 1, 1)))
  # a fully-known group must still sum to exactly 1
  expect_no_error(chk_multinom_group(c(10, 10, 10), c(0.2, 0.3, 0.5), c(1, 1, 1)))
  expect_error(
    chk_multinom_group(c(10, 10, 10), c(0.2, 0.3, 0.4), c(1, 1, 1)),
    "`prob` must sum to 1 for every `group`"
  )
})

test_that("chk_multinom_group modal row count check", {
  expect_no_error(
    chk_multinom_group(
      c(10, 10, 10, 6, 6, 6),
      c(0.2, 0.3, 0.5, 0.2, 0.3, 0.5),
      c(1, 1, 1, 2, 2, 2)
    )
  )
  expect_error(
    chk_multinom_group(
      c(10, 10, 10, 6, 6),
      c(0.2, 0.3, 0.5, 0.5, 0.5),
      c(1, 1, 1, 2, 2)
    ),
    "Every `group` should have the same number of rows"
  )
})

test_that("chk_multinom_group breaks an exact row-count tie toward the smaller size", {
  # 2 groups of 2 rows, 2 groups of 3 rows -- documented to prefer the
  # smaller count (table()'s ascending sort + which.max()'s first-match)
  expect_error(
    chk_multinom_group(
      c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
      c(0.5, 0.5, 0.2, 0.3, 0.5, 0.5, 0.5, 0.2, 0.3, 0.5),
      c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4)
    ),
    "should have the same number of rows \\(2,"
  )
})

test_that("multinom_row_na flags a whole group when any size/prob in it is NA", {
  expect_identical(
    multinom_row_na(c(10, NA, 10, 10), c(0.2, 0.8, 0.4, 0.6), c(1, 1, 2, 2)),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    multinom_row_na(c(10, 10), c(0.2, 0.8), c(1, 1)),
    c(FALSE, FALSE)
  )
})
