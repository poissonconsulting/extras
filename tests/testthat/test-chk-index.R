test_that("vld_index", {
  expect_true(vld_index(1L))
  expect_true(vld_index(2:1))
  expect_false(vld_index(integer(0)))
  expect_false(vld_index(0L))
  expect_false(vld_index(NA_integer_))
  expect_false(vld_index(1))
})

test_that("chk_index", {
  expect_null(chk_index(1L))
  expect_error(chk_index(0L),
    "^`0L` must be greater than 0, not 0[.]$",
    class = "chk_error"
  )
})
