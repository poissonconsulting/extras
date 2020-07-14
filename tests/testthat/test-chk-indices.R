test_that("vld_indices", {
  expect_true(vld_indices(list()))
  expect_true(vld_indices(list(1L)))
  expect_true(vld_indices(list(1L, 2L)))
  expect_true(vld_indices(list(1:2, 3:2)))
  expect_false(vld_indices(1L))
  expect_false(vld_indices(list(1)))
  expect_false(vld_indices(list(-1L)))
  expect_false(vld_indices(list(NA_integer_)))
  expect_false(vld_indices(list(integer(0))))
})

test_that("chk_indices", {
  expect_null(chk_indices(list()))
  expect_error(chk_indices(1L), "^`1L` must be a list[.]$", class = "chk_error")
  expect_error(chk_indices(list(1)), "^All elements of `list[(]1[)]` must be integer[.]$", class = "chk_error")
})
