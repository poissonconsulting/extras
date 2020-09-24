test_that("as_list_unnamed works", {
  expect_identical(as_list_unnamed(1:2), list(1L, 2L))
  expect_identical(as_list_unnamed(c(x = 1L, y = 2L)), list(x = 1L, y = 2L))
  expect_identical(as_list_unnamed(integer(0)), list())
})
