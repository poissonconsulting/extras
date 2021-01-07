test_that("as_list_unnamed works", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")
  expect_identical(as_list_unnamed(1:2), list(1L, 2L))
  expect_identical(as_list_unnamed(c(x = 1L, y = 2L)), list(x = 1L, y = 2L))
  expect_identical(as_list_unnamed(integer(0)), list())
})

test_that("as_list works", {
  rlang::scoped_options(lifecycle_verbosity = "quiet")
  expect_identical(as_list(1:2), list(1L, 2L))
  expect_identical(as_list(c(x = 1L, y = 2L)), list(x = 1L, y = 2L))
  expect_identical(as_list(integer(0)), list())
})
