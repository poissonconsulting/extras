test_that("chk_natomic", {
  expect_error(chk_natomic(list()),
    "^`list[(][)]` must be a numeric [(]integer or double[)] atomic [(]vector, matrix or array[)] object[.]",
    class = "chk_error"
  )
  expect_error(chk_natomic(NULL),
    "^`NULL` must be a numeric [(]integer or double[)] atomic [(]vector, matrix or array[)] object[.]",
    class = "chk_error"
  )
  expect_error(chk_natomic(NULL, x_name = "nine"),
    "^Nine must be a numeric [(]integer or double[)] atomic [(]vector, matrix or array[)] object[.]",
    class = "chk_error"
  )
  expect_false(vld_natomic(NULL))
  expect_null(chk_natomic(1))
  expect_invisible(chk_natomic(1))
  expect_true(vld_natomic(1))
  expect_true(vld_natomic(matrix(1)))
  expect_true(vld_natomic(array(1)))
})
