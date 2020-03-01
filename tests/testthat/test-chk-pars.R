test_that("chk_pars", {
  expect_null(chk_pars(character(0)))
  expect_invisible(chk_pars(character(0)))
  expect_null(chk_pars("a"))
  expect_null(chk_pars(c("a", "A")))
  expect_null(chk_pars("a1._"))

  expect_error(chk_pars(NA_character_),
    "^`NA_character_` must not have any missing values[.]$",
    class = "chk_error"
  )
  x <- c("a", "a")
  expect_error(chk_pars(x),
    "^`x` must be unique[.]$",
    class = "chk_error"
  )
  x <- factor("a")
  expect_error(chk_pars(x),
    "^`x` must inherit from S3 class 'character'[.]$",
    class = "chk_error"
  )
  x <- ".1"
  expect_error(chk_pars("."),
    "^`\".\"` must match regular expression",
    class = "chk_error"
  )
})


test_that("vld_pars", {
  expect_true(vld_pars(character(0)))
  expect_false(vld_pars(factor(0)))
  expect_false(vld_pars(NA_character_))
  expect_true(vld_pars("a"))
  expect_false(vld_pars(c("a", "a")))
  expect_true(vld_pars(c("a", "A")))
  expect_false(vld_pars("a[1]"))
  expect_true(vld_pars("a1._"))
  expect_false(vld_pars(".a"))
  expect_false(vld_pars("_a"))
  expect_false(vld_pars("1a"))
})

