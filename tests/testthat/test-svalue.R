context("svalue")

test_that("svalue", {
  expect_identical(svalue(NA_integer_), NA_real_)
  expect_identical(svalue(integer(0)), NA_real_)
  expect_equal(svalue(1), 1)
  expect_equal(svalue(c(1, 1)), 1.58496250072116)
  expect_equal(svalue(0), 0)
  expect_equal(svalue(c(rep(-1, 25), rep(1, 1000 - 25))), 4.2948009168645)
})
