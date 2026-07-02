test_that("xtr_ci_eti() returns the correct values.", {
  expect_equal(xtr_ci_eti(NA_real_), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(NA_integer_), data.frame(lower = NA_integer_, upper = NA_integer_))
  expect_equal(xtr_ci_eti(numeric(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(double(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(integer(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(c(NA_integer_, 1:2)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(c(NA_integer_, 1:2), na_rm = TRUE), data.frame(lower = 1.025, upper = 1.975))
  expect_error(xtr_ci_eti(1, 1, TRUE), "`...` must be unused.")

  expect_no_error(xtr_ci_eti(matrix(1:9, ncol = 3)))
  expect_no_error(xtr_ci_eti(array(1:27, dim = c(3, 3, 3))))
  expect_error(xtr_ci_eti(list(1)), "`x` must be numeric.")
  expect_error(xtr_ci_eti(data.frame(1)), "`x` must be numeric.")

  expect_error(xtr_ci_eti(1:100, level = 0), "level` must be greater than 0, not 0.")
  expect_no_error(xtr_ci_eti(1:100, level = 1))
  expect_error(xtr_ci_eti(1:100, level = 2), "level` must be between 0 and 1, not 2.")

  expect_equal(xtr_ci_eti(1:100, level = 1), data.frame(lower = 1, upper = 100))
  expect_equal(xtr_ci_eti(NA_integer_), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(c(0, rep(Inf, 19))), data.frame(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_eti(c(0, rep(-Inf, 19))), data.frame(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_eti(1:100), data.frame(lower = 1 + 2.475, upper = 100 - 2.475))
  expect_equal(xtr_ci_eti(seq(0, 1, length.out = 1e3)),
               data.frame(lower = 0.025, upper = 0.975))
  expect_equal(xtr_ci_eti(c(1:100, Inf)), data.frame(lower = 3.5, upper = 98.5))
  expect_equal(xtr_ci_eti(c(1:100, rep(Inf, 200))), data.frame(lower = 8.475, upper = Inf))
})
