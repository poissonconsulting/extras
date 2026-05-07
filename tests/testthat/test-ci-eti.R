test_that("xtr_ci_eti() returns the correct values.", {
  expect_equal(xtr_ci_eti(NA_real_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(NA_integer_), tibble::tibble(lower = NA_integer_, upper = NA_integer_))
  expect_equal(xtr_ci_eti(numeric(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(double(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(integer(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_error(xtr_ci_eti(1, 1, TRUE), "`...` must be unused.")

  expect_no_error(xtr_ci_eti(matrix(1:9, ncol = 3)))
  expect_no_error(xtr_ci_eti(array(1:27, dim = c(3, 3, 3))))
  expect_error(xtr_ci_eti(list(1)), "`x` must be numeric.")
  expect_error(xtr_ci_eti(data.frame(1)), "`x` must be numeric.")

  expect_error(xtr_ci_eti(1:100, level = 0), "level` must be greater than 0, not 0.")
  expect_no_error(xtr_ci_eti(1:100, level = 1))

  expect_equal(xtr_ci_eti(1:100, level = 1), tibble::tibble(lower = 1, upper = 100))
  expect_equal(xtr_ci_eti(NA_real_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(numeric(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(NA_integer_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_eti(c(0, rep(Inf, 19))), tibble::tibble(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_eti(c(0, rep(-Inf, 19))), tibble::tibble(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_eti(1:100), tibble::tibble(lower = 1 + 2.475, upper = 100 - 2.475))
  expect_equal(xtr_ci_eti(seq(0, 1, length.out = 1e3)),
               tibble::tibble(lower = 0.025, upper = 0.975))
  expect_equal(xtr_ci_eti(c(1:100, Inf)), tibble::tibble(lower = 3.5, upper = 98.5))
  expect_equal(xtr_ci_eti(c(1:100, rep(Inf, 200))), tibble::tibble(lower = 8.475, upper = Inf))
})
