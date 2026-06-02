test_that("xtr_ci_hdi() returns the correct values.", {
  expect_equal(xtr_ci_hdi(NA_real_), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(NA_integer_), data.frame(lower = NA_integer_, upper = NA_integer_))
  expect_equal(xtr_ci_hdi(numeric(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(double(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(integer(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(c(NA_integer_, 1:2), na_rm = TRUE), data.frame(lower = 1L, upper = 2L))
  expect_error(xtr_ci_hdi(1, 1, TRUE), "`...` must be unused.")

  expect_no_error(xtr_ci_hdi(matrix(1:9, ncol = 3)))
  expect_no_error(xtr_ci_hdi(array(1:27, dim = c(3, 3, 3))))
  expect_error(xtr_ci_hdi(list(1)), "`x` must be numeric.")
  expect_error(xtr_ci_hdi(data.frame(1)), "`x` must be numeric.")

  expect_error(xtr_ci_hdi(1:100, level = 0), "level` must be greater than 0, not 0.")
  expect_no_error(xtr_ci_hdi(1:100, level = 1))
  expect_error(xtr_ci_hdi(1:100, level = 2), "level` must be between 0 and 1, not 2.")
})

test_that("limits alternate with odd and even sample sizes to avoid limits that are not in the sample.", {
  # differences between small even and odd sample sizes
  expect_equal(xtr_ci_hdi(1:6, 0 / 6 + 1e-6), data.frame(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:6, 1 / 6), data.frame(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:6, 2 / 6), data.frame(lower = 3, upper = 4))
  expect_equal(xtr_ci_hdi(1:6, 3 / 6), data.frame(lower = 2, upper = 4))
  expect_equal(xtr_ci_hdi(1:6, 4 / 6), data.frame(lower = 2, upper = 5))
  expect_equal(xtr_ci_hdi(1:6, 5 / 6), data.frame(lower = 1, upper = 5))
  expect_equal(xtr_ci_hdi(1:6, 5 / 6 + 1e-6), data.frame(lower = 1, upper = 6))
  expect_equal(xtr_ci_hdi(1:6, 6 / 6 - 1e-6), data.frame(lower = 1, upper = 6))
  expect_equal(xtr_ci_hdi(1:6, 6 / 6), data.frame(lower = 1, upper = 6))

  expect_equal(xtr_ci_hdi(1:5, 1 / 5), data.frame(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:5, 2 / 5), data.frame(lower = 2, upper = 3))
  expect_equal(xtr_ci_hdi(1:5, 3 / 5), data.frame(lower = 2, upper = 4))
  expect_equal(xtr_ci_hdi(1:5, 4 / 5), data.frame(lower = 1, upper = 4))
  expect_equal(xtr_ci_hdi(1:5, 5 / 5 - 1e-6), data.frame(lower = 1, upper = 5))
  expect_equal(xtr_ci_hdi(1:5, 5 / 5), data.frame(lower = 1, upper = 5))

  expect_equal(xtr_ci_hdi(1:10, 0.1), data.frame(lower = 5, upper = 5))
  expect_equal(xtr_ci_hdi(1:10, 0.2), data.frame(lower = 5, upper = 6))
  expect_equal(xtr_ci_hdi(1:10, 0.3), data.frame(lower = 4, upper = 6))
  expect_equal(xtr_ci_hdi(1:10, 0.4), data.frame(lower = 4, upper = 7))
  expect_equal(xtr_ci_hdi(1:10, 0.5), data.frame(lower = 3, upper = 7))
  expect_equal(xtr_ci_hdi(1:10, 0.6), data.frame(lower = 3, upper = 8))
  expect_equal(xtr_ci_hdi(1:10, 0.7), data.frame(lower = 2, upper = 8))
  expect_equal(xtr_ci_hdi(1:10, 0.8), data.frame(lower = 2, upper = 9))
  expect_equal(xtr_ci_hdi(1:10, 0.9), data.frame(lower = 1, upper = 9))
  expect_equal(xtr_ci_hdi(1:10, 0.99), data.frame(lower = 1, upper = 10))
  expect_equal(xtr_ci_hdi(1:10, 1), data.frame(lower = 1, upper = 10))

  expect_equal(xtr_ci_hdi(1:100), data.frame(lower = 3, upper = 97))
})

test_path("xtr_ci_hdi() can deal with infinities correctly.", {
  # complications with +/-Inf values
  expect_equal(xtr_ci_hdi(1:20), data.frame(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, 2:20)), data.frame(lower = 2, upper = 20))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:20)), data.frame(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(rep(-Inf, 3), 4:20)), data.frame(lower = -Inf, upper = 19))

  expect_equal(xtr_ci_hdi(1:20), data.frame(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:19, Inf)), data.frame(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:18, Inf, Inf)), data.frame(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:17, rep(Inf, 3))), data.frame(lower = 2, upper = Inf))

  expect_equal(xtr_ci_hdi(c(rep(-Inf, 10), 11:20)), data.frame(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(rep(-Inf, 19), 20)), data.frame(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_hdi(c(rep(-Inf, 19), Inf)), data.frame(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_hdi(rep(-Inf, 20)), data.frame(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_hdi(rep(Inf, 20)), data.frame(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(-Inf, rep(Inf, 19))), data.frame(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1, rep(Inf, 19))), data.frame(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:2, rep(Inf, 18))), data.frame(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:10, rep(Inf, 10))), data.frame(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:18, Inf, Inf)), data.frame(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:19, Inf)), data.frame(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:20)), data.frame(lower = 1, upper = 19))

  expect_equal(xtr_ci_hdi(c(-Inf, 2:19, Inf)), data.frame(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:19, Inf)), data.frame(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, 2:18, Inf, Inf)), data.frame(lower = -Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:18, Inf, Inf)), data.frame(lower = -Inf, upper = Inf))
})

test_that("xtr_ci_hdi() can deal with bimodal distributions correctly.", {
  if (FALSE) {
    hist(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), breaks = 0.5 + 0:13)
  }
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.9),
               data.frame(lower = 1, upper = 11))
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.1),
               data.frame(lower = 3, upper = 3))

  # shift the left peak to simulate variation in the data with small sample sizes
  if (FALSE) {
    hist(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), breaks = 0.5 + 0:13)
  }
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.1),
               data.frame(lower = 4, upper = 4))
  expect_equal(xtr_ci_hdi(c(rep(1, 4), 2, 5, rep(6, 4)), 0.1),
               data.frame(lower = 1, upper = 1))
  expect_equal(xtr_ci_hdi(c(rep(1, 4), 2, 5, rep(6, 4)), 0.001),
               data.frame(lower = 1, upper = 1))
})
