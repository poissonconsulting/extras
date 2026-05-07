test_that("xtr_ci_hdi()", {
  expect_equal(xtr_ci_hdi(NA_real_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(NA_integer_), tibble::tibble(lower = NA_integer_, upper = NA_integer_))
  expect_equal(xtr_ci_hdi(numeric(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(double(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(integer(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_error(xtr_ci_hdi(1, 1, TRUE), "`...` must be unused.")

  expect_no_error(xtr_ci_hdi(matrix(1:9, ncol = 3)))
  expect_no_error(xtr_ci_hdi(array(1:27, dim = c(3, 3, 3))))
  expect_error(xtr_ci_hdi(list(1)), "`x` must be numeric.")
  expect_error(xtr_ci_hdi(data.frame(1)), "`x` must be numeric.")

  expect_error(xtr_ci_hdi(1:100, level = 0), "level` must be greater than 0, not 0.")
  expect_no_error(xtr_ci_hdi(1:100, level = 1))

  # differences between small even and odd sample sizes
  # limits alternate to avoid limits that are not in the sample
  expect_equal(xtr_ci_hdi(1:6, 0 / 6 + 1e-6), tibble::tibble(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:6, 1 / 6), tibble::tibble(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:6, 2 / 6), tibble::tibble(lower = 3, upper = 4))
  expect_equal(xtr_ci_hdi(1:6, 3 / 6), tibble::tibble(lower = 2, upper = 4))
  expect_equal(xtr_ci_hdi(1:6, 4 / 6), tibble::tibble(lower = 2, upper = 5))
  expect_equal(xtr_ci_hdi(1:6, 5 / 6), tibble::tibble(lower = 1, upper = 5))
  expect_equal(xtr_ci_hdi(1:6, 5 / 6 + 1e-6), tibble::tibble(lower = 1, upper = 6))
  expect_equal(xtr_ci_hdi(1:6, 6 / 6 - 1e-6), tibble::tibble(lower = 1, upper = 6))
  expect_equal(xtr_ci_hdi(1:6, 6 / 6), tibble::tibble(lower = 1, upper = 6))

  expect_equal(xtr_ci_hdi(1:5, 1 / 5), tibble::tibble(lower = 3, upper = 3))
  expect_equal(xtr_ci_hdi(1:5, 2 / 5), tibble::tibble(lower = 2, upper = 3))
  expect_equal(xtr_ci_hdi(1:5, 3 / 5), tibble::tibble(lower = 2, upper = 4))
  expect_equal(xtr_ci_hdi(1:5, 4 / 5), tibble::tibble(lower = 1, upper = 4))
  expect_equal(xtr_ci_hdi(1:5, 5 / 5 - 1e-6), tibble::tibble(lower = 1, upper = 5))
  expect_equal(xtr_ci_hdi(1:5, 5 / 5), tibble::tibble(lower = 1, upper = 5))

  # larger sample size
  expect_equal(xtr_ci_hdi(1:10, 0.1), tibble::tibble(lower = 5, upper = 5))
  expect_equal(xtr_ci_hdi(1:10, 0.2), tibble::tibble(lower = 5, upper = 6))
  expect_equal(xtr_ci_hdi(1:10, 0.3), tibble::tibble(lower = 4, upper = 6))
  expect_equal(xtr_ci_hdi(1:10, 0.4), tibble::tibble(lower = 4, upper = 7))
  expect_equal(xtr_ci_hdi(1:10, 0.5), tibble::tibble(lower = 3, upper = 7))
  expect_equal(xtr_ci_hdi(1:10, 0.6), tibble::tibble(lower = 3, upper = 8))
  expect_equal(xtr_ci_hdi(1:10, 0.7), tibble::tibble(lower = 2, upper = 8))
  expect_equal(xtr_ci_hdi(1:10, 0.8), tibble::tibble(lower = 2, upper = 9))
  expect_equal(xtr_ci_hdi(1:10, 0.9), tibble::tibble(lower = 1, upper = 9))
  expect_equal(xtr_ci_hdi(1:10, 0.99), tibble::tibble(lower = 1, upper = 10))
  expect_equal(xtr_ci_hdi(1:10, 1), tibble::tibble(lower = 1, upper = 10))

  expect_equal(xtr_ci_hdi(1:100), tibble::tibble(lower = 3, upper = 97))

  # complications with +/-Inf values
  expect_equal(xtr_ci_hdi(1:20), tibble::tibble(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, 2:20)), tibble::tibble(lower = 2, upper = 20))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:20)), tibble::tibble(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(rep(-Inf, 3), 4:20)), tibble::tibble(lower = -Inf, upper = 19))

  expect_equal(xtr_ci_hdi(1:20), tibble::tibble(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:19, Inf)), tibble::tibble(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:18, Inf, Inf)), tibble::tibble(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:17, rep(Inf, 3))), tibble::tibble(lower = 2, upper = Inf))

  expect_equal(xtr_ci_hdi(c(rep(-Inf, 10), 11:20)), tibble::tibble(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(rep(-Inf, 19), 20)), tibble::tibble(lower = -Inf, upper = 20))
  expect_equal(xtr_ci_hdi(rep(-Inf, 20)), tibble::tibble(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_hdi(rep(Inf, 20)), tibble::tibble(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1, rep(Inf, 19))), tibble::tibble(lower = 1, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:2, rep(Inf, 18))), tibble::tibble(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:10, rep(Inf, 10))), tibble::tibble(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:18, Inf, Inf)), tibble::tibble(lower = 2, upper = Inf))
  expect_equal(xtr_ci_hdi(c(1:19, Inf)), tibble::tibble(lower = 1, upper = 19))
  expect_equal(xtr_ci_hdi(c(1:20)), tibble::tibble(lower = 1, upper = 19))

  expect_equal(xtr_ci_hdi(c(-Inf, 2:19, Inf)), tibble::tibble(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:19, Inf)), tibble::tibble(lower = -Inf, upper = 19))
  expect_equal(xtr_ci_hdi(c(-Inf, 2:18, Inf, Inf)), tibble::tibble(lower = -Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(-Inf, -Inf, 3:18, Inf, Inf)), tibble::tibble(lower = -Inf, upper = Inf))

  # bimodal distributions
  # hist(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), breaks = 0.5 + 0:13)
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.9),
               tibble::tibble(lower = 1, upper = 11))
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.1),
               tibble::tibble(lower = 3, upper = 3))

  # shift the left peak to simulate variation in the data with small sample sizes
  # hist(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), breaks = 0.5 + 0:13)
  expect_equal(xtr_ci_hdi(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 8, 9, 9, 10, 10, 10, 11, 11, 12), 0.1),
               tibble::tibble(lower = 4, upper = 4))
  expect_equal(xtr_ci_hdi(c(rep(1, 4), 2, 5, rep(6, 4)), 0.1),
               tibble::tibble(lower = 1, upper = 1))
  expect_equal(xtr_ci_hdi(c(rep(1, 4), 2, 5, rep(6, 4)), 0.001),
               tibble::tibble(lower = 1, upper = 1))
})
