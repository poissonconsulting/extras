test_that("xtr_ci_eti() returns the correct values.", {
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
