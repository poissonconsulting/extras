test_that("xtr_ci_hdi() returns the correct values.", {
  expect_equal(xtr_ci_hdi(NA_real_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(numeric(0)), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(NA_integer_), tibble::tibble(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_hdi(c(0, rep(Inf, 19))), tibble::tibble(lower = Inf, upper = Inf))
  expect_equal(xtr_ci_hdi(c(0, rep(-Inf, 19))), tibble::tibble(lower = -Inf, upper = -Inf))
  expect_equal(xtr_ci_hdi(1:100), tibble::tibble(lower = 3, upper = 98))
  expect_equal(xtr_ci_hdi(seq(0, 1, length.out = 1e3)),
               tibble::tibble(lower = 0.032032032032032, upper = 0.982982982982983))
  expect_equal(xtr_ci_hdi(c(1:100, Inf)), tibble::tibble(lower = 2, upper = 98))
  expect_equal(xtr_ci_hdi(c(1:100, rep(Inf, 200))), tibble::tibble(lower = 8, upper = Inf))
})
