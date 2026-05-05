test_that("xtr_ci_norm() returns the correct values.", {
  expect_equal(xtr_ci_norm(NA_real_), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_norm(numeric(0)), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_norm(NA_integer_), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_norm(c(0, rep(Inf, 19))), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_norm(c(0, rep(-Inf, 19))), data.frame(lower = NA_real_, upper = NA_real_))
  expect_equal(xtr_ci_norm(qnorm(seq(1e-3, 1 - 1e-3, length.out = 1e3))),
               data.frame(lower = -1.95, upper = 1.95), tolerance = 0.01)
  expect_equal(xtr_ci_norm(1:100),
               data.frame(lower = -6.36147941, upper = 107.361479))
  expect_equal(xtr_ci_norm(seq(0, 1, length.out = 1e3)),
               data.frame(lower = -0.0666423351179538, upper = 1.06664233511795))
  expect_equal(xtr_ci_norm(c(1:100, Inf)),
               data.frame(lower = NA_real_, upper = NA_real_))
})
