test_that("exp2", {
  expect_error(exp2(NULL))
  expect_error(exp2("4"))
  expect_equal(exp2(NA), NA_real_)
  expect_equal(exp2(c(NA, 5)), c(NA_real_, 32))
  expect_equal(exp2(rep(NA, 2)), rep(NA_real_, 2))
  expect_equal(exp2(0), 1)
  expect_equal(exp2(c(-Inf, Inf)), c(0, Inf))
  expect_equal(exp2(-10:10),
               c(0.0009765625, 0.001953125, 0.00390625, 0.0078125, 0.015625,
                 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128,
                 256, 512, 1024))
  expect_equal(exp2(seq(0, 1, length.out = 10)),
               c(1, 1.08005973889231, 1.16652903957612, 1.25992104989487, 1.36079000017438,
                 1.4697344922756, 1.5874010519682, 1.71448796570615, 1.85174942457458,
                 2))
  expect_equal(exp2(-100:100), 2^(-100:100))
  expect_equal(log2(exp2(-100:100)), -100:100)
})

test_that("exp10", {
  expect_error(exp10(NULL))
  expect_error(exp10("4"))
  expect_equal(exp10(NA), NA_real_)
  expect_equal(exp10(c(NA, 5)), c(NA_real_, 1e+05))
  expect_equal(exp10(rep(NA, 2)), rep(NA_real_, 2))
  expect_equal(exp10(0), 1)
  expect_equal(exp10(c(-Inf, Inf)), c(0, Inf))
  expect_equal(exp10(-10:10),
               c(1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 0.001, 0.01,
                 0.1, 1, 10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08, 1e+09,
                 1e+10))
  expect_equal(exp10(seq(0, 1, length.out = 10)),
               c(1, 1.29154966501488, 1.66810053720006, 2.15443469003188, 2.78255940220712,
                 3.59381366380463, 4.64158883361278, 5.99484250318941, 7.74263682681127,
                 10))
  expect_equal(exp10(-100:100), 10^(-100:100))
  expect_equal(log10(exp10(-100:100)), -100:100)
})
