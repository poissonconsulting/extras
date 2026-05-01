test_that("directional_information() returns correct values.", {
  expect_identical(directional_information(NA_integer_), NA_real_)
  expect_identical(directional_information(NA_real_), NA_real_)
  expect_identical(directional_information(NaN), NA_real_)
  expect_identical(directional_information(Inf), NA_real_)
  expect_identical(directional_information(-Inf), NA_real_)
  expect_identical(directional_information(integer(0)), NA_real_)
  expect_identical(directional_information(c(1, NA)), NA_real_)
  expect_identical(directional_information(1), 1)
  expect_equal(directional_information(c(1, 1)), 2)
  expect_equal(directional_information(c(1, 1, 1)), 3)
  expect_equal(directional_information(1:9), 9)
  expect_equal(directional_information(-(1:9)), 9)
  expect_equal(directional_information(c(-1, 1, 1)), log2(2) - log2(1))
  expect_equal(directional_information(c(-1, 1, 1, 1)), log2(3) - log(1))
  expect_equal(directional_information(c(-1, -1, 1, 1)), log2(2) - log2(2))
  expect_equal(directional_information(c(rep(-1, 25), rep(1, 1000 - 25))),
               log2(1000 - 25) - log2(25))
  expect_equal(directional_information(c(rep(-1, 24), rep(1, 1000 - 24))),
               log2(1000 - 24) - log2(24))
  expect_equal(directional_information(c(rep(-1, 25), rep(0, 1000 - 25))),
               -1e3)
  expect_equal(directional_information(-9:10), log2(10) - log2(9))
  expect_equal(directional_information(-9:10 + 1, threshold = 1),
               log2(10) - log2(9))
  expect_identical(directional_information(c(-1, 1, 1, 1, NA)), NA_real_)
  expect_equal(directional_information(c(-1, 1, 1, 1, NA), na_rm = TRUE),
               log2(3) - log2(1))
  expect_equal(directional_information(rep(10, 1e6)), 1e6)
  expect_equal(directional_information(x = rnorm(1e7, mean = qnorm(0.95))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rcauchy(1e7, qcauchy(0.95))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rnorm(1e7, mean = qnorm(0.05))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rcauchy(1e7, qcauchy(0.05))),
               4.246, tolerance = 0.01)
})

test_that("directional_information() performs the right correction if most x = threshold", {
  expect_identical(directional_information(0), 0)
  expect_equal(directional_information(c(0, 0)), 0)
  expect_identical(directional_information(c(0, 0, 0)), 0)
  expect_equal(directional_information(c(rep(1, 25), rep(0, 1000 - 25))), 1e3)
})
