test_that("directional_information() returns correct values.", {
  expect_identical(directional_information(NA_integer_), NA_real_)
  expect_identical(directional_information(NA_real_), NA_real_)
  expect_identical(directional_information(NaN), NA_real_)
  expect_identical(directional_information(Inf), log2(1))
  expect_identical(directional_information(-Inf), log2(1))
  expect_identical(directional_information(integer(0)), NA_real_)
  expect_identical(directional_information(c(1, NA)), NA_real_)

  expect_identical(directional_information(c(-1:2), threshold_split = "left"), log2(2/2))
  expect_identical(directional_information(c(-1:2), threshold_split = "right"), log2(3/1))
  expect_identical(directional_information(c(-1:2), threshold_split = "equal"), log2(2.5/1.5))
  expect_identical(directional_information(c(-1:2), threshold_split = "proportional"), log2(2/1))
  expect_identical(directional_information(c(-1:2), threshold_split = "exclude"), log2(2/1))

  expect_identical(directional_information(1), 0)
  expect_equal(directional_information(c(1, 1)), log2(2))
  expect_equal(directional_information(c(1, 1, 1)), log2(3))
  expect_equal(directional_information(1:9), log2(9))
  expect_equal(directional_information(-(1:9)), log2(9))
  expect_equal(directional_information(c(-1, 1, 1)), log2(2) - log2(1))
  expect_equal(directional_information(c(-1, 1, 1, 1)), log2(3) - log(1))
  expect_equal(directional_information(c(-1, -1, 1, 1)), log2(2) - log2(2))
  expect_equal(directional_information(c(rep(-1, 25), rep(1, 1000 - 25))),
               log2(1000 - 25) - log2(25))
  expect_equal(directional_information(c(rep(-1, 24), rep(1, 1000 - 24))),
               log2(1000 - 24) - log2(24))
  expect_equal(directional_information(c(rep(-1, 25), rep(0, 1000 - 25))),
               -log2(1000))
  expect_equal(directional_information(-9:10), log2(10) - log2(9))
  expect_equal(directional_information(-9:10 + 1, threshold = 1),
               log2(10) - log2(9))
  expect_identical(directional_information(c(-1, 1, 1, 1, NA)), NA_real_)
  expect_equal(directional_information(c(-1, 1, 1, 1, NA), na_rm = TRUE),
               log2(3) - log2(1))
  expect_equal(directional_information(rep(10, 1e6)), log2(1e6))
  expect_equal(directional_information(x = rnorm(1e7, mean = qnorm(0.95))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rcauchy(1e7, qcauchy(0.95))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rnorm(1e7, mean = qnorm(0.05))),
               4.246, tolerance = 0.01)
  expect_equal(directional_information(x = rcauchy(1e7, qcauchy(0.05))),
               4.246, tolerance = 0.01)
})

test_that("directional_information() errors with unused arguments", {
  expect_error(directional_information(1, 0),  "`...` must be unused.")
  expect_error(directional_information(1, foo = TRUE),  "`...` must be unused.")
})

test_that("directional_information() skeptical argument works correctly with default side", {
  expect_equal(directional_information(rep(1, 4)), log2(4))
  expect_equal(directional_information(rep(1, 4), skeptical = FALSE), 4)
  expect_equal(directional_information(rep(-1, 4), skeptical = TRUE), log2(4))
  expect_equal(directional_information(c(-1, 1, 1), skeptical = TRUE),
               log2(2) - log2(1))
  # skeptical = FALSE has no effect when samples are on both sides
  expect_equal(directional_information(c(-1, 1, 1), skeptical = TRUE),
               directional_information(c(-1, 1, 1), skeptical = FALSE))
})

test_that("directional_information() skeptical works correctly with side argument", {
  # all positive: supports right, contradicts left
  expect_equal(directional_information(rep(1, 4), side = "right", skeptical = TRUE), log2(4))
  expect_equal(directional_information(rep(1, 4), side = "left",  skeptical = TRUE), -log2(4))
  # all negative: supports left, contradicts right
  expect_equal(directional_information(rep(-1, 4), side = "left",  skeptical = TRUE), log2(4))
  expect_equal(directional_information(rep(-1, 4), side = "right", skeptical = TRUE), -log2(4))
  # default (median) follows the direction of the data
  expect_equal(directional_information(rep(1,  4), side = "median", skeptical = TRUE), log2(4))
  expect_equal(directional_information(rep(-1, 4), side = "median", skeptical = TRUE), log2(4))
})

test_that("directional_information() performs the right correction if most x = threshold", {
  expect_identical(directional_information(0), 0)
  expect_equal(directional_information(c(0, 0)), 0)
  expect_identical(directional_information(c(0, 0, 0)), 0)
  expect_equal(directional_information(c(rep(1, 25), rep(0, 1000 - 25))), log2(1000))
})

test_that("p2info() returns the correct values", {
  expect_identical(p2info(NA_real_), NA_real_)
  expect_identical(p2info(NA_integer_), NA_real_)
  expect_identical(p2info(0), -Inf)
  expect_identical(p2info(.Machine$double.eps), log2(.Machine$double.eps) - log2(1 - .Machine$double.eps))
  expect_identical(p2info(1 - .Machine$double.eps), log2(1 - .Machine$double.eps) - log2(.Machine$double.eps))
  expect_identical(p2info(1), Inf)

  expect_identical(p2info(NA_real_, 1L), NA_real_)
  expect_identical(p2info(0, 100), -100)
  expect_identical(p2info(.Machine$double.eps, 30), -30)
  p <- runif(1e4)
  expect_equal(p2info(p), log2(p) - log2(1 - p))
  expect_identical(p2info(1 - .Machine$double.eps, 30), 30)
  expect_identical(p2info(1, 1L), 1)
  expect_identical(p2info(1, 2), 2)
})
