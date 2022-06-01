test_that("ilog", {
  expect_equal(c(0, 0.1, 1, 10), ilog(c(-Inf, -2.302585,  0.000000,  2.302585)),
               tolerance = 0.0000001)
})

