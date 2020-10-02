test_that("multiplication works", {
  expect_identical(devpois(integer(0), integer(0)), numeric(0))
  expect_identical(devpois(1, 1), 0)
  expect_identical(devpois(NA, 1), NA_real_)
  expect_identical(devpois(1, NA), NA_real_)
  expect_equal(devpois(c(1,3.5,4), 3),
               c(-1.34267472705186, 0.281166781094084, 0.549050616623137))
  expect_identical(devpois(c(1,3.5,4), c(1, 3.5, 4)),
                   c(0, 0, 0))
})
