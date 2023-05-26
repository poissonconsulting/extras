test_that("ilog", {
  expect_equal(c(0, 0.1, 1, 10), ilog(c(-Inf, -2.302585,  0.000000,  2.302585)),
               tolerance = 0.0000001)
})

test_that("ilog2 incorrect inputs", {
  expect_error(ilog2("6"), "`x` must be numeric\\.")
  expect_error(ilog2(TRUE), "`x` must be numeric\\.")
  expect_error(ilog2(NA), "`x` must be numeric\\.")
})

test_that("ilog2 edge cases", {
  expect_error(ilog2(NULL), "`x` must be numeric\\.")
  expect_equal(ilog2(numeric(0)), numeric(0))
  expect_equal(ilog2(NA_real_), NA_real_)
  expect_equal(ilog2(rep(NA_real_, 2)), rep(NA_real_, 2))
  expect_equal(ilog2(Inf), Inf)
  expect_equal(ilog2(-Inf), 0)
})

test_that("ilog2 numeric", {
  expect_equal(ilog2(1L), 2)
  expect_equal(ilog2(-5L), 0.03125)
  expect_equal(ilog2(100L), 1.26765060022823e+30)
  expect_equal(ilog2(-0.0231), 0.984115805960014)
  expect_equal(ilog2(1.000322), 2.00044643660328)
  expect_equal(ilog2(2.491), 5.62167480082688)
})

test_that("ilog2 vectorized", {
  expect_equal(ilog2(-10:10),
               c(0.0009765625, 0.001953125, 0.00390625, 0.0078125, 0.015625,
                 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128,
                 256, 512, 1024))
  expect_equal(ilog2(seq(-2, 2, length.out = 10)),
               c(0.25, 0.340197500043594, 0.462937356143645, 0.629960524947437,
                 0.857243982853073, 1.16652903957612, 1.5874010519682, 2.16011947778461,
                 2.9394689845512, 4))
})

test_that("ilog2 matrix", {
  expect_equal(ilog2(matrix(1:10, ncol = 2, nrow = 5)),
               structure(c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), dim = c(5L, 2L)))
  expect_equal(ilog2(matrix(seq(-2, 2, length.out = 10), nrow = 2, ncol = 5)),
               structure(c(0.25, 0.340197500043594, 0.462937356143645, 0.629960524947437,
                           0.857243982853073, 1.16652903957612, 1.5874010519682, 2.16011947778461,
                           2.9394689845512, 4), dim = c(2L, 5L)))
})

test_that("ilog2 array", {
  expect_equal(ilog2(array(1:20, dim = c(5, 2, 2))),
               structure(c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096,
                           8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576),
                         dim = c(5L, 2L, 2L)))
  expect_equal(ilog2(array(seq(-2, 2, length.out = 20), dim = c(2, 5, 2))),
               structure(c(0.25, 0.28927755932068, 0.334726025306118, 0.387314910566664,
                           0.448166048068928, 0.518577522223096, 0.600051359789051, 0.694325571307328,
                           0.803411226566882, 0.929635355008406, 1.07569058622018, 1.24469258946402,
                           1.44024653753876, 1.66652401279709, 1.92835199588499, 2.23131583552309,
                           2.58187839589481, 2.98751792330897, 3.45688757312642, 4), dim = c(2L, 5L, 2L)))
})

test_that("ilog10 incorrect inputs", {
  expect_error(ilog10("6"), "`x` must be numeric\\.")
  expect_error(ilog10(TRUE), "`x` must be numeric\\.")
  expect_error(ilog10(NA), "`x` must be numeric\\.")
})

test_that("ilog10 edge cases", {
  expect_error(ilog10(NULL), "`x` must be numeric\\.")
  expect_equal(ilog10(numeric(0)), numeric(0))
  expect_equal(ilog10(NA_real_), NA_real_)
  expect_equal(ilog10(rep(NA_real_, 2)), rep(NA_real_, 2))
  expect_equal(ilog10(Inf), Inf)
  expect_equal(ilog10(-Inf), 0)
})

test_that("ilog10 numeric", {
  expect_equal(ilog10(1L), 10)
  expect_equal(ilog10(-5L), 1e-05)
  expect_equal(ilog10(100L), 1e+100)
  expect_equal(ilog10(-0.0231), 0.948200107019506)
  expect_equal(ilog10(1.000322), 10.0074170732889)
  expect_equal(ilog10(2.491), 309.741929921658)
})

test_that("ilog10 vectorized", {
  expect_equal(ilog10(-10:10),
               c(1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 0.001, 0.01,
                 0.1, 1, 10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08, 1e+09,
                 1e+10))
  expect_equal(ilog10(seq(-2, 2, length.out = 10)),
               c(0.01, 0.0278255940220712, 0.0774263682681127, 0.215443469003188,
                 0.599484250318941, 1.66810053720006, 4.64158883361278, 12.9154966501488,
                 35.9381366380463, 100))
})

test_that("ilog10 matrix", {
  expect_equal(ilog10(matrix(1:10, ncol = 2, nrow = 5)),
               structure(c(10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08,
                           1e+09, 1e+10), dim = c(5L, 2L)))
  expect_equal(ilog10(matrix(seq(-2, 2, length.out = 10), nrow = 2, ncol = 5)),
               structure(c(0.01, 0.0278255940220712, 0.0774263682681127, 0.215443469003188,
                           0.599484250318941, 1.66810053720006, 4.64158883361278, 12.9154966501488,
                           35.9381366380463, 100), dim = c(2L, 5L)))
})

test_that("ilog10 array", {
  expect_equal(ilog10(array(1:20, dim = c(5, 2, 2))),
               structure(c(10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08,
                           1e+09, 1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15, 1e+16, 1e+17,
                           1e+18, 1e+19, 1e+20), dim = c(5L, 2L, 2L)))
  expect_equal(ilog10(array(seq(-2, 2, length.out = 20), dim = c(2, 5, 2))),
               structure(c(0.01, 0.0162377673918872, 0.0263665089873036, 0.0428133239871939,
                           0.0695192796177561, 0.112883789168469, 0.183298071083244, 0.297635144163132,
                           0.483293023857175, 0.784759970351461, 1.27427498570313, 2.06913808111479,
                           3.35981828628378, 5.45559478116851, 8.85866790410082, 14.3844988828766,
                           23.3572146909012, 37.9269019073225, 61.5848211066026, 100), dim = c(2L, 5L, 2L)))
})
