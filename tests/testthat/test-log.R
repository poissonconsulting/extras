test_that("log2<- errors incorrect inputs", {
  x <- NULL
  expect_error(log2(x) <- "5", "non-numeric argument to binary operator")
})

test_that("log2<- edge cases", {
  x <- NULL
  log2(x) <- NULL
  expect_equal(x, numeric(0))
  x <- NULL
  log2(x) <- numeric(0)
  expect_equal(x, numeric(0))
  x <- NULL
  log2(x) <- -Inf
  expect_equal(x, 0)
  x <- NULL
  log2(x) <- Inf
  expect_equal(x, Inf)
  x <- NULL
  log2(x) <- NA_real_
  expect_equal(x, NA_real_)
  x <- NULL
  log2(x) <- rep(NA_real_, 3)
  expect_equal(x, rep(NA_real_, 3))
})

test_that("log2<- logicals", {
  x <- NULL
  log2(x) <- TRUE
  expect_equal(x, 2)
  x <- NULL
  log2(x) <- FALSE
  expect_equal(x, 1)
  x <- NULL
  log2(x) <- NA
  expect_equal(x, NA_real_)
  x <- NULL
  log2(x) <- c(TRUE, FALSE, NA)
  expect_equal(x, c(2, 1, NA_real_))
})

test_that("log2<- numeric", {
  x <- NULL
  log2(x) <- -5L
  expect_equal(x, 0.03125)
  x <- NULL
  log2(x) <- 0L
  expect_equal(x, 1)
  x <- NULL
  log2(x) <- 100L
  expect_equal(x, 1.26765060022823e+30)
  x <- NULL
  log2(x) <- -1.233
  expect_equal(x, 0.42543186475327)
  x <- NULL
  log2(x) <- 1.002
  expect_equal(x, 2.00277451142267)
})

test_that("log2<- vectors", {
  x <- NULL
  log2(x) <- c(-10L, -5L, -1L, 0L, 1L, 5L, 10L)
  expect_equal(x, c(0.0009765625, 0.03125, 0.5, 1, 2, 32, 1024))
  x <- NULL
  log2(x) <- seq(-5, 5, length.out = 21)
  expect_equal(x, c(0.03125, 0.0441941738241592, 0.0625, 0.0883883476483184, 0.125,
                    0.176776695296637, 0.25, 0.353553390593274, 0.5, 0.707106781186548,
                    1, 1.4142135623731, 2, 2.82842712474619, 4, 5.65685424949238,
                    8, 11.3137084989848, 16, 22.6274169979695, 32))
})

test_that("log2<- matrix", {
  x <- NULL
  log2(x) <- matrix(-10:9, nrow = 4, ncol = 5)
  expect_equal(x, structure(c(0.0009765625, 0.001953125, 0.00390625, 0.0078125,
                              0.015625, 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16,
                              32, 64, 128, 256, 512), dim = 4:5))
  x <- NULL
  log2(x) <- matrix(seq(-1, 1, length.out = 20), nrow = 4, ncol = 5)
  expect_equal(x, structure(c(0.5, 0.537845293110091, 0.57855511864136, 0.622346294732012,
                              0.669452050612236, 0.72012326876938, 0.774629821133328, 0.833262006398545,
                              0.896332096137856, 0.964175997942495, 1.03715504444619, 1.11565791776154,
                              1.2001027195781, 1.2909391979474, 1.38865114261466, 1.49375896165449,
                              1.60682245313376, 1.72844378656321, 1.85927071001681, 2), dim = 4:5))
})

test_that("log2<- array", {
  x <- NULL
  log2(x) <- array(-10:9, dim = c(2, 2, 5))
  expect_equal(x, structure(c(0.0009765625, 0.001953125, 0.00390625, 0.0078125,
                              0.015625, 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16,
                              32, 64, 128, 256, 512), dim = c(2L, 2L, 5L)))
  x <- NULL
  log2(x) <- array(seq(-1, 1, length.out = 20), dim = c(2, 2, 5))
  expect_equal(x, structure(c(0.5, 0.537845293110091, 0.57855511864136, 0.622346294732012,
                              0.669452050612236, 0.72012326876938, 0.774629821133328, 0.833262006398545,
                              0.896332096137856, 0.964175997942495, 1.03715504444619, 1.11565791776154,
                              1.2001027195781, 1.2909391979474, 1.38865114261466, 1.49375896165449,
                              1.60682245313376, 1.72844378656321, 1.85927071001681, 2), dim = c(2L, 2L, 5L)))

})

test_that("log2<- dataframe", {
  x <- NULL
  log2(x) <- data.frame(y = 10:15)
  expect_equal(x, data.frame(y = c(1024, 2048, 4096, 8192, 16384, 32768)))
})

test_that("log10<- errors incorrect inputs", {
  x <- NULL
  expect_error(log10(x) <- "5", "non-numeric argument to binary operator")
})

test_that("log10<- edge cases", {
  x <- NULL
  log10(x) <- NULL
  expect_equal(x, numeric(0))
  x <- NULL
  log10(x) <- numeric(0)
  expect_equal(x, numeric(0))
  x <- NULL
  log10(x) <- -Inf
  expect_equal(x, 0)
  x <- NULL
  log10(x) <- Inf
  expect_equal(x, Inf)
  x <- NULL
  log10(x) <- NA_real_
  expect_equal(x, NA_real_)
  x <- NULL
  log10(x) <- rep(NA_real_, 3)
  expect_equal(x, rep(NA_real_, 3))
})

test_that("log10<- logicals", {
  x <- NULL
  log10(x) <- TRUE
  expect_equal(x, 10)
  x <- NULL
  log10(x) <- FALSE
  expect_equal(x, 1)
  x <- NULL
  log10(x) <- NA
  expect_equal(x, NA_real_)
  x <- NULL
  log10(x) <- c(TRUE, FALSE, NA)
  expect_equal(x, c(10, 1, NA_real_))
})

test_that("log10<- numeric", {
  x <- NULL
  log10(x) <- -5L
  expect_equal(x, 1e-05)
  x <- NULL
  log10(x) <- 0L
  expect_equal(x, 1)
  x <- NULL
  log10(x) <- 100L
  expect_equal(x, 1e+100)
  x <- NULL
  log10(x) <- -1.233
  expect_equal(x, 0.0584790084144481)
  x <- NULL
  log10(x) <- 1.002
  expect_equal(x, 10.046157902784)
})

test_that("log10<- vectors", {
  x <- NULL
  log10(x) <- c(-10L, -5L, -1L, 0L, 1L, 5L, 10L)
  expect_equal(x, c(1e-10, 1e-05, 0.1, 1, 10, 1e+05, 1e+10))
  x <- NULL
  log10(x) <- seq(-5, 5, length.out = 21)
  expect_equal(x, c(1e-05, 3.16227766016838e-05, 1e-04, 0.000316227766016838, 0.001,
                    0.00316227766016838, 0.01, 0.0316227766016838, 0.1, 0.316227766016838,
                    1, 3.16227766016838, 10, 31.6227766016838, 100, 316.227766016838,
                    1000, 3162.27766016838, 10000, 31622.7766016838, 1e+05))
})

test_that("log10<- matrix", {
  x <- NULL
  log10(x) <- matrix(-10:9, nrow = 4, ncol = 5)
  expect_equal(x, structure(c(1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04,
                              0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07,
                              1e+08, 1e+09), dim = 4:5))
  x <- NULL
  log10(x) <- matrix(seq(-1, 1, length.out = 20), nrow = 4, ncol = 5)
  expect_equal(x, structure(c(0.1, 0.127427498570313, 0.162377673918872, 0.206913808111479,
                              0.263665089873036, 0.335981828628378, 0.428133239871939, 0.545559478116852,
                              0.695192796177561, 0.885866790410083, 1.12883789168469, 1.43844988828766,
                              1.83298071083244, 2.33572146909012, 2.97635144163132, 3.79269019073225,
                              4.83293023857175, 6.15848211066026, 7.84759970351461, 10), dim = 4:5))
})

test_that("log10<- array", {
  x <- NULL
  log10(x) <- array(-10:9, dim = c(2, 2, 5))
  expect_equal(x, structure(c(1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04,
                              0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07,
                              1e+08, 1e+09), dim = c(2L, 2L, 5L)))
  x <- NULL
  log10(x) <- array(seq(-1, 1, length.out = 20), dim = c(2, 2, 5))
  expect_equal(x, structure(c(0.1, 0.127427498570313, 0.162377673918872, 0.206913808111479,
                              0.263665089873036, 0.335981828628378, 0.428133239871939, 0.545559478116852,
                              0.695192796177561, 0.885866790410083, 1.12883789168469, 1.43844988828766,
                              1.83298071083244, 2.33572146909012, 2.97635144163132, 3.79269019073225,
                              4.83293023857175, 6.15848211066026, 7.84759970351461, 10), dim = c(2L, 2L, 5L)))

})

test_that("log10<- dataframe", {
  x <- NULL
  log10(x) <- data.frame(y = 10:15)
  expect_equal(x, data.frame(y = c(1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15)))
})
