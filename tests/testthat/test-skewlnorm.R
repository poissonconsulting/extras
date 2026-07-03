# dskewlnorm ----
test_that("dskewlnorm passes through numeric(0) with zero-length arguments", {
  skip_if_not_installed("sn")
  expect_identical(dskewlnorm(logical(0)), numeric(0))
  expect_identical(dskewlnorm(numeric(0)), numeric(0))
  expect_identical(dskewlnorm(1, numeric(0)), numeric(0))
  expect_identical(dskewlnorm(1, 2, numeric(0)), numeric(0))
  expect_identical(dskewlnorm(1, 2, 3, numeric(0)), numeric(0))
  expect_identical(dskewlnorm(1, numeric(0), 2, 3), numeric(0))
})

test_that("dskewlnorm errors with arguments = NULL", {
  skip_if_not_installed("sn")
  expect_error(dskewlnorm(NULL), "invalid arguments")
  expect_error(dskewlnorm(1, NULL), "invalid arguments")
  expect_error(dskewlnorm(1, 2, NULL), "invalid arguments")
  expect_error(dskewlnorm(1, 2, 3, NULL), "invalid arguments")
})

test_that("dskewlnorm errors with character arguments", {
  skip_if_not_installed("sn")
  expect_chk_error(dskewlnorm(character(0)), "`character` must be FALSE.")
  expect_chk_error(dskewlnorm(character(0), 1:5, 1:5, 1:5), "`character` must be FALSE.")
  expect_chk_error(dskewlnorm("1"), "`character` must be FALSE.")
})

test_that("dskewlnorm does not allow negative sdlog argument", {
  skip_if_not_installed("sn")
  expect_chk_error(dskewlnorm(1, 0, -5, 0), "`sdlog` must be greater than or equal to 0, not -5.")
  expect_chk_error(dskewlnorm(1, 0, -1, -10), "`sdlog` must be greater than or equal to 0, not -1.")
})

test_that("dskewlnorm returns NA with NA arguments", {
  skip_if_not_installed("sn")
  expect_identical(dskewlnorm(NA), NA_real_)
  expect_identical(dskewlnorm(1, NA), NA_real_)
  expect_identical(dskewlnorm(1, 2, NA), NA_real_)
  expect_identical(dskewlnorm(1, 2, 3, NA), NA_real_)
})

test_that("dskewlnorm returns 0 for non-positive x", {
  skip_if_not_installed("sn")
  expect_identical(dskewlnorm(0), 0)
  expect_identical(dskewlnorm(-1, 0, 1, 2), 0)
  expect_equal(dskewlnorm(c(-1, 0, 1), shape = 1), c(0, 0, dskewlnorm(1, shape = 1)))
})

test_that("dskewlnorm returns 0 with x = Inf", {
  skip_if_not_installed("sn")
  expect_identical(dskewlnorm(Inf), 0)
  expect_identical(dskewlnorm(Inf, 4, 2, -1), 0)
})

test_that("dskewlnorm equal to dlnorm when shape = 0", {
  skip_if_not_installed("sn")
  expect_equal(dskewlnorm(1:5, 0.3, 0.7, 0), stats::dlnorm(1:5, 0.3, 0.7))
  expect_equal(dskewlnorm(1:5, 0.3, 0.7, 0, log = TRUE), stats::dlnorm(1:5, 0.3, 0.7, log = TRUE))
})

test_that("dskewlnorm returns expected output", {
  skip_if_not_installed("sn")
  expect_equal(dskewlnorm(2, 0, 1, 2), 0.287760753184518)
  expect_equal(
    dskewlnorm(1:5, 0, 1, 2),
    c(
      0.3989422804014326, 0.2877607531845184, 0.1434198002929759,
      0.0760947326584013, 0.0436733090574449
    )
  )
  expect_equal(
    dskewlnorm(2, c(-1, 0, 1), 1, 2),
    c(0.0951151990705843, 0.2877607531845184, 0.1026485125306297)
  )
})

test_that("dskewlnorm setting log = TRUE returns same value as log(call with log = FALSE)", {
  skip_if_not_installed("sn")
  expect_equal(dskewlnorm(2, 0, 1, 2, log = TRUE), log(dskewlnorm(2, 0, 1, 2)))
})

test_that("dskewlnorm errors if argument lengths are incompatible", {
  skip_if_not_installed("sn")
  expect_chk_error(dskewlnorm(1:3, 0, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 5.")
})

# pskewlnorm ----
test_that("pskewlnorm passes through numeric(0) with zero-length arguments", {
  skip_if_not_installed("sn")
  expect_identical(pskewlnorm(logical(0)), numeric(0))
  expect_identical(pskewlnorm(numeric(0)), numeric(0))
  expect_identical(pskewlnorm(1, numeric(0)), numeric(0))
  expect_identical(pskewlnorm(1, 2, numeric(0)), numeric(0))
  expect_identical(pskewlnorm(1, 2, 3, numeric(0)), numeric(0))
})

test_that("pskewlnorm errors with arguments = NULL", {
  skip_if_not_installed("sn")
  expect_error(pskewlnorm(NULL), "invalid arguments")
  expect_error(pskewlnorm(1, NULL), "invalid arguments")
})

test_that("pskewlnorm errors with character arguments", {
  skip_if_not_installed("sn")
  expect_chk_error(pskewlnorm("1"), "`character` must be FALSE.")
})

test_that("pskewlnorm does not allow negative sdlog argument", {
  skip_if_not_installed("sn")
  expect_chk_error(pskewlnorm(1, 0, -5, 0), "`sdlog` must be greater than or equal to 0, not -5.")
})

test_that("pskewlnorm returns NA with NA arguments", {
  skip_if_not_installed("sn")
  expect_identical(pskewlnorm(NA), NA_real_)
  expect_identical(pskewlnorm(1, NA), NA_real_)
})

test_that("pskewlnorm returns 0 for non-positive q", {
  skip_if_not_installed("sn")
  expect_identical(pskewlnorm(0), 0)
  expect_equal(pskewlnorm(c(-1, 0, 1), shape = 1), c(0, 0, pskewlnorm(1, shape = 1)))
})

test_that("pskewlnorm returns 1 with q = Inf", {
  skip_if_not_installed("sn")
  expect_identical(pskewlnorm(Inf), 1)
  expect_identical(pskewlnorm(Inf, 4, 2, -1), 1)
})

test_that("pskewlnorm equal to plnorm when shape = 0", {
  skip_if_not_installed("sn")
  expect_equal(pskewlnorm(1:5, 0.3, 0.7, 0), stats::plnorm(1:5, 0.3, 0.7))
})

test_that("pskewlnorm returns expected output", {
  skip_if_not_installed("sn")
  expect_equal(pskewlnorm(2, 0, 1, 2), 0.521879277607988)
  expect_equal(
    pskewlnorm(1:5, 0, 1, 2),
    c(
      0.147583617650433, 0.521879277607988, 0.728958097681998,
      0.834447986341680, 0.892494858566127
    )
  )
})

# qskewlnorm ----
test_that("qskewlnorm passes through numeric(0) with zero-length arguments", {
  skip_if_not_installed("sn")
  expect_identical(qskewlnorm(logical(0)), numeric(0))
  expect_identical(qskewlnorm(numeric(0)), numeric(0))
  expect_identical(qskewlnorm(0.1, numeric(0)), numeric(0))
})

test_that("qskewlnorm errors with arguments = NULL", {
  skip_if_not_installed("sn")
  expect_error(qskewlnorm(NULL), "invalid arguments")
})

test_that("qskewlnorm errors with character arguments", {
  skip_if_not_installed("sn")
  expect_chk_error(qskewlnorm("1"), "`character` must be FALSE.")
})

test_that("qskewlnorm errors with p < 0 or p > 1", {
  skip_if_not_installed("sn")
  expect_chk_error(qskewlnorm(-1), "`p` must be greater than or equal to 0, not -1.")
  expect_chk_error(qskewlnorm(1.1), "`p` must be less than or equal to 1, not 1.1.")
})

test_that("qskewlnorm does not allow negative sdlog argument", {
  skip_if_not_installed("sn")
  expect_chk_error(qskewlnorm(0.5, 0, -5, 0), "`sdlog` must be greater than or equal to 0, not -5.")
})

test_that("qskewlnorm returns NA with NA arguments", {
  skip_if_not_installed("sn")
  expect_identical(qskewlnorm(NA), NA_real_)
  expect_identical(qskewlnorm(0.1, NA), NA_real_)
})

test_that("qskewlnorm returns 0 and Inf at boundary probabilities", {
  skip_if_not_installed("sn")
  expect_identical(qskewlnorm(0, 0, 1, 2), 0)
  expect_identical(qskewlnorm(1, 0, 1, 2), Inf)
})

test_that("qskewlnorm equal to qlnorm when shape = 0", {
  skip_if_not_installed("sn")
  expect_equal(qskewlnorm(c(0.1, 0.5, 0.9), 0.3, 0.7, 0), stats::qlnorm(c(0.1, 0.5, 0.9), 0.3, 0.7))
})

test_that("qskewlnorm is the inverse of pskewlnorm", {
  skip_if_not_installed("sn")
  expect_equal(pskewlnorm(qskewlnorm(0.7, 0.3, 0.7, 2), 0.3, 0.7, 2), 0.7, tolerance = 1e-6)
})

test_that("qskewlnorm returns expected output", {
  skip_if_not_installed("sn")
  expect_equal(qskewlnorm(0.5, 0, 1, 2), 1.92585572275397)
  expect_equal(
    qskewlnorm(c(0.1, 0.5, 0.9), 0, 1, 2),
    c(0.874755157318516, 1.925855722753973, 5.179970465703753)
  )
})

# rskewlnorm ----
test_that("rskewlnorm passes through numeric(0) with zero-length arguments", {
  skip_if_not_installed("sn")
  expect_identical(rskewlnorm(logical(0)), numeric(0))
  expect_identical(rskewlnorm(1, numeric(0)), numeric(0))
})

test_that("rskewlnorm errors as expected", {
  skip_if_not_installed("sn")
  expect_error(rskewlnorm(NULL), "invalid arguments")
  expect_chk_error(rskewlnorm("1"), "`n` must be a whole number.")
  expect_chk_error(rskewlnorm(NA), "`n` must not have any missing values.")
  expect_chk_error(rskewlnorm(Inf), "`n` must be less than Inf, not Inf.")
  expect_chk_error(rskewlnorm(-1), "`n` must be greater than or equal to 0, not -1.")
  expect_chk_error(rskewlnorm(1, 0, -5, 0), "`sdlog` must be greater than or equal to 0, not -5.")
})

test_that("rskewlnorm returns numeric(0) with n = 0L", {
  skip_if_not_installed("sn")
  expect_identical(rskewlnorm(0L), numeric(0))
})

test_that("rskewlnorm returns positive deviates", {
  skip_if_not_installed("sn")
  withr::with_seed(
    101,
  expect_true(all(rskewlnorm(100, 0, 1, 2) > 0))
)
})

test_that("rskewlnorm has same mean and sd as rlnorm when shape = 0", {
  skip_if_not_installed("sn")
  withr::with_seed(
   101, 
   {
  skew <- rskewlnorm(1e5, 0.3, 0.7, 0)
  ln <- stats::rlnorm(1e5, 0.3, 0.7)
  expect_equal(mean(skew), mean(ln), tolerance = 0.05)
  expect_equal(sd(skew), sd(ln), tolerance = 0.05)
})
})

test_that("rskewlnorm returns expected output", {
  skip_if_not_installed("sn")
  set.seed(101)
  expect_equal(rskewlnorm(3, 0, 1, 2), c(1.71375069101727, 2.01285193758523, 2.23216981261934))
})
