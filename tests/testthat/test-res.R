test_that("res_pois", {
  expect_identical(res_pois(integer(0), integer(0)), numeric(0))
  expect_identical(res_pois(1, 1), 0)
  expect_equal(res_pois(0, 1), -1.4142135623731)
  expect_identical(res_pois(NA, 1), NA_real_)
  expect_identical(res_pois(1, NA), NA_real_)
  expect_error(res_pois(1, 3, type = "unknown"))
  expect_equal(res_pois(1, 3, type = "raw"), -2)
  expect_equal(res_pois(1, 3), dev_pois(1, 3, res = TRUE))
  expect_equal(
    res_pois(c(1, 3.5, 4), 3, type = "raw"),
    c(-2, 0.5, 1)
  )
  set.seed(101)
  expect_equal(res_pois(1:2, 2, simulate = TRUE, type = "raw"), c(-1L, -2L))
  expect_equal(res_pois(1:2, 2, simulate = TRUE), c(0.657868260861539, 0))
  set.seed(101)
  expect_equal(res_pois(1:2, 2, simulate = TRUE, type = "raw"), c(-1L, -2L))
  expect_equal(res_pois(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_pois(1:2, 2, simulate = TRUE, type = "data"), c(1L, 0L))
  set.seed(101)
  expect_equal(
    res_pois(1:2, 2, simulate = TRUE, type = "standardized"),
    c(-0.707106781186547, -1.4142135623731)
  )
})

test_that("res_pois simulate", {
  set.seed(101)
  res <- res_pois(rep(2, 10000), lambda = 10, simulate = TRUE, type = "dev")
  expect_equal(mean(res), -0.0747938731623342)
  expect_equal(sd(res), 1.00265783064155)
  set.seed(101)
  res <- res_pois(rep(2, 10000), lambda = 10, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), -0.0207445414507045)
  expect_equal(sd(res), 0.991366705510403)
})

test_that("res_pois_zi", {
  expect_identical(res_pois_zi(integer(0), integer(0)), numeric(0))
  expect_identical(res_pois_zi(1, 1), 0)
  expect_identical(res_pois_zi(1, 1, type = "raw"), 0)
  expect_equal(res_pois_zi(1, 1, 0.5), 1.17741002251547)
  expect_identical(res_pois_zi(1, 1, 0.5, type = "raw"), 0.5)
  expect_identical(res_pois_zi(1, lambda = 1, prob = 1), Inf)
  expect_identical(res_pois_zi(1, lambda = 1, prob = 1, type = "raw"), 1)
  expect_equal(res_pois_zi(0, 1), -1.4142135623731)
  expect_identical(res_pois_zi(NA, 1), NA_real_)
  expect_identical(res_pois_zi(1, NA), NA_real_)
  expect_error(res_pois_zi(1, 3, type = "unknown"))
  expect_identical(res_pois_zi(1, 3, type = "raw"), -2)
  expect_equal(res_pois_zi(1, 3), dev_pois(1, 3, res = TRUE))
  expect_equal(
    res_pois_zi(c(1, 3.5, 4), 3, type = "raw"),
    c(-2, 0.5, 1)
  )
  set.seed(101)
  expect_equal(res_pois_zi(1:2, 2, simulate = TRUE, type = "raw"), c(-1L, -2L))
  expect_equal(res_pois_zi(1:2, 2, simulate = TRUE), c(0.657868260861539, 0))
  set.seed(101)
  expect_equal(res_pois_zi(1:2, 2, simulate = TRUE, type = "raw"), c(-1L, -2L))
  expect_equal(res_pois_zi(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_pois_zi(1:2, 2, simulate = TRUE, type = "data"), c(1L, 0L))
  set.seed(101)
  expect_equal(
    res_pois_zi(1:10, 10, 0.5, simulate = TRUE, type = "data"),
    c(0L, 11L, 7L, 8L, 0L, 0L, 0L, 10L, 0L, 0L)
  )
  set.seed(101)
  expect_equal(
    res_pois_zi(1:10, 10, 0.5, simulate = TRUE, type = "standardized"),
    c(
      -0.912870929175277, 1.09544511501033, 0.365148371670111, 0.547722557505166,
      -0.912870929175277, -0.912870929175277, -0.912870929175277, 0.912870929175277,
      -0.912870929175277, -0.912870929175277
    )
  )
})

test_that("res_pois_zi simulate", {
  set.seed(101)
  res <- res_pois_zi(rep(2, 10000), lambda = 100, prob = 0.01, simulate = TRUE, type = "dev")
  expect_equal(mean(res), -0.0391353490642631)
  expect_equal(sd(res), 1.04793224317444)
  set.seed(101)
  res <- res_pois_zi(rep(2, 10000), lambda = 100, prob = 0.01, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.00328329103187647)
  expect_equal(sd(res), 0.971476934943575)
})

test_that("res_norm", {
  expect_identical(res_norm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_norm(0), 0)
  expect_identical(res_norm(NA, 1, 1), NA_real_)
  expect_identical(res_norm(1, NA, 1), NA_real_)
  expect_identical(res_norm(1, 1, NA), NA_real_)
  expect_equal(res_norm(-2), dev_norm(-2, res = TRUE))
  expect_equal(res_norm(-2:2), -2:2)
  expect_equal(res_norm(-2:2, 3, 2), c(-2.5, -2, -1.5, -1, -0.5))
  expect_equal(res_norm(-2:2, type = "raw"), -2:2)
  expect_equal(res_norm(-2:2, mean = 2, type = "raw"), -4:0)
  expect_equal(res_norm(-2:2, mean = -2:2, type = "raw"), rep(0, 5))
  set.seed(101)
  expect_equal(res_norm(1:2, 2, simulate = TRUE, type = "raw"), c(-0.326036490515386, 0.552461855419138))
  expect_equal(res_norm(1:2, 2, simulate = TRUE), c(-0.67494384395583, 0.214359459043425))
  set.seed(101)
  expect_equal(res_norm(1:2, 2, simulate = TRUE, type = "raw"), c(-0.326036490515386, 0.552461855419138))
  expect_equal(res_norm(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_norm(1:2, 2, simulate = TRUE, type = "data"), c(1.67396350948461, 2.55246185541914))
  set.seed(101)
  expect_equal(res_norm(1:2, 2, simulate = TRUE, type = "standardized"), c(-0.326036490515386, 0.552461855419138))
  expect_error(res_norm(10, type = "unknown"))
})

test_that("res_norm simulate", {
  set.seed(101)
  res <- res_norm(rep(2, 10000), simulate = TRUE, type = "dev")
  expect_equal(mean(res), 0.00527789283092907)
  expect_equal(sd(res), 0.993174129657874)
  set.seed(101)
  res <- res_norm(rep(2, 10000), simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.0052778928309291)
  expect_equal(sd(res), 0.993174129657875)
})

test_that("res_lnorm", {
  expect_identical(res_lnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_lnorm(exp(0)), 0)
  expect_identical(res_lnorm(1), 0)
  expect_identical(res_lnorm(0), -Inf)
  expect_identical(res_lnorm(-1), -Inf)
  expect_identical(res_lnorm(NA, 1, 1), NA_real_)
  expect_identical(res_lnorm(1, NA, 1), NA_real_)
  expect_identical(res_lnorm(1, 1, NA), NA_real_)
  expect_equal(res_lnorm(exp(-2:2)), -2:2)
  expect_equal(
    res_lnorm(exp(-2:2), meanlog = -1:3, sdlog = 1:5),
    c(-1, -0.5, -0.333333333333333, -0.25, -0.2)
  )
  expect_equal(res_lnorm(1, type = "raw"), 0)
  expect_equal(res_lnorm(exp(1), type = "raw"), 1.71828182845905)
  set.seed(101)
  expect_equal(res_lnorm(1:2, 2, simulate = TRUE, type = "raw"), c(-2.05579169361621, 5.44961576366372))
  expect_equal(res_lnorm(1:2, 2, simulate = TRUE), c(-0.67494384395583, 0.214359459043425))
  set.seed(101)
  expect_equal(res_lnorm(1:2, 2, simulate = TRUE, type = "raw"), c(-2.05579169361621, 5.44961576366372))
  expect_equal(res_lnorm(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_lnorm(1:2, 2, simulate = TRUE, type = "data"), c(5.33326440531444, 12.8386718625944))
  set.seed(101)
  expect_equal(res_lnorm(1:2, 2, simulate = TRUE, type = "standardized"), c(-0.428902244197459, 0.0410901945157245))
  set.seed(101)
  res <- res_lnorm(rep(2, 10000), simulate = TRUE, type = "standardized")
  expect_equal(mean(res), -0.000972725402704599)
  expect_equal(sd(res), 0.96810060574941)
  expect_error(res_lnorm(10, type = "unknown"))
})

test_that("res_binom", {
  expect_identical(res_binom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_binom(NA, 1, 1), NA_real_)
  expect_identical(res_binom(1, NA, 1), NA_real_)
  expect_identical(res_binom(1, 1, NA), NA_real_)
  expect_equal(res_binom(1, 3, 0.5), dev_binom(1, 3, 0.5, res = TRUE))
  expect_equal(res_binom(0, 1, 0.5), -1.17741002251547)
  expect_equal(res_binom(1, 1, 0.5), 1.17741002251547)
  expect_equal(res_binom(0, 1, 0.7), -1.55175565365552)
  expect_equal(res_binom(1, 1, 0.7), 0.844600430900592)
  expect_identical(res_binom(1, 2, 0.5), 0)
  expect_identical(res_binom(5, 10, 0.5), 0)
  expect_equal(res_binom(1, 10, 0.5), -2.71316865369073)
  expect_equal(
    res_binom(1:9, 10, 0.5),
    c(
      -2.71316865369073, -1.96338868806845, -1.28283185573988, -0.634594572159089,
      0, 0.634594572159089, 1.28283185573988, 1.96338868806845, 2.71316865369073
    )
  )
  expect_equal(res_binom(0, 2, 0.5, type = "raw"), -1)
  set.seed(101)
  expect_equal(res_binom(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_binom(1:2, 2, simulate = TRUE), c(0, 0))
  set.seed(101)
  expect_equal(res_binom(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_binom(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_binom(1:2, 2, simulate = TRUE, type = "data"), 1:0)
  set.seed(101)
  expect_equal(res_binom(1:2, 2, simulate = TRUE, type = "standardized"), c(0, -1.4142135623731))
  expect_error(res_binom(10, type = "unknown"))
})

test_that("res_binom simulate", {
  set.seed(101)
  res <- res_binom(rep(2, 10000), size = 10, simulate = TRUE, type = "dev")
  expect_equal(mean(res), 0.00262573802624805)
  expect_equal(sd(res), 1.02781369459726)
  set.seed(101)
  res <- res_binom(rep(2, 10000), 10, 0.5, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.00252982212813479)
  expect_equal(sd(res), 0.998525565686988)
})

test_that("res_bern", {
  expect_identical(res_bern(logical(0), integer(0)), numeric(0))
  expect_identical(res_bern(NA, 1), NA_real_)
  expect_identical(res_bern(1, NA), NA_real_)
  expect_identical(res_bern(1, 1), 0)
  expect_identical(res_bern(0, 0), 0)
  expect_identical(res_bern(1, 0), Inf)
  expect_identical(res_bern(0, 1), -Inf)
  expect_equal(res_bern(0, 0.5), dev_bern(0, 0.5, res = TRUE))
  expect_identical(res_bern(0, 1), -Inf)
  expect_identical(
    res_bern(c(1, 1, 0, 0), c(0, 1, 0, 1)),
    c(Inf, 0, 0, -Inf)
  )
  expect_equal(
    res_bern(c(1, 0), 0.5),
    c(1.17741002251547, -1.17741002251547)
  )
  expect_equal(
    res_bern(c(1, 0), 0.7),
    c(0.844600430900592, -1.55175565365552)
  )
  expect_equal(
    res_bern(c(1, 0), c(0.7, 0.5)),
    c(0.844600430900592, -1.17741002251547)
  )
  expect_equal(res_bern(c(0, 1), c(1, 0), type = "raw"), c(-1, 1))
  set.seed(101)
  expect_equal(res_bern(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_bern(1:2, simulate = TRUE), c(1.17741002251547, 1.17741002251547))
  set.seed(101)
  expect_equal(res_bern(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_bern(1:2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_bern(0:1, simulate = TRUE, type = "data"), c(0L, 0L))
  set.seed(101)
  expect_equal(res_bern(0:1, simulate = TRUE, type = "standardized"), c(-1L, -1L))
  expect_error(res_bern(0, type = "unknown"))
})

test_that("res_bern simulate", {
  set.seed(101)
  res <- res_bern(rep(2, 10000), simulate = TRUE, type = "dev")
  expect_equal(mean(res), -0.00659349612608666)
  expect_equal(sd(res), 1.17745043457519)
  set.seed(101)
  res <- res_bern(rep(2, 10000), simulate = TRUE, type = "standardized")
  expect_equal(mean(res), -0.00559999999999963)
  expect_equal(sd(res), 1.00003432284339)
})

test_that("res_gamma_pois", {
  expect_identical(res_gamma_pois(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_gamma_pois(1, 1, 0), 0)
  expect_equal(res_gamma_pois(0, 1, 0), -1.4142135623731)
  expect_identical(res_gamma_pois(1, 1, 1), 0)
  expect_identical(res_gamma_pois(NA, 1, 1), NA_real_)
  expect_identical(res_gamma_pois(1, NA, 1), NA_real_)
  expect_identical(res_gamma_pois(1, 1, NA), NA_real_)
  expect_error(res_gamma_pois(1, 3, 1, type = "unknown"))
  expect_equal(res_gamma_pois(1, 3, 1, type = "raw"), -2)
  expect_equal(res_gamma_pois(1, 3, 2), dev_gamma_pois(1, 3, 2, res = TRUE))
  expect_equal(
    res_gamma_pois(c(1, 3.5, 4), 3, 10, type = "raw"),
    c(-2, 0.5, 1)
  )
  set.seed(101)
  expect_identical(res_gamma_pois(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_gamma_pois(1:2, 2, 2, simulate = TRUE), c(0.699911676381084, -0.382338214383655))
  set.seed(101)
  expect_identical(res_gamma_pois(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_gamma_pois(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_gamma_pois(1:2, 2, simulate = TRUE, type = "data"), 2:1)
  set.seed(101)
  expect_equal(res_gamma_pois(1:2, 2, simulate = TRUE, type = "standardized"), c(0, -0.707106781186547))
})

test_that("res_gamma_pois simulate", {
  set.seed(101)
  res <- res_gamma_pois(rep(2, 10000), lambda = 100, theta = 0.5, simulate = TRUE, type = "dev")
  expect_equal(mean(res), -0.250801462240149)
  expect_equal(sd(res), 1.01494516351537)
  set.seed(101)
  res <- res_gamma_pois(rep(2, 10000), lambda = 100, theta = 0.5, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), -0.00860612139643616)
  expect_equal(sd(res), 0.999023530246265)
})

test_that("res_neg_binom", {
  expect_identical(res_neg_binom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_neg_binom(1, 1, 0), 0)
  expect_equal(res_neg_binom(0, 1, 0), -1.4142135623731)
  expect_identical(res_neg_binom(1, 1, 1), 0)
  expect_identical(res_neg_binom(NA, 1, 1), NA_real_)
  expect_identical(res_neg_binom(1, NA, 1), NA_real_)
  expect_identical(res_neg_binom(1, 1, NA), NA_real_)
  expect_error(res_neg_binom(1, 3, 1, type = "unknown"))
  expect_equal(res_neg_binom(1, 3, 1, type = "raw"), -2)
  expect_equal(res_neg_binom(1, 3, 2), dev_gamma_pois(1, 3, 2, res = TRUE))
  expect_equal(
    res_neg_binom(c(1, 3.5, 4), 3, 10, type = "raw"),
    c(-2, 0.5, 1)
  )
  set.seed(101)
  expect_identical(res_neg_binom(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_neg_binom(1:2, 2, 2, simulate = TRUE), c(0.699911676381084, -0.382338214383655))
  set.seed(101)
  expect_identical(res_neg_binom(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_neg_binom(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_neg_binom(1:2, 2, simulate = TRUE, type = "data"), 2:1)
  set.seed(101)
  expect_equal(res_neg_binom(1:2, 2, simulate = TRUE, type = "standardized"), c(0, -0.707106781186547))
  set.seed(101)
  res <- res_neg_binom(rep(2, 10000), 2, 2, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), -0.0100560429593354)
  expect_equal(sd(res), 0.973207885992976)
})

test_that("res_gamma_pois_zi", {
  expect_identical(res_gamma_pois_zi(integer(0), integer(0), integer(0)), numeric(0), numeric(0))
  expect_identical(res_gamma_pois_zi(1, 1, 0, 0), 0)
  expect_identical(res_gamma_pois_zi(1, 1, 0, 0, type = "raw"), 0)
  expect_equal(res_gamma_pois_zi(1, 1, 0, 0.5), 1.17741002251547)
  expect_equal(res_gamma_pois_zi(1, 1, 0, 0.5, type = "raw"), 0.5)
  expect_identical(res_gamma_pois_zi(1, 1, 0, 1), Inf)
  expect_equal(res_gamma_pois_zi(1, 1, 0, 1, type = "raw"), 1)
  expect_equal(res_gamma_pois_zi(0, 1, 0), -1.4142135623731)
  expect_equal(res_gamma_pois_zi(0, 1, 0, type = "raw"), -1)
  expect_identical(res_gamma_pois_zi(1, 1, 1), 0)
  expect_identical(res_gamma_pois_zi(1, 1, 1, type = "raw"), 0)
  expect_identical(res_gamma_pois_zi(NA, 1, 1, 1), NA_real_)
  expect_identical(res_gamma_pois_zi(1, NA, 1, 1), NA_real_)
  expect_identical(res_gamma_pois_zi(1, 1, NA, 1), NA_real_)
  expect_identical(res_gamma_pois_zi(1, 1, 1, NA), NA_real_)
  expect_error(res_gamma_pois_zi(1, 3, 1, type = "unknown"))
  expect_equal(res_gamma_pois_zi(1, 3, 1, type = "raw"), -2)
  expect_equal(res_gamma_pois_zi(1, 3), dev_pois(1, 3, res = TRUE))
  expect_equal(res_gamma_pois_zi(1, 3, 2), dev_gamma_pois(1, 3, 2, res = TRUE))
  #  expect_equal(res_gamma_pois_zi(1, 3, prob = 0.5), dev_pois_zi(1, 3, prob = 0.5, res = TRUE))
  expect_equal(
    res_gamma_pois_zi(c(1, 3.5, 4), 3, 10, type = "raw"),
    c(-2, 0.5, 1)
  )
  expect_equal(
    res_gamma_pois_zi(c(1, 3.5, 4), 3, 10, 0.5, type = "raw"),
    c(-0.5, 2, 2.5)
  )
  set.seed(101)
  expect_identical(res_gamma_pois_zi(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_gamma_pois_zi(1:2, 2, 2, simulate = TRUE), c(0.699911676381084, -0.382338214383655))
  set.seed(101)
  expect_identical(res_gamma_pois_zi(1:2, 2, 2, 0.5, simulate = TRUE, type = "raw"), c(0, 3))
  expect_equal(res_gamma_pois_zi(1:2, 2, 2, 0.5, simulate = TRUE), c(1.36973381204323, -0.80437196763369))
  set.seed(101)
  expect_identical(res_gamma_pois_zi(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-1, 2))
  expect_equal(res_gamma_pois_zi(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_identical(res_gamma_pois_zi(1:2, 2, 2, 0.5, simulate = TRUE, type = "raw"), c(0, 3))
  expect_equal(res_gamma_pois_zi(1:2, 2, prob = 0.5, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_gamma_pois_zi(1:2, 2, simulate = TRUE, type = "data"), 2:1)
  set.seed(101)
  expect_equal(res_gamma_pois_zi(1:2, 2, prob = 0.7, simulate = TRUE, type = "data"), c(0L, 0L))
  set.seed(101)
  expect_equal(
    res_gamma_pois_zi(1:2, 2, prob = 0.7, simulate = TRUE, type = "standardized"),
    c(-0.424264068711929, -0.424264068711929)
  )
  set.seed(101)
  res <- res_gamma_pois_zi(rep(2, 10000), 2, prob = 0.7, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.00593969696196716)
  expect_equal(sd(res), 0.860406154019735)
})

test_that("res_gamma", {
  expect_identical(res_gamma(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_gamma(1, 1, 1), 0)
  expect_identical(res_gamma(1, 1, 1, type = "raw"), 0)
  expect_equal(res_gamma(1, 1, 0.5, type = "raw"), -1)
  expect_equal(res_gamma(1, 1, 0.5), -0.621525833026987)
  expect_identical(res_gamma(2, 2, 2, type = "raw"), 1)
  expect_equal(res_gamma(0, 1, 0, type = "raw"), -Inf)
  expect_identical(res_gamma(NA, 1, 1), NA_real_)
  expect_identical(res_gamma(1, NA, 1), NA_real_)
  expect_identical(res_gamma(1, 1, NA), NA_real_)
  expect_error(res_gamma(1, 3, 1, type = "unknown"))
  expect_equal(res_gamma(1, 3, 1, type = "raw"), -2)

  expect_equal(
    res_gamma(c(1, 3.5, 4), 3, 10, type = "raw"),
    c(0.7, 3.2, 3.7)
  )
  expect_equal(
    res_gamma(c(1, 3.5, 4), 3, 5, type = "raw"),
    c(0.4, 2.9, 3.4)
  )
  set.seed(101)
  expect_equal(res_gamma(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-0.436368285679167, 0.0193475947038637))
  expect_equal(res_gamma(1:2, 2, 2, simulate = TRUE), c(-0.682136415630992, -0.0483989592818978))
  set.seed(101)
  expect_equal(res_gamma(1:2, 2, 0.5, simulate = TRUE, type = "raw"), c(-1.74547314271667, 0.0773903788154549))
  expect_equal(res_gamma(1:2, 2, 0.5, simulate = TRUE), c(-0.682136415630992, -0.0483989592818978))
  set.seed(101)
  expect_equal(res_gamma(1:2, 2, 1, simulate = TRUE, type = "data"), c(1.12726342864167, 2.03869518940773))
  set.seed(101)
  expect_equal(res_gamma(1:2, 2, 1, simulate = TRUE, type = "standardized"), c(-0.617117947796975, 0.0273616308295019))
  set.seed(101)
  res <- res_gamma(rep(2, 10000), 2, 1, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.00754439840612374)
  expect_equal(sd(res), 1.00440120529188)
})

test_that("res_student", {
  expect_identical(res_student(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_student(1, 1, 0, 0), NaN)
  expect_identical(res_student(1, 1, 0.1, 0, type = "raw"), 0)
  expect_equal(res_student(2, 1, 1, 0.5), 1.10290313460634)
  expect_identical(res_student(2, 1, 1, 0.5, type = "raw"), 1)
  expect_identical(res_student(1, 1, 1, 1), 0)
  expect_identical(res_student(1, 0, 1, 1, type = "raw"), 1)
  expect_equal(res_student(0, 1, 1), -1)
  expect_equal(res_student(0, 1, 1, type = "raw"), -1)
  expect_identical(res_student(1, 1, 1), 0)
  expect_identical(res_student(1, 1, 1, type = "raw"), 0)
  expect_identical(res_student(NA, 1, 1, 1), NA_real_)
  expect_identical(res_student(1, NA, 1, 1), NA_real_)
  expect_identical(res_student(1, 1, NA, 1), NA_real_)
  expect_identical(res_student(1, 1, 1, NA), NA_real_)
  expect_error(res_student(1, 3, 1, type = "unknown"))
  expect_equal(res_student(1, 3, 1, type = "raw"), -2)
  expect_equal(res_student(1, 3, 2), dev_student(1, 3, 2, res = TRUE))
  expect_equal(res_student(1, 3, 2), res_norm(1, 3, 2))
  expect_equal(
    res_student(c(1, 3.5, 4), 3, 10, type = "raw"),
    c(-2, 0.5, 1)
  )
  expect_equal(
    res_student(c(1, 3.5, 4), 3, 10, 0.5, type = "raw"),
    c(-2, 0.5, 1)
  )
  set.seed(101)
  expect_equal(res_student(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-0.652072981030771, 1.10492371083828))
  expect_equal(res_student(1:2, 2, 2, simulate = TRUE), c(-0.67494384395583, 0.214359459043425))
  set.seed(101)
  expect_equal(res_student(1:2, 2, 2, 0.5, simulate = TRUE, type = "raw"), c(-0.663122114744059, -1.65775518832743))
  expect_equal(res_student(1:2, 2, 2, 0.5, simulate = TRUE), c(0.292028638517356, 1.05785074161446))
  set.seed(101)
  expect_equal(res_student(1:2, 2, 2, simulate = TRUE, type = "raw"), c(-0.652072981030771, 1.10492371083828))
  expect_equal(res_student(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_student(1:2, 2, 2, 0.5, simulate = TRUE, type = "raw"), c(-0.663122114744059, -1.65775518832743))
  expect_equal(res_student(1:2, 2, 0.5, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_student(1:2, 2, simulate = TRUE, type = "data"), c(1.67396350948461, 2.55246185541914))
  set.seed(101)
  expect_equal(res_student(1:2, 2, theta = 0.7, simulate = TRUE, type = "data"), c(1.38213662305365, 2.14557389211128))
  set.seed(101)
  expect_equal(res_student(1:2, 0, 0), res_norm(1:2, 0, 0))
  expect_equal(res_student(1:2, 0, 0, 10), res_norm(1:2, 0, 0))
  expect_equal(res_student(NA, 6, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_student(5, NA, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_student(5, 6, NA, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_student(5, 6, 0.5, NA, type = "standardized"), NA_real_)
  expect_equal(res_student(NA, NA, NA, NA, type = "standardized"), NA_real_)
  expect_equal(res_student(numeric(0), numeric(0), numeric(0), numeric(0), type = "standardized"), numeric(0))
  set.seed(101)
  expect_equal(
    res_student(1:2, 5, 10, 1 / 5, simulate = TRUE, type = "standardized"),
    c(-0.236216877861339, -0.543340118407321)
  )
  set.seed(101)
  expect_equal(
    res_student(1:3, 5, 10, 0, simulate = TRUE, type = "standardized"),
    c(-0.326036490515386, 0.552461855419139, -0.67494384395583)
  )
  set.seed(101)
  expect_equal(res_student(1:3, 5, 10, 1, simulate = TRUE, type = "standardized"), rep(NaN, 3))
  set.seed(101)
  expect_equal(res_student(1:3, 5, 10, 1 / 1.5, simulate = TRUE, type = "standardized"), rep(0, 3))
  set.seed(101)
  expect_equal(
    res_student(1:3, 5, 10, 1 / 5, simulate = TRUE, type = "standardized"),
    c(-0.236216877861339, -0.543340118407321, 0.190192700158201)
  )
  set.seed(101)
  res <- res_student(rep(2, 10000), 5, 10, 1 / 5, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.000136223967493112)
  expect_equal(sd(res), 1.01150399649323)
})

test_that("res_beta_binom", {
  expect_identical(res_beta_binom(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_beta_binom(NA, 1, 1, 1), NA_real_)
  expect_identical(res_beta_binom(1, NA, 1, 1), NA_real_)
  expect_identical(res_beta_binom(1, 1, NA, 1), NA_real_)
  expect_identical(res_beta_binom(1, 1, 1, NA), NA_real_)
  expect_equal(res_beta_binom(1, 3, 0.5), dev_beta_binom(1, 3, 0.5, res = TRUE))
  expect_equal(res_beta_binom(1, 3, 0.5, 1.0), dev_beta_binom(1, 3, 0.5, 1.0, res = TRUE))
  expect_equal(res_beta_binom(0, 1, 0.5), -1.17741002251547)
  expect_equal(res_beta_binom(1, 1, 0.5), 1.17741002251547)
  expect_equal(res_beta_binom(0, 1, 0.7), -1.55175565365552)
  expect_equal(res_beta_binom(1, 1, 0.7), 0.844600430900592)
  expect_equal(res_beta_binom(0, 2, 0.5, 0.5), -1.55175564931989)
  expect_equal(res_beta_binom(1, 2, 0.5, 0.5), 2.1073424255447e-08)
  expect_equal(res_beta_binom(0, 2, 0.7, 0.5), -2.01243800222306)
  expect_equal(res_beta_binom(1, 2, 0.7, 0.5), 0.590513991612016)
  expect_identical(res_beta_binom(1, 2, 0.5), 0)
  expect_identical(res_beta_binom(5, 10, 0.5), 0)
  expect_identical(res_beta_binom(1, 2, 0.5, 2), 0)
  expect_identical(res_beta_binom(5, 10, 0.5, 2), 0)
  expect_equal(res_beta_binom(1, 10, 0.5), -2.71316865369073)
  expect_equal(res_beta_binom(1, 10, 0.5, 0.5), -1.48165822226217)
  expect_equal(
    res_beta_binom(1:9, 10, 0.5),
    c(
      -2.71316865369073, -1.96338868806845, -1.28283185573988, -0.634594572159089,
      0, 0.634594572159089, 1.28283185573988, 1.96338868806845, 2.71316865369073
    )
  )
  expect_equal(
    res_beta_binom(1:9, 10, 0.5, 0.1),
    c(
      -2.26648244134579, -1.61504974877637, -1.04572341572075, -0.51475735786997,
      0, 0.514757357869973, 1.04572341572074, 1.61504974877637, 2.26648244134578
    )
  )
  expect_equal(res_beta_binom(0, 2, 0.5, type = "dev"), -1.6651092223154)
  expect_equal(res_beta_binom(0, 2, 0.5, 10, type = "dev"), -1.24912653737637)
  expect_equal(res_beta_binom(0, 2, 0.5, type = "raw"), -1)
  expect_equal(res_beta_binom(0, 2, 0.5, 10, type = "raw"), -1)
  set.seed(101)
  expect_equal(res_beta_binom(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_beta_binom(1:2, 2, simulate = TRUE), c(0, 0))
  set.seed(101)
  expect_equal(res_beta_binom(1:2, theta = 2, simulate = TRUE, type = "raw"), c(0.5, -0.5))
  expect_equal(res_beta_binom(1:2, 2, theta = 2, simulate = TRUE), c(0, 0))
  set.seed(101)
  expect_equal(res_beta_binom(1:2, simulate = TRUE, type = "raw"), c(-0.5, -0.5))
  expect_equal(res_beta_binom(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_beta_binom(1:2, theta = 2, simulate = TRUE, type = "raw"), c(0.5, -0.5))
  expect_equal(res_beta_binom(1:2, 2, theta = 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_beta_binom(1:2, 2, simulate = TRUE, type = "data"), 1:0)
  expect_equal(res_beta_binom(1:2, 2, theta = 2, simulate = TRUE, type = "data"), c(0, 2))
  set.seed(101)
  expect_equal(
    res_beta_binom(1:2, 2, prob = 0.5, theta = 2, simulate = TRUE, type = "standardized"),
    c(0.816496580927726, -0.816496580927726)
  )
  expect_equal(res_beta_binom(NA, 6, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_beta_binom(5, NA, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_beta_binom(5, 6, NA, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_beta_binom(5, 6, 0.5, NA, type = "standardized"), NA_real_)
  expect_equal(res_beta_binom(NA, NA, NA, NA, type = "standardized"), NA_real_)
  expect_equal(res_beta_binom(numeric(0), numeric(0), numeric(0), numeric(0), type = "standardized"), numeric(0))
  set.seed(101)
  expect_equal(
    res_beta_binom(1:2, 2, prob = 0.5, theta = 0, simulate = TRUE, type = "standardized"),
    c(0, -1.4142135623731)
  )
  set.seed(101)
  res <- res_beta_binom(rep(2, 10000), size = 10, prob = 0.3, theta = 0.2, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.0345591969703882)
  expect_equal(sd(res), 2.24030672587351)
  expect_error(res_beta_binom(10, type = "unknown"))
})

test_that("res_skewnorm", {
  skip_if_not_installed("sn")
  expect_identical(res_skewnorm(integer(0), integer(0), integer(0)), numeric(0))
  expect_identical(res_skewnorm(NA, 1, 1, 1), NA_real_)
  expect_identical(res_skewnorm(1, NA, 1, 1), NA_real_)
  expect_identical(res_skewnorm(1, 1, NA, 1), NA_real_)
  expect_identical(res_skewnorm(1, 1, 1, NA), NA_real_)
  expect_equal(res_skewnorm(1, 3, 0.5), dev_skewnorm(1, 3, 0.5, res = TRUE))
  expect_equal(res_skewnorm(1, 3, 0.5, 1.0), dev_skewnorm(1, 3, 0.5, 1.0, res = TRUE))
  expect_equal(res_skewnorm(0, 1, 0.5), -2)
  expect_equal(res_skewnorm(1, 1, 0.5), 0)
  expect_equal(res_skewnorm(0, 1, 0.7), -1.42857142857143)
  expect_equal(res_skewnorm(1, 1, 0.7), 0)
  expect_equal(res_skewnorm(0, 2, 0.5, 0.5), -4.72416743525476)
  expect_equal(res_skewnorm(1, 2, 0.5, 1), -3.2524653138693)
  expect_equal(res_skewnorm(0, 2, 0.7, -4), -2.80920319431421)
  expect_equal(res_skewnorm(1, 2, 0.7, -2), -1.20493839504062)
  expect_equal(res_skewnorm(1, 2, 0.5), -2)
  expect_equal(res_skewnorm(5, 10, 0.5), -10)
  expect_equal(res_skewnorm(1, 2, 0.5, -2), -1.84576386463206)
  expect_equal(res_skewnorm(5, 10, 5, 2), -2.82367660468737)
  expect_equal(res_skewnorm(1, 10, 10), res_norm(1, 10, 10))
  expect_equal(res_skewnorm(1, 10, 0.5, 0.5), -20.2486465600944)
  expect_equal(
    res_skewnorm(1:9, 10, 0.5, 10),
    c(
      -180.931340047915, -160.835052963763, -140.739643395107, -120.645509483193,
      -100.553340951113, -80.4644530321068, -60.3817280342612, -40.3130604527845,
      -20.2918769408978
    )
  )
  expect_equal(
    res_skewnorm(1:9, 10, 0.5, -10),
    c(
      -17.9979431892188, -15.9976860527498, -13.9973554303071, -11.9969145634345,
      -9.99629726660561, -7.99537110097754, -5.99382674443854, -3.99073414829236,
      -1.98140330128592
    )
  )
  expect_equal(res_skewnorm(0, 2, 0.5, type = "raw"), -2)
  expect_equal(res_skewnorm(0, 2, 0.5, 10, type = "raw"), -1.60303759425339)
  set.seed(101)
  expect_equal(res_skewnorm(1:2, simulate = TRUE, type = "raw"), c(0.552461855419139, 0.214359459043425))
  expect_equal(res_skewnorm(1:2, 2, simulate = TRUE), c(1.1739662875627, -0.112734314754215))
  set.seed(101)
  expect_equal(res_skewnorm(1:2, shape = 2, simulate = TRUE, type = "raw"), c(1.25233400157483, 1.41320223730144))
  expect_equal(res_skewnorm(1:2, 2, shape = 2, simulate = TRUE), c(0.403578184606959, -0.0430501884150838))
  set.seed(101)
  expect_equal(res_skewnorm(1:2, simulate = TRUE, type = "raw"), c(0.552461855419139, 0.214359459043425))
  expect_equal(res_skewnorm(1:2, 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_skewnorm(1:2, shape = 2, simulate = TRUE, type = "raw"), c(1.25233400157483, 1.41320223730144))
  expect_equal(res_skewnorm(1:2, 2, shape = 2, type = "data"), 1:2)
  set.seed(101)
  expect_equal(res_skewnorm(1:2, 2, simulate = TRUE, type = "data"), c(2.55246185541914, 2.21435945904343))
  expect_equal(res_skewnorm(1:2, 2, shape = 2, simulate = TRUE, type = "data"), c(2.8029741225477, 2.50304615414932))
  set.seed(101)
  expect_equal(
    res_skewnorm(1:2, 2, sd = 0.5, shape = 2, simulate = TRUE, type = "standardized"),
    c(-0.713119218167245, -0.0574564315318186)
  )
  expect_equal(res_skewnorm(NA, 6, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_skewnorm(5, NA, 0.5, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_skewnorm(5, 6, NA, 0.2, type = "standardized"), NA_real_)
  expect_equal(res_skewnorm(5, 6, 0.5, NA, type = "standardized"), NA_real_)
  expect_equal(res_skewnorm(NA, NA, NA, NA, type = "standardized"), NA_real_)
  expect_equal(res_skewnorm(numeric(0), numeric(0), numeric(0), numeric(0), type = "standardized"), numeric(0))
  set.seed(101)
  expect_equal(
    res_skewnorm(1:2, 2, sd = 0.5, shape = 0, simulate = TRUE, type = "standardized"),
    c(1.10492371083828, 0.428718918086851)
  )
  set.seed(101)
  res <- res_skewnorm(rep(2, 10000), mean = 10, sd = 0.3, shape = 0.2, simulate = TRUE, type = "standardized")
  expect_equal(mean(res), 0.0236707225149864)
  expect_equal(sd(res), 3.34458775450197)
  expect_error(res_skewnorm(10, type = "unknown"))
})
