test_that("ran_pois", {
  expect_error(ran_pois(NA_integer_))
  expect_error(ran_pois(integer(0)))
  expect_identical(ran_pois(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_pois(), 1L)
    expect_identical(ran_pois(2), c(0L, 1L))
    expect_identical(ran_pois(2, 10), c(11L, 8L))
    expect_identical(ran_pois(2, c(0.1, 100)), c(0L, 103L))
    expect_identical(ran_pois(1, c(0.1, 100)), 0L)
  })
})

test_that("ran_pois_zi", {
  expect_error(ran_pois_zi(NA_integer_))
  expect_error(ran_pois_zi(integer(0)))
  expect_identical(ran_pois_zi(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_pois_zi(), 1L)
    expect_identical(ran_pois_zi(2), c(0L, 1L))
    expect_identical(ran_pois_zi(2, 10), c(11L, 8L))
    expect_identical(ran_pois_zi(2, c(0.1, 100)), c(0L, 103L))
    expect_identical(ran_pois_zi(1, c(0.1, 100)), 0L)
  })
  withr::with_seed(101, {
    expect_identical(
      ran_pois_zi(10, 10, 0.5),
      c(0L, 11L, 7L, 8L, 0L, 0L, 0L, 10L, 0L, 0L)
    )
  })
})

test_that("ran_norm", {
  expect_error(ran_norm(NA_integer_))
  expect_error(ran_norm(integer(0)))
  expect_identical(ran_norm(0L), numeric(0))
  withr::with_seed(101, {
    expect_equal(ran_norm(), -0.326036490515386)
    expect_equal(ran_norm(2), c(0.552461855419139, -0.67494384395583))
    expect_equal(ran_norm(2, 10), c(10.2143594590434, 10.3107692173136))
    expect_equal(ran_norm(2, c(0.1, 100)), c(1.2739662875627, 100.618789855626))
    expect_equal(ran_norm(1, c(0.1, 100)), -0.0127343147542145)
  })
})

test_that("ran_lnorm", {
  expect_error(ran_lnorm(NA_integer_))
  expect_error(ran_lnorm(integer(0)))
  expect_identical(ran_lnorm(0L), numeric(0))
  withr::with_seed(101, {
    expect_equal(ran_lnorm(), 0.721778848868975)
    expect_equal(ran_lnorm(2), c(1.73752529290614, 0.509185013620822))
    expect_equal(ran_lnorm(2, 10), c(27292.2882352846, 30054.546228237))
    expect_equal(
      ran_lnorm(2, c(0.1, 100)),
      c(3.57500397331701, 4.99097288094876e+43)
    )
    expect_equal(ran_lnorm(1, c(0.1, 100)), 0.987346423552865)
  })
})

test_that("ran_binom", {
  expect_error(ran_binom(NA_integer_))
  expect_error(ran_binom(integer(0)))
  expect_identical(ran_binom(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_binom(), 0L)
    expect_identical(ran_binom(2), c(0L, 1L))
    expect_identical(ran_binom(2, 10), c(6L, 4L))
    expect_identical(ran_binom(2, c(2, 100)), c(1L, 47L))
    expect_identical(ran_binom(1, c(2, 100)), 1L)
    expect_identical(ran_binom(1, 100, 0.1), 10L)
  })
})

test_that("ran_bern", {
  expect_error(ran_bern(NA_integer_))
  expect_error(ran_bern(integer(0)))
  expect_identical(ran_bern(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_bern(), 0L)
    expect_identical(ran_bern(2), c(0L, 1L))
    expect_identical(ran_bern(2, c(0.1, 1)), c(0L, 1L))
    expect_identical(ran_bern(1, c(0.1, 1)), 0L)
  })
})

test_that("ran_gamma_pois", {
  expect_error(ran_gamma_pois(NA_integer_))
  expect_error(ran_gamma_pois(integer(0)))
  expect_identical(ran_gamma_pois(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois(), 1L)
    expect_identical(ran_gamma_pois(2), c(0L, 2L))
    expect_identical(ran_gamma_pois(2, 10), c(14L, 7L))
    expect_identical(ran_gamma_pois(2, c(0.1, 100)), c(0L, 108L))
    expect_identical(ran_gamma_pois(1, c(0.1, 100)), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois(theta = 10), 0L)
    expect_identical(ran_gamma_pois(2, theta = 10), c(1L, 2L))
    expect_identical(ran_gamma_pois(2, 10, theta = 10), c(0L, 0L))
    expect_identical(ran_gamma_pois(2, 10, theta = 10), c(7L, 13L))
    expect_identical(ran_gamma_pois(2, c(0.1, 100), theta = 1), c(0L, 12L))
    expect_identical(ran_gamma_pois(2, c(0.1, 100), theta = 1), c(0L, 117L))
    expect_identical(ran_gamma_pois(1, c(0.1, 100), theta = 1), 0L)
  })
})

test_that("ran_multinom", {
  expect_identical(
    ran_multinom(size = numeric(0), prob = numeric(0), group = numeric(0)),
    integer(0)
  )
  # a mismatched, non-recyclable length errors clearly instead of being
  # silently (and wrongly) recycled by rep_len()
  expect_error(
    ran_multinom(size = c(10, 10, 10), prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1, 2, 2)),
    "must be all zero length or the same length"
  )
  expect_error(
    ran_multinom(size = c(10, 5), prob = c(0.5, 0.5), group = c(1, 1)),
    "`size` must be the same for every row belonging to the same `group`"
  )
  expect_error(
    ran_multinom(size = 10, prob = c(0.5, 0.4), group = c(1, 1)),
    "`prob` must sum to 1 for every `group`"
  )
  expect_error(
    ran_multinom(size = 10, prob = 1, group = 1),
    "must contain at least 2 rows"
  )
  expect_error(
    ran_multinom(
      size = c(10, 10, 10, 10),
      prob = c(0.2, 0.3, 0.5, 1),
      group = c(1, 1, 1, 2)
    ),
    "must contain at least 2 rows"
  )
  # every group must have the same number of rows (categories) as the most
  # common number of rows per group in the data (ordinary multinomial
  # logistic regression assumes a fixed set of categories for every trial)
  expect_error(
    ran_multinom(
      size = c(10, 10, 10, 10, 10, 10, 6, 6),
      prob = c(0.2, 0.3, 0.5, 0.2, 0.3, 0.5, 0.5, 0.5),
      group = c(1, 1, 1, 2, 2, 2, 3, 3)
    ),
    "Every `group` should have the same number of rows"
  )
  # group must not contain NA -- there's no way to know which trial an
  # unlabelled row belongs to
  expect_error(
    ran_multinom(size = c(10, 10, 10), prob = c(0.4, 0.6, 1), group = c(1, 1, NA)),
    "must not have any missing values"
  )
  expect_error(
    ran_multinom(size = c(10, 10), prob = c(0.4, 0.6), group = c(NA, NA)),
    "must not have any missing values"
  )
  # NA in size or prob is a structural input shared by the whole trial (the
  # categories aren't independent draws), so it makes the whole trial's
  # result NA, not just the row where the NA appears
  expect_identical(
    ran_multinom(size = c(10, NA), prob = c(0.4, 0.6), group = c(1, 1)),
    c(NA_integer_, NA_integer_)
  )
  expect_identical(
    ran_multinom(size = c(10, 10), prob = c(0.4, NA), group = c(1, 1)),
    c(NA_integer_, NA_integer_)
  )
  # an NA elsewhere doesn't leak into an unrelated, fully-known group (same
  # number of categories in both groups, since groups must match on that)
  withr::with_seed(101, {
    x <- ran_multinom(
      size = c(10, 10, NA, 6, 6, 6),
      prob = c(0.2, 0.3, 0.5, 0.2, 0.3, 0.5),
      group = c(1, 1, 1, 2, 2, 2)
    )
    expect_identical(x[1:3], c(NA_integer_, NA_integer_, NA_integer_))
    expect_identical(sum(x[4:6]), 6L)
  })
  withr::with_seed(101, {
    x <- ran_multinom(size = 10, prob = c(0.2, 0.3, 0.5), group = c(1, 1, 1))
    expect_identical(sum(x), 10L)
    expect_length(x, 3L)
  })
  withr::with_seed(101, {
    x <- ran_multinom(
      size = c(10, 10, 6, 6),
      prob = c(0.2, 0.8, 0.5, 0.5),
      group = c(1, 1, 2, 2)
    )
    expect_identical(x[1] + x[2], 10L)
    expect_identical(x[3] + x[4], 6L)
  })
})
test_that("ran_multinom with two categories matches ran_binom", {
  size <- c(10, 4, 20)
  prob <- c(0.2, 0.5, 0.75)
  # rmultinom() draws a two-category trial with a single rbinom() call, so
  # the first category is stream-identical to ran_binom()
  withr::with_seed(42, {
    x <- ran_multinom(
      size = rep(size, each = 2),
      prob = as.vector(rbind(prob, 1 - prob)),
      group = rep(seq_along(size), each = 2)
    )
  })
  withr::with_seed(42, {
    y <- ran_binom(length(size), size = size, prob = prob)
  })
  expect_identical(matrix(x, nrow = 2)[1, ], y)

  # and the marginal of a category is binomial, so its mean and variance
  # match size * prob and size * prob * (1 - prob)
  n_group <- 20000
  withr::with_seed(7, {
    x <- ran_multinom(
      size = rep(10, 2 * n_group),
      prob = rep(c(0.3, 0.7), n_group),
      group = rep(seq_len(n_group), each = 2)
    )
  })
  first <- matrix(x, nrow = 2)[1, ]
  expect_equal(mean(first), 10 * 0.3, tolerance = 0.01)
  expect_equal(var(first), 10 * 0.3 * 0.7, tolerance = 0.01)
})

test_that("ran_neg_binom", {
  expect_error(ran_neg_binom(NA_integer_))
  expect_error(ran_neg_binom(integer(0)))
  expect_identical(ran_neg_binom(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_neg_binom(), 1L)
    expect_identical(ran_neg_binom(2), c(0L, 2L))
    expect_identical(ran_neg_binom(2, 10), c(14L, 7L))
    expect_identical(ran_neg_binom(2, c(0.1, 100)), c(0L, 108L))
    expect_identical(ran_neg_binom(1, c(0.1, 100)), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_neg_binom(theta = 10), 0L)
    expect_identical(ran_neg_binom(2, theta = 10), c(1L, 2L))
    expect_identical(ran_neg_binom(2, 10, theta = 10), c(0L, 0L))
    expect_identical(ran_neg_binom(2, 10, theta = 10), c(7L, 13L))
    expect_identical(ran_neg_binom(2, c(0.1, 100), theta = 1), c(0L, 12L))
    expect_identical(ran_neg_binom(2, c(0.1, 100), theta = 1), c(0L, 117L))
    expect_identical(ran_neg_binom(1, c(0.1, 100), theta = 1), 0L)
  })
})

test_that("ran_gamma_pois_zi", {
  expect_error(ran_gamma_pois_zi(NA_integer_))
  expect_error(ran_gamma_pois_zi(integer(0)))
  expect_identical(ran_gamma_pois_zi(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois_zi(), 1L)
    expect_identical(ran_gamma_pois_zi(2), c(0L, 2L))
    expect_identical(ran_gamma_pois_zi(2, 10), c(14L, 7L))
    expect_identical(ran_gamma_pois_zi(2, c(0.1, 100)), c(0L, 108L))
    expect_identical(ran_gamma_pois_zi(1, c(0.1, 100)), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois_zi(prob = 0.5), 0L)
    expect_identical(ran_gamma_pois_zi(2, prob = 0.5), c(1L, 1L))
    expect_identical(ran_gamma_pois_zi(2, 10, prob = 0.5), c(7L, 0L))
    expect_identical(ran_gamma_pois_zi(2, c(0.1, 100), prob = 0.5), c(0L, 100L))
    expect_identical(ran_gamma_pois_zi(1, c(0.1, 100), prob = 0.5), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois_zi(theta = 10), 0L)
    expect_identical(ran_gamma_pois_zi(2, theta = 10), c(1L, 2L))
    expect_identical(ran_gamma_pois_zi(2, 10, theta = 10), c(0L, 0L))
    expect_identical(ran_gamma_pois_zi(2, 10, theta = 10), c(7L, 13L))
    expect_identical(ran_gamma_pois_zi(2, c(0.1, 100), theta = 1), c(0L, 12L))
    expect_identical(ran_gamma_pois_zi(2, c(0.1, 100), theta = 1), c(0L, 117L))
    expect_identical(ran_gamma_pois_zi(1, c(0.1, 100), theta = 1), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_gamma_pois_zi(theta = 10, prob = 0.5), 0L)
    expect_identical(ran_gamma_pois_zi(2, theta = 10, prob = 0.5), c(0L, 0L))
    expect_identical(
      ran_gamma_pois_zi(2, 10, theta = 10, prob = 0.5),
      c(9L, 0L)
    )
    expect_identical(
      ran_gamma_pois_zi(2, 10, theta = 10, prob = 0.5),
      c(0L, 0L)
    )
    expect_identical(
      ran_gamma_pois_zi(2, c(0.1, 100), theta = 1, prob = 0.5),
      c(0L, 0L)
    )
    expect_identical(
      ran_gamma_pois_zi(2, c(0.1, 100), theta = 1, prob = 0.5),
      c(0L, 0L)
    )
    expect_identical(
      ran_gamma_pois_zi(1, c(0.1, 100), theta = 1, prob = 0.5),
      0L
    )
  })
})

test_that("ran_gamma", {
  expect_error(ran_gamma(NA_integer_))
  expect_error(ran_gamma(integer(0)))
  expect_identical(ran_gamma(0L), numeric(0))
  withr::with_seed(101, {
    expect_equal(ran_gamma(), 0.296032334929205)
    expect_equal(ran_gamma(2), c(0.828447196595941, 0.198011979729563))
  })
  withr::with_seed(101, {
    expect_equal(ran_gamma(shape = 2), 1.12726342864167)
    expect_equal(ran_gamma(rate = 2), 0.41422359829797)
    expect_equal(ran_gamma(shape = 2, rate = 2), 0.463321963301684)
    expect_equal(ran_gamma(shape = 0.5), 1.25656793027554)
    expect_equal(ran_gamma(rate = 0.5), 2.06655144879794)
    expect_equal(ran_gamma(shape = 0.5, rate = 0.5), 1.88702952622222)
  })
  withr::with_seed(101, {
    expect_equal(ran_gamma(1, shape = c(1, 2), rate = 0.5), 0.592064669858409)
    expect_equal(ran_gamma(1, rate = c(1, 2)), 0.828447196595941)
    expect_equal(
      ran_gamma(2, shape = c(1, 2), rate = c(1, 2)),
      c(0.198011979729563, 0.95237869084932)
    )
  })
})

test_that("ran_student", {
  expect_error(ran_student(NA_integer_))
  expect_error(ran_student(integer(0)))
  expect_error(ran_student(-1))
  expect_error(ran_student(0.1))
  expect_identical(ran_student(0L), numeric(0))
  withr::with_seed(101, {
    expect_equal(ran_student(), -0.326036490515386)
    expect_equal(ran_student(2), c(0.552461855419139, -0.67494384395583))
    expect_equal(
      ran_student(2, mean = 10),
      c(10.2143594590434, 10.3107692173136)
    )
    expect_equal(
      ran_student(2, mean = c(0.1, 100)),
      c(1.2739662875627, 100.618789855626)
    )
    expect_equal(
      ran_student(2, sd = 10),
      c(-1.12734314754215, 9.17028289512712)
    )
    expect_equal(ran_student(1, sd = c(0.1, 100)), -0.0223259364627263)
    expect_equal(ran_student(1, 5, 0), 5L)
    expect_equal(ran_student(1, 5, 0, 1), 5L)
    expect_equal(
      ran_student(2, mean = c(5, 10), sd = c(0.1, 1)),
      c(4.85331803058365, 9.7633166213971)
    )
    expect_equal(
      ran_student(1, mean = c(5, 10), sd = c(0.1, 1)),
      4.98066620350025
    )
  })
  withr::with_seed(101, {
    expect_equal(ran_student(theta = 0.5), -0.33156105737203)
    expect_equal(
      ran_student(2, theta = 0.5),
      c(-0.828877594163717, 0.240144994036107)
    )
    expect_equal(
      ran_student(2, 10, theta = 0.5),
      c(10.950902377826, 9.64745806982169)
    )
    expect_equal(
      ran_student(2, c(0.1, 100), theta = 0.5),
      c(-0.585038332110831, 101.180666799288)
    )
    expect_equal(ran_student(1, c(0.1, 100), theta = 0.5), 2.36756946562059)
  })
  withr::with_seed(101, {
    expect_equal(ran_student(theta = 10), -1.87474739594541)
    expect_equal(
      ran_student(2, theta = 10),
      c(-26.8550547217203, 0.208423313901581)
    )
    expect_equal(
      ran_student(2, 10, theta = 10),
      c(312.416238182549, 1232.50541097839)
    )
    expect_equal(
      ran_student(2, c(0.1, 100), theta = 1),
      c(1.57371753266519, 111.974370801549)
    )
    expect_equal(
      ran_student(2, c(0.1, 100), theta = 1),
      c(-0.233907776839805, 74.8346231062475)
    )
    expect_equal(ran_student(1, c(0.1, 100), theta = 1), 1.17304786153918)
  })
  withr::with_seed(101, {
    expect_equal(ran_student(theta = 0.1), -0.304076815655304)
    expect_equal(
      ran_student(2, theta = 0.1),
      c(-0.677235928466201, 0.256581693475524)
    )
    expect_equal(
      ran_student(2, 10, theta = 0.1),
      c(10.6700666098513, 8.63003697321142)
    )
    expect_equal(
      ran_student(2, 10, 0.1, theta = 0.1),
      c(9.93731209896207, 9.83624845227821)
    )
    expect_equal(
      ran_student(2, c(0.1, 100), 10, theta = 0.1),
      c(-6.27604275569631, 83.3203568426077)
    )
    expect_equal(
      ran_student(2, c(0.1, 100), c(0.1, 100), theta = 0.1),
      c(0.0828946276667616, -31.2764527374883)
    )
  })
})

test_that("ran_beta_binom", {
  expect_error(ran_beta_binom(NA_integer_))
  expect_error(ran_beta_binom(integer(0)))
  expect_identical(ran_beta_binom(0L), integer(0))
  withr::with_seed(101, {
    expect_identical(ran_beta_binom(), 0L)
    expect_identical(ran_beta_binom(2), c(0L, 1L))
    expect_identical(ran_beta_binom(2, 10), c(6L, 4L))
    expect_identical(ran_beta_binom(2, c(5, 10)), c(2L, 5L))
    expect_identical(ran_beta_binom(1, c(5, 10)), 2L)
  })
  withr::with_seed(101, {
    expect_identical(ran_beta_binom(prob = 0.2), 0L)
    expect_identical(ran_beta_binom(2, prob = 0.2), c(0L, 0L))
    expect_identical(ran_beta_binom(2, 10, prob = 0.2), c(2L, 1L))
    expect_identical(ran_beta_binom(2, c(5, 10), prob = 0.2), c(0L, 2L))
    expect_identical(ran_beta_binom(1, c(5, 10), prob = 0.2), 1L)
    expect_identical(ran_beta_binom(1, 0, prob = 0.2), 0L)
    expect_identical(ran_beta_binom(1, 0, prob = 0.9), 0L)
  })
  withr::with_seed(101, {
    expect_identical(ran_beta_binom(theta = 10), 1L)
    expect_identical(ran_beta_binom(2, theta = 10), c(0L, 1L))
    expect_identical(ran_beta_binom(2, 10, theta = 10), c(0L, 10L))
    expect_identical(ran_beta_binom(2, 10, theta = 10), c(0L, 0L))
    expect_identical(ran_beta_binom(2, c(10, 100), theta = 1), c(2L, 70L))
    expect_identical(ran_beta_binom(2, c(10, 100), theta = 1), c(10L, 54L))
    expect_identical(ran_beta_binom(1, c(10, 100), theta = 1), 6L)
  })
  withr::with_seed(101, {
    expect_identical(ran_beta_binom(theta = 0.5, prob = 0.2), 1L)
    expect_identical(ran_beta_binom(2, theta = 0.5, prob = 0.2), c(0L, 1L))
    expect_identical(ran_beta_binom(2, 10, theta = 0.5, prob = 0.2), c(4L, 1L))
    expect_identical(ran_beta_binom(2, 10, theta = 0.5, prob = 0.2), c(0L, 5L))
    expect_identical(
      ran_beta_binom(2, c(10, 100), theta = 1, prob = 0.2),
      c(0L, 0L)
    )
    expect_identical(
      ran_beta_binom(2, c(10, 100), theta = 1, prob = 0.2),
      c(0L, 64L)
    )
    expect_identical(ran_beta_binom(1, c(10, 100), theta = 1, prob = 0.2), 4L)
  })
})

test_that("ran_skewnorm", {
  skip_if_not_installed("sn")
  expect_error(ran_skewnorm(NA_integer_))
  expect_error(ran_skewnorm(NULL))
  expect_error(ran_skewnorm(integer(0)))
  expect_error(ran_skewnorm(1, 0, sd = -1))
  expect_identical(ran_skewnorm(0L), numeric(0))
  withr::with_seed(101, {
    expect_equal(ran_skewnorm(), 0.552461855419139)
    expect_equal(ran_skewnorm(2), c(0.214359459043425, 1.1739662875627))
    expect_equal(ran_skewnorm(2, 10), c(9.88726568524578, 9.77674063537274))
    expect_equal(
      ran_skewnorm(2, c(5, 10)),
      c(4.20515556458494, 8.53318030583653)
    )
    expect_equal(ran_skewnorm(1, c(5, 10)), c(4.8066620350025))
  })
  withr::with_seed(101, {
    expect_equal(ran_skewnorm(sd = 1), 0.552461855419139)
    expect_equal(ran_skewnorm(2, sd = 2), c(0.428718918086851, 2.3479325751254))
    expect_equal(
      ran_skewnorm(2, 10, sd = 3),
      c(9.66179705573736, 9.33022190611821)
    )
    expect_equal(
      ran_skewnorm(2, c(5, 10), sd = 2),
      c(3.41031112916989, 7.06636061167307)
    )
    expect_equal(ran_skewnorm(1, c(5, 10), sd = 1), c(4.8066620350025))
    expect_equal(ran_skewnorm(1, 0, sd = 3), 0.175396493548505)
    expect_equal(ran_skewnorm(1, 0, sd = 4), -8.20123126255853)
  })
  withr::with_seed(101, {
    expect_equal(ran_skewnorm(shape = 10), 0.379390442659801)
    expect_equal(
      ran_skewnorm(2, shape = 10),
      c(0.692923789421489, 0.426040940417248)
    )
    expect_equal(
      ran_skewnorm(2, 10, shape = 10),
      c(10.6045014356932, 10.8902621154531)
    )
    expect_equal(
      ran_skewnorm(2, 10, shape = 10),
      c(10.4447454597277, 11.2747158507856)
    )
    expect_equal(
      ran_skewnorm(2, c(10, 100), shape = -1),
      c(9.69592899188521, 99.4404740107593)
    )
    expect_equal(
      ran_skewnorm(2, c(10, 100), shape = -1),
      c(7.97203318662633, 100.385208042625)
    )
    expect_equal(ran_skewnorm(1, c(10, 100), shape = -1), c(8.77536013503756))
  })
  withr::with_seed(101, {
    expect_equal(ran_skewnorm(sd = 1, shape = 5), 0.428051766065545)
    expect_equal(
      ran_skewnorm(2, sd = 1, shape = 5),
      c(0.703876239197544, 0.534968020196629)
    )
    expect_equal(
      ran_skewnorm(2, 10, sd = 3, shape = 5),
      c(11.7539930699001, 12.5663063681019)
    )
    expect_equal(
      ran_skewnorm(2, 10, sd = 2, shape = -2),
      c(8.3473305357842, 6.13398981871521)
    )
    expect_equal(
      ran_skewnorm(2, c(10, 100), sd = 5, shape = -2),
      c(8.50920292023333, 96.3305141003515)
    )
    expect_equal(ran_skewnorm(2, c(10, 100), sd = 0, shape = -1), c(10, 100))
    expect_equal(
      ran_skewnorm(1, c(10, 100), sd = 10, shape = -1),
      c(-2.24639864962439)
    )
  })
})

test_that("ran_skewlnorm", {
  skip_if_not_installed("sn")
  expect_error(ran_skewlnorm(NA_integer_))
  expect_error(ran_skewlnorm(NULL))
  expect_error(ran_skewlnorm(1, 0, sdlog = -1))
  expect_identical(ran_skewlnorm(0L), numeric(0))
  withr::with_seed(
    101,
    expect_true(all(ran_skewlnorm(100, 0, 1, 2) > 0))
  )
  withr::with_seed(
    101,
    expect_equal(
      ran_skewlnorm(3, 0, 1, 2),
      c(1.71375069101727, 2.01285193758523, 2.23216981261934)
    )
  )
})
