# dskewnorm ----

test_that("dskewnorm passes through numeric(0) with zero-length arguments", {
  expect_identical(dskewnorm(logical(0)), numeric(0))
  expect_identical(dskewnorm(numeric(0)), numeric(0))
  expect_identical(dskewnorm(1, numeric(0)), numeric(0))
  expect_identical(dskewnorm(1, 2,numeric(0)), numeric(0))
  expect_identical(dskewnorm(1, 2, 3, numeric(0)), numeric(0))
  expect_identical(dskewnorm(1, numeric(0), 2, 3), numeric(0))
})

test_that("dskewnorm errors with character arguments", {
  expect_chk_error(dskewnorm(character(0)), "`character` must be FALSE.")
  expect_chk_error(dskewnorm(character(0), 1:5, 1:5, 1:5), "`character` must be FALSE.")
  expect_chk_error(dskewnorm("1"), "`character` must be FALSE.")
})

test_that("dskewnorm does not allow negative sd argument", {
  expect_chk_error(dskewnorm(1, 0, -5, 0), "`sd` must be greater than or equal to 0, not -5.")
  expect_chk_error(dskewnorm(1, 0, -1, -10), "`sd` must be greater than or equal to 0, not -1.")
})

test_that("dskewnorm returns NA with NA arguments", {
  expect_identical(dskewnorm(NA), NA_real_)
  expect_identical(dskewnorm(1, NA), NA_real_)
  expect_identical(dskewnorm(1, 2, NA), NA_real_)
  expect_identical(dskewnorm(1, 2, 3, NA), NA_real_)
})

test_that("dskewnorm returns NA with NA arguments vectorized", {
  expect_equal(dskewnorm(c(NA, 1)), c(NA_real_, 0.241970724519143))
  expect_equal(dskewnorm(0.1, c(NA, 1)), c(NA_real_, 0.266085249898755))
  expect_equal(dskewnorm(0.1, 2, c(NA, 1)), c(NA_real_, 0.0656158147746766))
  expect_equal(dskewnorm(0.1, 2, 3, c(NA, 1)), c(NA_real_, 0.0572928207875276))
})

test_that("dskewnorm returns 0 with x = Inf", {
  expect_identical(dskewnorm(Inf), 0)
  expect_identical(dskewnorm(Inf, 4, 2, -1), 0)
  expect_identical(dskewnorm(Inf, 4, 2, -10), 0)
})

test_that("dskewnorm returns 0 with x = -Inf", {
  expect_identical(dskewnorm(-Inf), 0)
  expect_identical(dskewnorm(-Inf, 4, 2, -1), 0)
  expect_identical(dskewnorm(-Inf, 4, 2, -10), 0)
})

test_that("dskewnorm returns 0 with mean = Inf", {
  expect_identical(dskewnorm(1, Inf, 1, 3), 0)
  expect_identical(dskewnorm(1, Inf, 2, -1), 0)
  expect_identical(dskewnorm(1, Inf, 2, -10), 0)
})

test_that("dskewnorm returns 0 with mean = -Inf", {
  expect_identical(dskewnorm(1, -Inf, 1, 3), 0)
  expect_identical(dskewnorm(1, -Inf, 2, -1), 0)
  expect_identical(dskewnorm(1, -Inf, 2, -10), 0)
})

test_that("dskewnorm returns 0 with sd = Inf", {
  expect_identical(dskewnorm(1, 0, Inf, 3), 0)
  expect_identical(dskewnorm(1, 0, Inf, -1), 0)
  expect_identical(dskewnorm(1, 0, Inf, -10), 0)
})

test_that("dskewnorm returns expected value with shape = Inf", {
  expect_equal(dskewnorm(1, 0, 2, Inf), 0.3520653267643)
  expect_equal(dskewnorm(5, 2, 3, Inf), 0.161313816346096)
})

test_that("dskewnorm returns expected value with shape = -Inf", {
  expect_equal(dskewnorm(-2, 0, 2, -Inf), 0.241970724519143)
  expect_equal(dskewnorm(0, 2, 3, -Inf), 0.212965337014902)
})

test_that("dskewnorm equal to dnorm when shape = 0", {
  expect_equal(dskewnorm(1:4, 3, 1, 0), stats::dnorm(1:4, 3, 1))
  expect_equal(dskewnorm(15:20, 13, 3, 0), stats::dnorm(15:20, 13, 3))
})

test_that("dskewnorm equal to dnorm when shape = 0 with log = TRUE", {
  expect_equal(dskewnorm(1:4, 3, 1, 0, log = TRUE), stats::dnorm(1:4, 3, 1, log = TRUE))
  expect_equal(dskewnorm(15:20, 13, 3, 0, log = TRUE), stats::dnorm(15:20, 13, 3, log = TRUE))
})

test_that("dskewnorm returns expected output scalar", {
  expect_equal(dskewnorm(1, 4, 3, 2), 0.00366991060693802)
  expect_equal(dskewnorm(1, 4, 3, -2), 0.157643905739158)
  expect_equal(dskewnorm(10, 4, 3, 2), 0.0359928377014872)
  expect_equal(dskewnorm(10, 4, 3, -2), 1.13997397149537e-06)
})

test_that("dskewnorm returns expected output vector", {
  expect_equal(
    dskewnorm(1:5, 4, 3, 2),
    c(0.00366991060693802, 0.0194248281484597, 0.0635242991919014,
      0.132980760133811, 0.188064519270094)
  )
  expect_equal(
    dskewnorm(1:5, 4, 3, -2),
    c(0.157643905739158, 0.193540508866442, 0.188064519270094,
      0.132980760133811, 0.0635242991919014)
  )
  expect_equal(
    dskewnorm(10, -1:1, 3, 2),
    c(0.000320180434413845, 0.00102818599751395, 0.00295456560504373)
  )
  expect_equal(
    dskewnorm(10, -1:1, 3, -2),
    c(3.59398736170685e-17, 1.34527081549028e-14, 2.91493792526532e-12)
  )
  expect_equal(
    dskewnorm(10, 7, 1:3, 2),
    c(0.0088636968151312, 0.129342760118441, 0.157643905739158)
  )
  expect_equal(
    dskewnorm(10, 7, 1:3, -2),
    c(8.74481377579596e-12, 0.00017483554745085, 0.00366991060693802)
  )
  expect_equal(
    dskewnorm(10, -1:1, 3, 1:3),
    c(0.000320141094999767, 0.00102818599751395, 0.00295456560795867)
  )
  expect_equal(
    dskewnorm(10, -1:1, 3, -3:-1),
    c(6.11755812580031e-32, 1.34527081549028e-14, 3.98836229850538e-06)
  )
})

test_that("dskewnorm errors if argument lengths are incompatible", {
  expect_chk_error(dskewnorm(1:3, 0, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 5.")
  expect_chk_error(dskewnorm(1:3, 0:3, 3, 1), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 4.")
  expect_chk_error(dskewnorm(1, 0:10, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 5 and 11.")
})

test_that("dskewnorm setting log = TRUE returns same value as log(call with log = FALSE)", {
  expect_identical(dskewnorm(1, 4, 2, -10, log = TRUE), log(dskewnorm(1, 4, 2, -10)))
})

# pskewnorm ----
test_that("pskewnorm passes through numeric(0) with zero-length arguments", {
  expect_identical(pskewnorm(logical(0)), numeric(0))
  expect_identical(pskewnorm(numeric(0)), numeric(0))
  expect_identical(pskewnorm(0.1, numeric(0)), numeric(0))
  expect_identical(pskewnorm(0.1, 2, numeric(0)), numeric(0))
  expect_identical(pskewnorm(0.1, 2, 3, numeric(0)), numeric(0))
  expect_identical(pskewnorm(0.1, numeric(0), 2, 3), numeric(0))
})

test_that("pskewnorm errors with character arguments", {
  expect_chk_error(pskewnorm(character(0)), "`character` must be FALSE.")
  expect_chk_error(pskewnorm("1", 1:5, 1:5, 1:5), "`character` must be FALSE.")
  expect_chk_error(pskewnorm("1"), "`character` must be FALSE.")
})

test_that("pskewnorm does not allow negative sd argument", {
  expect_chk_error(pskewnorm(1, 0, -5, 0), "`sd` must be greater than or equal to 0, not -5.")
  expect_chk_error(pskewnorm(1, 0, -1, -10), "`sd` must be greater than or equal to 0, not -1.")
})

test_that("pskewnorm returns NA with NA arguments", {
  expect_identical(pskewnorm(NA), NA_real_)
  expect_identical(pskewnorm(0.1, NA), NA_real_)
  expect_identical(pskewnorm(0.1, 2, NA), NA_real_)
  expect_identical(pskewnorm(0.1, 2, 3, NA), NA_real_)
})

test_that("pskewnorm returns NA with NA arguments vectorized", {
  expect_equal(pskewnorm(c(NA, 1)), c(NA, 0.841344746068543))
  expect_equal(pskewnorm(0.1, c(NA, 1)), c(NA, 0.18406012534676))
  expect_equal(pskewnorm(0.1, 2, c(NA, 1)), c(NA, 0.0287165598160018))
  expect_equal(pskewnorm(0.1, 2, 3, c(NA, 1)), c(NA, 0.0693047719522059))
})

test_that("pskewnorm returns 1 with q = Inf", {
  expect_identical(pskewnorm(Inf), 1)
  expect_identical(pskewnorm(Inf, 4, 2, -1), 1)
  expect_identical(pskewnorm(Inf, 4, 2, -10), 1)
})

test_that("pskewnorm returns 0 with q = -Inf", {
  expect_identical(pskewnorm(-Inf), 0)
  expect_identical(pskewnorm(-Inf, 4, 2, -1), 0)
  expect_identical(pskewnorm(-Inf, 4, 2, -10), 0)
})

test_that("pskewnorm returns 0 with mean = Inf", {
  expect_identical(pskewnorm(1, Inf, 1, 3), 0)
  expect_identical(pskewnorm(1, Inf, 2, -1), 0)
  expect_identical(pskewnorm(1, Inf, 2, -10), 0)
})

test_that("pskewnorm returns 0 with mean = -Inf", {
  expect_identical(pskewnorm(1, -Inf, 1, 3), 1)
  expect_identical(pskewnorm(1, -Inf, 2, -1), 1)
  expect_identical(pskewnorm(1, -Inf, 2, -10), 1)
})

test_that("pskewnorm returns expected value with sd = Inf", {
  expect_equal(pskewnorm(1, 0, Inf, 3), 0.102416382349567)
  expect_equal(pskewnorm(1, 0, Inf, -1), 0.75)
  expect_equal(pskewnorm(1, 0, Inf, -10), 0.968274482569446)
})

test_that("dskewnorm returns expected value with shape = Inf", {
  expect_equal(pskewnorm(1, 0, 2, Inf), 0.382924922548026)
  expect_equal(pskewnorm(5, 2, 3, Inf), 0.682689492137086)
})

test_that("pskewnorm returns expected value with shape = -Inf", {
  expect_equal(pskewnorm(-2, 0, 2, -Inf), 0.317310507862914)
  expect_equal(pskewnorm(0, 2, 3, -Inf), 0.504985075093846)
})

test_that("pskewnorm equal to pnorm when shape = 0", {
  expect_equal(pskewnorm(1:4, 3, 1, 0), stats::pnorm(1:4, 3, 1))
  expect_equal(pskewnorm(15:20, 13, 3, 0), stats::pnorm(15:20, 13, 3))
})

test_that("pskewnorm returns expected output scalar", {
  expect_equal(pskewnorm(-1, 1, 0.3, 1), 8.72211872736017e-22)
  expect_equal(pskewnorm(-5, 1, 0.3, 1), 3.45994463144963e-93)
  expect_equal(pskewnorm(5, 1, 0.3, 1), 1)
  expect_equal(pskewnorm(1, 1, 0.3, -1), 0.75)
  expect_equal(pskewnorm(1, 1, 0.3, 1), 0.25)
})

test_that("pskewnorm returns expected output vector", {
  expect_equal(
    pskewnorm(1:5, 4, 3, 2),
    c(0.00171887994528883, 0.0115549756735866, 0.0502195406616784,
      0.147583617650433, 0.311336860298151)
  )
  expect_equal(
    pskewnorm(1:5, 4, 3, -2),
    c(0.315591627917625, 0.493430099420259, 0.688663139701849, 0.852416382349567,
      0.949780459338321)
  )
  expect_equal(
    pskewnorm(10, -1:1, 3, 2),
    c(0.99975426722007, 0.999141879333609, 0.9973002039373)
  )
  expect_equal(
    pskewnorm(10, -1:1, 3, -2),
    c(1, 0.999999999999998, 0.99999999999944)
  )
  expect_equal(
    pskewnorm(10, 7, 1:3, 2),
    c(0.9973002039373, 0.866426308006807, 0.684408372082375)
  )
  expect_equal(
    pskewnorm(10, 7, 1:3, -2),
    c(0.99999999999944, 0.999959289455477, 0.998281120054711)
  )
  expect_equal(
    pskewnorm(10, -1:1, 3, 1:3),
    c(0.999754282316219, 0.999141879333609, 0.99730020393674)
  )
  expect_equal(
    pskewnorm(1, -1:1, 3, -3:-1),
    c(0.99833662551985, 0.949780459338321, 0.75)
  )
})

test_that("pskewnorm errors if argument lengths are incompatible", {
  expect_chk_error(pskewnorm(1:3, 0, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 5.")
  expect_chk_error(pskewnorm(1:3, 0:3, 3, 1), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 4.")
  expect_chk_error(pskewnorm(1, 0:10, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 5 and 11.")
})

# qskewnorm
test_that("qskewnorm passes through numeric(0) with zero-length arguments", {
  expect_identical(qskewnorm(logical(0)), numeric(0))
  expect_identical(qskewnorm(numeric(0)), numeric(0))
  expect_identical(qskewnorm(0.1, numeric(0)), numeric(0))
  expect_identical(qskewnorm(0.1, 2, numeric(0)), numeric(0))
  expect_identical(qskewnorm(0.1, 2, 3, numeric(0)), numeric(0))
  expect_identical(qskewnorm(0.1, numeric(0), 2, 3), numeric(0))
})

test_that("qskewnorm errors with character arguments", {
  expect_chk_error(qskewnorm(character(0)), "`character` must be FALSE.")
  expect_chk_error(qskewnorm("1", 1:5, 1:5, 1:5), "`character` must be FALSE.")
  expect_chk_error(qskewnorm("1"), "`character` must be FALSE.")
})

test_that("qskewnorm does not allow negative sd argument", {
  expect_chk_error(qskewnorm(1, 0, -5, 0), "`sd` must be greater than or equal to 0, not -5.")
  expect_chk_error(qskewnorm(1, 0, -1, -10), "`sd` must be greater than or equal to 0, not -1.")
})

test_that("qskewnorm returns NA with NA arguments", {
  expect_identical(qskewnorm(NA), NA_real_)
  expect_identical(qskewnorm(0.1, NA), NA_real_)
  expect_identical(qskewnorm(0.1, 2, NA), NA_real_)
  expect_identical(qskewnorm(0.1, 2, 3, NA), NA_real_)
})

test_that("qskewnorm returns NA with NA arguments vectorized", {
  expect_identical(qskewnorm(c(NA, 1)), c(NA_real_, Inf))
  expect_equal(qskewnorm(0.1, c(NA, 1)), c(NA_real_, -0.281551565544601))
  expect_equal(qskewnorm(0.1, 2, c(NA, 1)), c(NA_real_, 0.718448434455399))
  expect_equal(qskewnorm(0.1, 2, 3, c(NA, 1)), c(NA_real_, 0.565179402872107))
})

test_that("qskewnorm errors with p < 0", {
  expect_chk_error(qskewnorm(-1), "`p` must be greater than or equal to 0, not -1.")
})

test_that("qskewnorm errors with p > 1", {
  expect_chk_error(qskewnorm(1.1), "`p` must be less than or equal to 1, not 1.1.")
})

test_that("qskewnorm returns Inf with mean = Inf", {
  expect_identical(qskewnorm(0.5, Inf, 1, 3), Inf)
  expect_identical(qskewnorm(0.5, Inf, 2, -1), Inf)
  expect_identical(qskewnorm(0.5, Inf, 2, -10), Inf)
})

test_that("qskewnorm returns Inf with mean = -Inf", {
  expect_identical(qskewnorm(0.5, -Inf, 1, 3), -Inf)
  expect_identical(qskewnorm(0.5, -Inf, 2, -1), -Inf)
  expect_identical(qskewnorm(0.5, -Inf, 2, -10), -Inf)
})

test_that("qskewnorm returns expected value with sd = Inf", {
  expect_equal(qskewnorm(0.5, 0, Inf, 3), Inf)
  expect_equal(qskewnorm(0.5, 0, Inf, -1), -Inf)
  expect_equal(qskewnorm(0.5, 0, Inf, -10), -Inf)
})

test_that("qskewnorm returns expected value with shape = Inf", {
  expect_equal(qskewnorm(0.1, 0, 2, Inf), 0.251322693710148)
  expect_equal(qskewnorm(0.5, 2, 3, Inf), 4.02346925058825)
})

test_that("qskewnorm returns expected value with shape = -Inf", {
  expect_equal(qskewnorm(0.5, 0, 2, -Inf), -1.34897950039216)
  expect_equal(qskewnorm(0.5, 2, 3, -Inf), -0.0234692505882457)
})

test_that("qskewnorm equal to pnorm when shape = 0", {
  expect_equal(qskewnorm(seq(0, 1, 0.1), 3, 1, 0), stats::qnorm(seq(0, 1, 0.1), 3, 1))
  expect_equal(qskewnorm(seq(0, 1, 0.1), 13, 3, 0), stats::qnorm(seq(0, 1, 0.1), 13, 3))
})

test_that("qskewnorm returns expected output scalar", {
  expect_equal(qskewnorm(0.1, 1, 0.3, 1), 0.856517940287211)
  expect_equal(qskewnorm(0.5, 1, 0.3, 1), 1.16348563932328)
  expect_equal(qskewnorm(0.2, 1, 0.3, 1), 0.960188690640188)
  expect_equal(qskewnorm(0.67, 1, 0.3, -1), 0.943680675100971)
  expect_equal(qskewnorm(0.99, 1, 0.3, 1), 1.7724884366769)
})

test_that("qskewnorm returns expected output vector", {
  expect_equal(
    qskewnorm(seq(0, 1, 0.2), 4, 3, 2),
    c(-Inf, 4.36071631784007, 5.457936823313, 6.49966344782408, 7.84260238010865,
      Inf)
  )
  expect_equal(
    qskewnorm(seq(0, 1, 0.2), 4, 3, -2),
    c(-Inf, 0.157397619891355, 1.50033655217592, 2.542063176687,
      3.63928368215993, Inf)
  )
  expect_equal(
    qskewnorm(0.1, -1:1, 3, 2),
    c(-1.40143375596674, -0.401433755966738, 0.598566244033262)
  )
  expect_equal(
    qskewnorm(0.1, -1:1, 3, -2),
    c(-5.93439806396064, -4.93439806396064, -3.93439806396064)
  )
  expect_equal(
    qskewnorm(0.1, 7, 1:3, 2),
    c(6.86618874801109, 6.73237749602217, 6.59856624403326)
  )
  expect_equal(
    qskewnorm(0.1, 7, 1:3, -2),
    c(5.35520064534646, 3.71040129069291, 2.06560193603936)
  )
  expect_equal(
    qskewnorm(0.1, -1:1, 3, 1:3),
    c(-2.43482059712789, -0.401433755966738, 0.981695304084913)
  )
  expect_equal(
    qskewnorm(0.1, -1:1, 3, -3:-1),
    c(-5.9345607947046, -4.93439806396064, -3.89665636885046)
  )
})

test_that("qskewnorm errors if argument lengths are incompatible", {
  expect_chk_error(qskewnorm(c(0.1, 0.2, 0.3), 0, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 5.")
  expect_chk_error(qskewnorm(c(0.1, 0.2, 0.3), 0:3, 3, 1), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 4.")
  expect_chk_error(qskewnorm(1, 0:10, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 5 and 11.")
})

# rskewnorm ----

test_that("rskewnorm passes through numeric(0) with zero-length arguments", {
  expect_identical(rskewnorm(logical(0)), numeric(0))
  expect_identical(rskewnorm(numeric(0)), numeric(0))
  expect_identical(rskewnorm(1, numeric(0)), numeric(0))
  expect_identical(rskewnorm(1, 2, numeric(0)), numeric(0))
  expect_identical(rskewnorm(1, 2, 3, numeric(0)), numeric(0))
  expect_identical(rskewnorm(1, numeric(0), 2, 3), numeric(0))
  expect_identical(rskewnorm(2, numeric(0), 2, 3), numeric(0))
})

test_that("rskewnorm errors with character arguments", {
  expect_chk_error(rskewnorm(character(0)), "`n` must be a whole number")
  expect_chk_error(rskewnorm("1", 1:5, 1:5, 1:5), "`n` must be a whole number.")
  expect_chk_error(rskewnorm("1"), "`n` must be a whole number.")
})

test_that("rskewnorm does not allow negative sd argument", {
  expect_chk_error(rskewnorm(1, 0, -5, 0), "`sd` must be greater than or equal to 0, not -5.")
  expect_chk_error(rskewnorm(1, 0, -1, -10), "`sd` must be greater than or equal to 0, not -1.")
})

test_that("rskewnorm errors with n = NA", {
  expect_chk_error(rskewnorm(NA), "`n` must not have any missing values.")
  expect_chk_error(rskewnorm(c(NA, 1)), "`n` must not have any missing values.")
})

test_that("rskewnorm errors with n not a whole number", {
  expect_chk_error(rskewnorm(0.1), "`n` must be a whole number")
  expect_chk_error(rskewnorm(c(NA, 1)), "n` must not have any missing values.")
})

test_that("rskewnorm returns NA with NA arguments", {
  expect_identical(rskewnorm(1, NA), NA_real_)
  expect_identical(rskewnorm(2, 2, NA), rep(NA_real_, 2))
  expect_identical(rskewnorm(1, 2, 3, NA), NA_real_)
})

test_that("rskewnorm returns NA with NA arguments vectorized", {
  set.seed(101)
  expect_equal(rskewnorm(1, c(NA, 1)), NA_real_)
  expect_equal(rskewnorm(2, c(NA, 1)), c(NA, 2.1739662875627))
  expect_equal(rskewnorm(2, 2, c(NA, 1)), c(NA, 1.77674063537274))
  expect_equal(rskewnorm(2, 2, 3, c(NA, 1)), c(NA, 1.91713242500331))
})

test_that("rskewnorm errors with n = Inf", { ### Got to here
  expect_chk_error(rskewnorm(Inf), "`n` must be less than Inf, not Inf.")
})

test_that("rskewnorm errors with n < 0", {
  expect_chk_error(rskewnorm(-1), "`n` must be greater than or equal to 0, not -1.")
  expect_chk_error(rskewnorm(-Inf, 4, 2, -1), "`n` must be greater than or equal to 0, not -Inf.")
})

test_that("rskewnorm returns Inf with mean = Inf", {
  expect_identical(rskewnorm(1, Inf, 1, 3), Inf)
  expect_identical(rskewnorm(1, Inf, 2, -1), Inf)
  expect_identical(rskewnorm(1, Inf, 2, -10), Inf)
  expect_identical(rskewnorm(2, Inf, 2, -10), rep(Inf, 2))
})

test_that("rskewnorm returns 0 with mean = -Inf", {
  expect_identical(rskewnorm(1, -Inf, 1, 3), -Inf)
  expect_identical(rskewnorm(1, -Inf, 2, -1), -Inf)
  expect_identical(rskewnorm(1, -Inf, 2, -10), -Inf)
  expect_identical(rskewnorm(2, -Inf, 2, -10), rep(-Inf, 2))
})

test_that("rskewnorm returns expected value with sd = Inf", {
  set.seed(101)
  expect_equal(rskewnorm(1, 0, Inf, 3), Inf)
  expect_equal(rskewnorm(1, 0, Inf, -1), -Inf)
  expect_equal(rskewnorm(1, 0, Inf, -10), -Inf)
})

test_that("rskewnorm returns NaN with shape = Inf", {
  expect_equal(rskewnorm(1, 0, 2, Inf), NaN)
  expect_equal(rskewnorm(5, 2, 3, Inf), rep(NaN, 5))
})

test_that("rskewnorm returns NaN with shape = -Inf", {
  expect_equal(rskewnorm(2, 0, 2, -Inf), rep(NaN, 2))
  expect_equal(rskewnorm(2, 2, 3, -Inf), rep(NaN, 2))
})

test_that("rskewnorm has same mean and sd as rnorm when shape = 0", {
  set.seed(101)
  skew <- rskewnorm(10000, 3, 1, 0)
  norm <- rnorm(10000, 3, 1)
  expect_equal(mean(skew), 3.00771103659923)
  expect_equal(mean(norm), 3.00603712968659)
  expect_equal(sd(skew), 0.992327435425139)
  expect_equal(sd(norm), 0.994433944761732)
})

test_that("rskewnorm returns values scalar", {
  set.seed(101)
  expect_equal(rskewnorm(1, 1, 0.3, 1), 1.18635764130144)
  expect_equal(rskewnorm(2, 1, 0.3, 1), c(1.18864971882471, 1.31495996312994))
  expect_equal(rskewnorm(3, 1, 0.3, 1), c(1.10735059138223, 1.1471706134089, 0.943064539115782))
  expect_equal(rskewnorm(1, 1, 0.3, -1), 0.38596786598709)
  expect_equal(rskewnorm(1, 1, 0.3, 1), 1.00919495076811)
})

test_that("rskewnorm returns expected output vector", {
  set.seed(101)
  expect_equal(
    rskewnorm(5, 1:5, 3, 2),
    c(2.61605306534115, 4.09865777252101, 5.4089223676431, 5.50913846244797,
      7.16111124161696)
  )
  expect_equal(
    rskewnorm(4, 1:4, 3, -2),
    c(-1.4790041963237, -3.79901527192719, 2.10552175214, 1.79830846021088)
  )
  expect_equal(
    rskewnorm(3, -1:1, 3, 2),
    c(-1.55673679163149, 1.38998471396783, -0.244989879796092)
  )
  expect_equal(
    rskewnorm(3, -1:1, 3, -2),
    c(-4.88976791751776, -1.41335862369797, 0.41458529416042)
  )
  expect_equal(
    rskewnorm(3, 7, 1:3, 2),
    c(7.92529673440786, 6.94868224890956, 9.22086166322179)
  )
  expect_equal(
    rskewnorm(3, 7, 1:3, -2),
    c(7.26133714169839, 4.41086533722627, 6.72267770112743)
  )
  expect_equal(
    rskewnorm(3, -1:1, 3, 1:3),
    c(2.94527949297239, 3.50664063033623, 1.94936774563551)
  )
  expect_equal(
    rskewnorm(3, -1:1, 3, -3:-1),
    c(-3.23334273777235, -3.45470050586524, -3.18919115608908)
  )
})

test_that("rskewnorm errors if argument lengths are incompatible", {
  expect_chk_error(rskewnorm(3, 0, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 3 and 5.")
  expect_chk_error(rskewnorm(2, 0:3, 3, 1), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 2 and 4.")
  expect_chk_error(rskewnorm(11, 0:10, 3, 1:5), "... objects must be all zero length or the same length with some of length of 1 but not lengths 1, 5 and 11.")
})
