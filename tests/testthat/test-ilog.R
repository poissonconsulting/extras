test_that("ilog errors with character input", {
  expect_error(ilog("6"), "non-numeric argument to mathematical function")
})

test_that("ilog errors with NULL input", {
  expect_error(ilog(NULL), "non-numeric argument to mathematical function")
})

test_that("ilog numeric(0) input returns numeric(0)", {
  expect_equal(ilog(numeric(0)), numeric(0))
})

test_that("ilog NA_real_ input returns NA_real_", {
  expect_equal(ilog(NA_real_), NA_real_)
})

test_that("ilog vectorized NA_real_ input returns vector of NA_real_", {
  expect_equal(ilog(rep(NA_real_, 2)), rep(NA_real_, 2))
})

test_that("ilog Inf input returns Inf", {
  expect_equal(ilog(Inf), Inf)
})

test_that("ilog -Inf input returns 0", {
  expect_equal(ilog(-Inf), 0)
})

test_that("ilog logical inputs coerced to numeric", {
  expect_equal(ilog(TRUE), exp(1))
  expect_equal(ilog(FALSE), 1)
  expect_equal(ilog(NA), NA_real_)
})

test_that("ilog integer inputs return expected output", {
  expect_equal(ilog(1L), exp(1))
  expect_equal(ilog(-5L), 0.00673794699908547)
  expect_equal(ilog(100L), 2.68811714181614e+43)
})

test_that("ilog real inputs return expected output", {
  expect_equal(ilog(-0.0231), 0.977164762411049)
  expect_equal(ilog(1.000322), 2.7191572561441)
  expect_equal(ilog(2.491), 12.0733434292139)
})

test_that("ilog vectorized inputs return expected output", {
  expect_equal(
    ilog(-10:10),
    c(
      4.53999297624849e-05, 0.00012340980408668, 0.000335462627902512,
      0.000911881965554516, 0.00247875217666636, 0.00673794699908547,
      0.0183156388887342, 0.0497870683678639, 0.135335283236613, 0.367879441171442,
      1, 2.71828182845905, 7.38905609893065, 20.0855369231877, 54.5981500331442,
      148.413159102577, 403.428793492735, 1096.63315842846, 2980.95798704173,
      8103.08392757538, 22026.4657948067
    )
  )
  expect_equal(
    ilog(seq(-2, 2, length.out = 10)),
    c(
      0.135335283236613, 0.21107208779109, 0.329192987807906, 0.513417119032592,
      0.800737402916808, 1.24884886900168, 1.94773404105468, 3.03773177751748,
      4.73771785964308, 7.38905609893065
    )
  )
})

test_that("ilog matrix inputs return expected output", {
  expect_equal(
    ilog(matrix(1:10, ncol = 2, nrow = 5)),
    structure(
      c(
        2.71828182845905, 7.38905609893065, 20.0855369231877,
        54.5981500331442, 148.413159102577, 403.428793492735, 1096.63315842846,
        2980.95798704173, 8103.08392757538, 22026.4657948067
      ),
      dim = c(5L, 2L)
    )
  )
  expect_equal(
    ilog(matrix(seq(-2, 2, length.out = 10), nrow = 2, ncol = 5)),
    structure(
      c(
        0.135335283236613, 0.21107208779109, 0.329192987807906,
        0.513417119032592, 0.800737402916808, 1.24884886900168, 1.94773404105468,
        3.03773177751748, 4.73771785964308, 7.38905609893065
      ),
      dim = c(2L, 5L)
    )
  )
})

test_that("ilog array inputs return expected output", {
  expect_equal(
    ilog(array(1:20, dim = c(5, 2, 2))),
    structure(structure(c(
      2.71828182845905, 7.38905609893065, 20.0855369231877,
      54.5981500331442, 148.413159102577, 403.428793492735, 1096.63315842846,
      2980.95798704173, 8103.08392757538, 22026.4657948067, 59874.1417151978,
      162754.791419004, 442413.39200892, 1202604.28416478, 3269017.37247211,
      8886110.52050787, 24154952.7535753, 65659969.1373305, 178482300.963187,
      485165195.40979
    ), dim = c(5L, 2L, 2L)))
  )
  expect_equal(
    ilog(array(seq(-2, 2, length.out = 20), dim = c(2, 5, 2))),
    structure(c(
      0.135335283236613, 0.167048066569284, 0.206192028251409,
      0.25450849798849, 0.314146846983714, 0.387760103296325, 0.478622972511232,
      0.590777513901232, 0.729212952525235, 0.900087626252259, 1.11100294108447,
      1.37134152175581, 1.69268460032686, 2.08932721042037, 2.57891410565208,
      3.18322469125988, 3.929141886827, 4.84984802021882, 5.98630095239829,
      7.38905609893065
    ), dim = c(2L, 5L, 2L))
  )
})

test_that("ilog dataframe inputs return expected output", {
  expect_equal(
    ilog(data.frame(y = -5:5)),
    structure(list(y = c(
      0.00673794699908547, 0.0183156388887342,
      0.0497870683678639, 0.135335283236613, 0.367879441171442, 1,
      2.71828182845905, 7.38905609893065, 20.0855369231877, 54.5981500331442,
      148.413159102577
    )), row.names = c(NA, -11L), class = "data.frame")
  )
})

test_that("ilog2 errors with character input", {
  expect_error(ilog2("6"), "non-numeric argument to binary operator")
})

test_that("ilog2 NULL input returns numeric(0)", {
  expect_equal(ilog2(NULL), numeric(0))
})

test_that("ilog2 numeric(0) input returns numeric(0)", {
  expect_equal(ilog2(numeric(0)), numeric(0))
})

test_that("ilog2 NA_real_ input returns NA_real_", {
  expect_equal(ilog2(NA_real_), NA_real_)
})

test_that("ilog2 vectorized NA_real_ input returns vector of NA_real_", {
  expect_equal(ilog2(rep(NA_real_, 2)), rep(NA_real_, 2))
})

test_that("ilog2 Inf input returns Inf", {
  expect_equal(ilog2(Inf), Inf)
})

test_that("ilog2 -Inf input returns 0", {
  expect_equal(ilog2(-Inf), 0)
})

test_that("ilog2 logical inputs coerced to numeric", {
  expect_equal(ilog2(TRUE), 2)
  expect_equal(ilog2(FALSE), 1)
  expect_equal(ilog2(NA), NA_real_)
})

test_that("ilog2 integer inputs return expected output", {
  expect_equal(ilog2(1L), 2)
  expect_equal(ilog2(-5L), 0.03125)
  expect_equal(ilog2(100L), 1.26765060022823e+30)
})

test_that("ilog2 real inputs return expected output", {
  expect_equal(ilog2(-0.0231), 0.984115805960014)
  expect_equal(ilog2(1.000322), 2.00044643660328)
  expect_equal(ilog2(2.491), 5.62167480082688)
})

test_that("ilog2 vectorized inputs return expected output", {
  expect_equal(
    ilog2(-10:10),
    c(
      0.0009765625, 0.001953125, 0.00390625, 0.0078125, 0.015625,
      0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128,
      256, 512, 1024
    )
  )
  expect_equal(
    ilog2(seq(-2, 2, length.out = 10)),
    c(
      0.25, 0.340197500043594, 0.462937356143645, 0.629960524947437,
      0.857243982853073, 1.16652903957612, 1.5874010519682, 2.16011947778461,
      2.9394689845512, 4
    )
  )
})

test_that("ilog2 matrix inputs return expected output", {
  expect_equal(
    ilog2(matrix(1:10, ncol = 2, nrow = 5)),
    structure(c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), dim = c(5L, 2L))
  )
  expect_equal(
    ilog2(matrix(seq(-2, 2, length.out = 10), nrow = 2, ncol = 5)),
    structure(c(
      0.25, 0.340197500043594, 0.462937356143645, 0.629960524947437,
      0.857243982853073, 1.16652903957612, 1.5874010519682, 2.16011947778461,
      2.9394689845512, 4
    ), dim = c(2L, 5L))
  )
})

test_that("ilog2 array inputs return expected output", {
  expect_equal(
    ilog2(array(1:20, dim = c(5, 2, 2))),
    structure(
      c(
        2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096,
        8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576
      ),
      dim = c(5L, 2L, 2L)
    )
  )
  expect_equal(
    ilog2(array(seq(-2, 2, length.out = 20), dim = c(2, 5, 2))),
    structure(c(
      0.25, 0.28927755932068, 0.334726025306118, 0.387314910566664,
      0.448166048068928, 0.518577522223096, 0.600051359789051, 0.694325571307328,
      0.803411226566882, 0.929635355008406, 1.07569058622018, 1.24469258946402,
      1.44024653753876, 1.66652401279709, 1.92835199588499, 2.23131583552309,
      2.58187839589481, 2.98751792330897, 3.45688757312642, 4
    ), dim = c(2L, 5L, 2L))
  )
})

test_that("ilog2 dataframe inputs return expected output", {
  expect_equal(
    ilog2(data.frame(y = -5:5)),
    structure(list(y = c(
      0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2,
      4, 8, 16, 32
    )), class = "data.frame", row.names = c(NA, -11L))
  )
})

test_that("ilog10 errors with character input", {
  expect_error(ilog10("6"), "non-numeric argument to binary operator")
})

test_that("ilog10 NULL input returns numeric(0)", {
  expect_equal(ilog10(NULL), numeric(0))
})

test_that("ilog10 numeric(0) input returns numeric(0)", {
  expect_equal(ilog10(numeric(0)), numeric(0))
})

test_that("ilog10 NA_real_ input returns NA_real_", {
  expect_equal(ilog10(NA_real_), NA_real_)
})

test_that("ilog10 vectorized NA_real_ input returns vector of NA_real_", {
  expect_equal(ilog10(rep(NA_real_, 2)), rep(NA_real_, 2))
})

test_that("ilog10 Inf input returns Inf", {
  expect_equal(ilog10(Inf), Inf)
})

test_that("ilog10 -Inf input returns 0", {
  expect_equal(ilog10(-Inf), 0)
})

test_that("ilog10 logical inputs coerced to numeric", {
  expect_equal(ilog10(TRUE), 10)
  expect_equal(ilog10(FALSE), 1)
  expect_equal(ilog10(NA), NA_real_)
})

test_that("ilog10 integer inputs return expected output", {
  expect_equal(ilog10(1L), 10)
  expect_equal(ilog10(-5L), 1e-05)
  expect_equal(ilog10(100L), 1e+100)
})

test_that("ilog10 real inputs return expected output", {
  expect_equal(ilog10(-0.0231), 0.948200107019506)
  expect_equal(ilog10(1.000322), 10.0074170732889)
  expect_equal(ilog10(2.491), 309.741929921658)
})

test_that("ilog10 vectorized numeric inputs return expected outputs", {
  expect_equal(
    ilog10(-10:10),
    c(
      1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 0.001, 0.01,
      0.1, 1, 10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08, 1e+09,
      1e+10
    )
  )
  expect_equal(
    ilog10(seq(-2, 2, length.out = 10)),
    c(
      0.01, 0.0278255940220712, 0.0774263682681127, 0.215443469003188,
      0.599484250318941, 1.66810053720006, 4.64158883361278, 12.9154966501488,
      35.9381366380463, 100
    )
  )
})

test_that("ilog10 matrix numeric inputs return expected output", {
  expect_equal(
    ilog10(matrix(1:10, ncol = 2, nrow = 5)),
    structure(c(
      10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08,
      1e+09, 1e+10
    ), dim = c(5L, 2L))
  )
  expect_equal(
    ilog10(matrix(seq(-2, 2, length.out = 10), nrow = 2, ncol = 5)),
    structure(c(
      0.01, 0.0278255940220712, 0.0774263682681127, 0.215443469003188,
      0.599484250318941, 1.66810053720006, 4.64158883361278, 12.9154966501488,
      35.9381366380463, 100
    ), dim = c(2L, 5L))
  )
})

test_that("ilog10 array numeric inputs return expected output", {
  expect_equal(
    ilog10(array(1:20, dim = c(5, 2, 2))),
    structure(c(
      10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07, 1e+08,
      1e+09, 1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15, 1e+16, 1e+17,
      1e+18, 1e+19, 1e+20
    ), dim = c(5L, 2L, 2L))
  )
  expect_equal(
    ilog10(array(seq(-2, 2, length.out = 20), dim = c(2, 5, 2))),
    structure(c(
      0.01, 0.0162377673918872, 0.0263665089873036, 0.0428133239871939,
      0.0695192796177561, 0.112883789168469, 0.183298071083244, 0.297635144163132,
      0.483293023857175, 0.784759970351461, 1.27427498570313, 2.06913808111479,
      3.35981828628378, 5.45559478116851, 8.85866790410082, 14.3844988828766,
      23.3572146909012, 37.9269019073225, 61.5848211066026, 100
    ), dim = c(2L, 5L, 2L))
  )
})

test_that("ilog10 dataframe numeric inputs return expected output", {
  expect_equal(
    ilog10(data.frame(y = -5:5)),
    structure(list(y = c(
      1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100,
      1000, 10000, 1e+05
    )), class = "data.frame", row.names = c(NA, -11L))
  )
})
