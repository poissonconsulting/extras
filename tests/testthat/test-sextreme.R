test_that("sextreme", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_identical(sextreme(numeric(0)), numeric(0))
  expect_identical(sextreme(NA_real_), NA_real_)
  expect_identical(sextreme(0.5), 0)
  expect_equal(
    sextreme(seq(0, 1, by = 0.1)),
    c(
      Inf,
      2.32192809488736,
      1.32192809488736,
      0.736965594166206,
      0.321928094887362,
      0,
      0.321928094887363,
      0.736965594166206,
      1.32192809488736,
      2.32192809488736,
      Inf
    )
  )
  expect_equal(
    sextreme(seq(0, 1, by = 0.1), directional = TRUE),
    c(
      -Inf,
      -2.32192809488736,
      -1.32192809488736,
      -0.736965594166206,
      -0.321928094887362,
      0,
      0.321928094887363,
      0.736965594166206,
      1.32192809488736,
      2.32192809488736,
      Inf
    )
  )
  expect_equal(
    sextreme(c(0, NA_real_)),
    c(Inf, NA_real_)
  )
  expect_equal(
    sextreme(c(0, NA_real_), directional = TRUE),
    c(-Inf, NA_real_)
  )
})

test_that("sextreme warns", {
  # sextreme() calls pextreme() internally (sharing id = "sextreme"), which
  # also emits a deprecation warning once the id-based dedup is bypassed by
  # forcing verbosity = "warning", so we can't assert a single exact match.
  rlang::local_options(lifecycle_verbosity = "warning")
  warnings <- testthat::capture_warnings(sextreme(0.5))
  expect_match(warnings, "sextreme.*deprecated", all = FALSE)
})
