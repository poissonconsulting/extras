test_that("`direction()` returns the correct direction.", {
  expect_equal(direction(NA_real_), NA_character_)
  expect_equal(direction(NA_integer_), NA_character_)
  expect_equal(direction(numeric(0)), NA_character_)
  expect_equal(direction(c(1, NA_real_)), NA_character_)
  expect_equal(direction(c(1, NA_real_), na_rm = TRUE), "right")

  expect_equal(direction(c(1, 2, 3)), "right")
  expect_equal(direction(c(-1)), "left")
  expect_equal(direction(c(0, 0, 0)), "right")
  expect_equal(direction(c(-100, 1, 1)), "right")
  expect_equal(direction(c(-100, 1, 1), estimate = mean), "left")
  expect_equal(direction(c(100, 0.01, 0.01), threshold = 1), "left")
  expect_equal(direction(c(100, 0.01, 0.01), estimate = mean, threshold = 1), "right")
  expect_equal(direction(c(100, 0.01, 0.01), function(.x) exp(mean(log(.x))),
                         threshold = 1), "left")
})

test_that("`direction()` accepts custom, unnamed functions.", {
  expect_no_error(direction(c(100, 0.01, 0.01),
                            estimate = function(.x) exp(mean(log(.x)))))
  expect_error(direction(c(100, 0.01, 0.01),
                            estimate = function(.x) as.character(.x[1])),
                  "The estimate function must return a number \\(non-missing numeric scalar\\).")
})

