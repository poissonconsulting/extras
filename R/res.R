#' Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_pois(c(1,3.5,4), 3)
res_pois <- function(x, lambda = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_pois(length(x), lambda = lambda)
  }
  switch(type,
         raw = x - lambda,
         dev = dev_pois(x, lambda, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Normal Residuals
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2))
res_norm <- function(x,  mean = 0, sd = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_norm(length(x), mean = mean, sd = sd)
  }
  switch(type,
         raw = x - mean,
         dev = dev_norm(x, mean = mean, sd = sd, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Log-Normal Residuals
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' dev_norm(exp(c(-2:2)))
res_lnorm <- function(x,  meanlog = 0, sdlog = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_lnorm(length(x), meanlog = meanlog, sdlog = sdlog)
  }
  switch(type,
         raw = x - exp(meanlog),
         dev = dev_lnorm(x, meanlog = meanlog, sdlog = sdlog, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Binomial Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_binom(c(0, 1, 2), 2, 0.3)
res_binom <- function(x, size = 1, prob = 0.5, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_binom(length(x), size = size, prob = prob)
  }
  switch(type,
         raw = x - size * prob,
         dev = dev_binom(x, size = size, prob = prob, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Bernoulli Residuals
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_bern(c(TRUE, FALSE), 0.7)
res_bern <- function(x, prob = 0.5, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_bern(length(x), prob = prob)
  }
  switch(type,
         raw = x - prob,
         dev = dev_bern(x, prob = prob, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Gamma Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_gamma_pois(c(0, 1, 2), 1, 1)
res_gamma_pois <- function(x, lambda = 1, theta = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_gamma_pois(length(x), lambda = lambda, theta = theta)
  }
  switch(type,
         raw = x - lambda,
         dev = dev_gamma_pois(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

#' Negative Binomial Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_neg_binom(c(0, 1, 5), 2, 3)
res_neg_binom <- function(x, lambda = 1, theta = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_neg_binom(length(x), lambda = lambda, theta = theta)
  }
  switch(type,
         raw = x - lambda,
         dev = dev_neg_binom(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}

