#' Beta-Binomial Residuals
#'
#' This parameterization of the beta-binomial distribution uses an expected probability parameter, `prob`, and a dispersion parameter, `theta`. The parameters of the underlying beta mixture are `alpha = (2 * prob) / theta` and `beta = (2 * (1 - prob)) / theta`. This parameterization of `theta` is unconventional, but has useful properties when modelling. When `theta = 0`, the beta-binomial reverts to the binomial distribution. When `theta = 1` and `prob = 0.5`, the parameters of the beta distribution become `alpha = 1` and `beta = 1`, which correspond to a uniform distribution for the beta-binomial probability parameter.
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_beta_binom(c(0, 1, 2), 4, 0.5, 0.1)
res_beta_binom <- function(x, size = 1, prob = 0.5, theta = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_beta_binom(length(x), size = size, prob = prob, theta = theta)
  }
  switch(type,
         data = x,
         raw = x - size * prob,
         dev = dev_beta_binom(x, size = size, prob = prob, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
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
         data = x,
         raw = x - prob,
         dev = dev_bern(x, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
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
         data = x,
         raw = x - size * prob,
         dev = dev_binom(x, size = size, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

#' Gamma Residuals
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_gamma(c(0, 1, 2), 1, 2)
res_gamma <- function(x, shape = 1, rate = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_gamma(length(x), shape = shape, rate = rate)
  }
  switch(type,
         data = x,
         raw = x - shape / rate,
         dev = dev_gamma(x, shape = shape, rate = rate, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

#' Gamma-Poisson Residuals
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
         data = x,
         raw = x - lambda,
         dev = dev_gamma_pois(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

#' Zero-Inflated Gamma-Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_gamma_pois_zi(c(0, 1, 2), 1, 1, 0.5)
res_gamma_pois_zi <- function(x, lambda = 1, theta = 0, prob = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_gamma_pois_zi(length(x), lambda = lambda, theta = theta, prob = prob)
  }
  switch(type,
         data = x,
         raw = x - lambda * (1 - prob),
         dev = dev_gamma_pois_zi(x, lambda, theta = theta, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
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
         data = x,
         raw = x - exp(meanlog),
         dev = dev_lnorm(x, meanlog = meanlog, sdlog = sdlog, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
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
         data = x,
         raw = x - lambda,
         dev = dev_neg_binom(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
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
         data = x,
         raw = x - mean,
         dev = dev_norm(x, mean = mean, sd = sd, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

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
         data = x,
         raw = x - lambda,
         dev = dev_pois(x, lambda, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

#' Zero-Inflated Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_pois_zi(c(1,3.5,4), 6, 0.5, type = "raw")
res_pois_zi <- function(x, lambda = 1, prob = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_pois_zi(length(x), lambda = lambda, prob = prob)
  }
  switch(type,
         data = x,
         raw = x - lambda * (1 - prob),
         dev = dev_pois_zi(x, lambda, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}

#' Student's t Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_student(c(1,3.5,4), mean = 6, sd = 0.5, theta = 1/3, type = "raw")
res_student <- function(x, mean = 0, sd = 1, theta = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_student(length(x), mean = mean, sd = sd, theta = theta)
  }
  switch(type,
         data = x,
         raw = x - mean,
         dev = dev_student(x, mean = mean, sd = sd, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev")))
}
