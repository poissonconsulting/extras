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
res_pois <- function(x, lambda, type = "dev") {
  chk_string(type)
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
res_norm <- function(x,  mean = 0, sd = 1, type = "dev") {
  chk_string(type)
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
res_lnorm <- function(x,  meanlog = 0, sdlog = 1, type = "dev") {
  x <- pmax(x, 0)
  res_norm(log(x), mean = meanlog, sd = sdlog, type = type)
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
res_binom <- function(x, size, prob, type = "dev") {
  chk_string(type)
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
res_bern <- function(x, prob, type = "dev") {
  chk_string(type)
  switch(type,
         raw = x - prob,
         dev = dev_bern(x, prob = prob, res = TRUE),
         chk_subset(x, c("raw", "dev")))
}
