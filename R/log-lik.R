#' Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_pois(c(1,3.5,4), 3)
log_lik_pois <- function(x, lambda = 1) {
  dpois(x, lambda, log = TRUE)
}

#' Normal Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' dev_norm(c(-2:2))
log_lik_norm <- function(x,  mean = 0, sd = 1) {
  dnorm(x, mean = mean, sd = sd, log = TRUE)
}

#' Log-Normal Log-Likelihood
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' dev_norm(exp(c(-2:2)))
log_lik_lnorm <- function(x,  meanlog = 0, sdlog = 1) {
  dlnorm(x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
}

#' Binomial Log-Likelihood
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_binom(c(0, 1, 2), 2, 0.3)
log_lik_binom <- function(x, size = 1, prob = 0.5) {
  dbinom(x, size = size, prob = prob, log = TRUE)
}

#' Bernoulli Log-Likelihood
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_bern(c(TRUE, FALSE), 0.7)
log_lik_bern <- function(x, prob = 0.5) {
  log_lik_binom(x, size = 1, prob = prob)
}

#' Negative Binomial Log-Likelihood
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_neg_binom(c(0, 1, 2), 2, 1)
log_lik_neg_binom <- function(x, lambda = 1, theta = 0) {
  dnbinom(x, mu = lambda, size = 1/theta, log = TRUE)
}

#' Gamma Poisson Log-Likelihood
#'
#' @inheritParams params
#' @param x A vector of 0s and 1s.
#'
#' @return An numeric vector of the corresponding log-likelihoods.
#' @family log_lik_dist
#' @export
#'
#' @examples
#' log_lik_gamma_pois(c(0, 1, 2), 1, 1)
log_lik_gamma_pois <- function(x, lambda = 1, theta = 0) {
  log_lik_neg_binom(x, lambda = lambda, theta = theta)
}
