res_beta_binom_standardized <- function(x, size, prob, theta) {
  res <- (x - size * prob) / sqrt(size * prob * (1 - prob))
  if (length(theta) == 1) {theta <- rep(theta, length(x))}
  bol <- is.na(theta)
  res[bol] <- NA_real_
  use_beta_binom <- !bol & theta > 0
  res[use_beta_binom] <- (x - size * prob) /
    sqrt((size * (4 * prob * (1 - prob) / theta) * (2 / theta + size)) /
           ((2 / theta)^2 * (2  / theta + 1)))
  return(res)
}

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
         standardized = res_beta_binom_standardized(x = x, size = size, prob = prob, theta = theta),
         dev = dev_beta_binom(x, size = size, prob = prob, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - prob) / sqrt(prob * (1 - prob)),
         dev = dev_bern(x, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - size * prob) / sqrt(size * prob * (1 - prob)),
         dev = dev_binom(x, size = size, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - shape / rate) / sqrt(shape / rate^2),
         dev = dev_gamma(x, shape = shape, rate = rate, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - lambda) / sqrt(lambda + theta * lambda^2),
         dev = dev_gamma_pois(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
}

#' Zero-Inflated Gamma-Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#' @param prob A numeric vector of values between 0 and 1 of the probability of zero-inflation.
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
         standardized = (x - lambda * (1 - prob)) / sqrt(lambda * (1 + lambda * theta)),
         dev = dev_gamma_pois_zi(x, lambda, theta = theta, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
#' res_norm(exp(c(-2:2)))
res_lnorm <- function(x,  meanlog = 0, sdlog = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_lnorm(length(x), meanlog = meanlog, sdlog = sdlog)
  }
  switch(type,
         data = x,
         raw = x - exp(meanlog),
         standardized = (x - exp(meanlog + (sdlog^2 / 2))) /
           sqrt(exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)),
         dev = dev_lnorm(x, meanlog = meanlog, sdlog = sdlog, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
}

#' Multinomial Residuals
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_multinomial(exp(c(-2:2)))
res_multinomial <- function(x, size = 1, prob = NULL, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    if (!is.matrix(x) & length(x) != 0) {
      x <- matrix(x, nrow = 1)
    }
    x <- ran_multinomial(nrow(x), size = size, prob = prob)
  }
  switch(type,
         data = x,
         # TODO: figure out what raw and standardized should be for whole row!
         raw = x - (size * prob), # do these dimensions work?
         standardized = (x - (size * prob)) / sqrt(size * prob * (1 - prob)), # same here?
         dev = dev_multinomial(x, size = size, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - lambda) / sqrt(lambda + theta * lambda^2),
         dev = dev_neg_binom(x, lambda = lambda, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
#' res_norm(c(-2:2))
res_norm <- function(x,  mean = 0, sd = 1, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_norm(length(x), mean = mean, sd = sd)
  }
  switch(type,
         data = x,
         raw = x - mean,
         standardized = (x - mean) / sd,
         dev = dev_norm(x, mean = mean, sd = sd, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = (x - lambda) / sqrt(lambda),
         dev = dev_pois(x, lambda, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
}

#' Zero-Inflated Poisson Residuals
#'
#' @inheritParams params
#' @param x A non-negative whole numeric vector of values.
#' @param prob A numeric vector of values between 0 and 1 of the probability of zero-inflation.
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
         standardized = (x - lambda * (1 - prob)) / sqrt((1 - prob) * lambda * (1 + lambda * prob)),
         dev = dev_pois_zi(x, lambda, prob = prob, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
}

res_student_standardized <- function(x, mean, sd, theta) {
  res <- rep(NA, length(x))
  if (length(mean) == 1) {mean <- rep(mean, length(x))}
  if (length(sd) == 1) {sd <- rep(sd, length(x))}
  if (length(theta) == 1) {theta <- rep(theta, length(x))}
  bol <- !is.na(theta)
  use_norm <- bol & theta == 0
  res[use_norm] <- ((x - mean) / sd)
  df <- 1 / theta
  df_var_undef <- bol & df <= 1
  res[df_var_undef] <- NaN
  df_var_inf <- bol & 1 < df & df <= 2
  res[df_var_inf] <- 0
  df_var_def <- bol & df > 2 & df < Inf
  res[df_var_def] <- ((x[df_var_def] - mean[df_var_def]) / (sd[df_var_def] *
                                sqrt((1 / theta[df_var_def]) / (1 / theta[df_var_def] - 2))))
  return(res)
}

#' Skew Normal Residuals
#'
#' @inheritParams params
#' @param x A numeric vector of values.
#' @param shape A numeric vector of shape.
#'
#' @return An numeric vector of the corresponding residuals.
#' @family res_dist
#' @export
#'
#' @examples
#' res_skewnorm(c(-2:2))
res_skewnorm <- function(x, mean = 0, sd = 1, shape = 0, type = "dev", simulate = FALSE) {
  chk_string(type)
  if(!vld_false(simulate)) {
    x <- ran_skewnorm(length(x), mean = mean, sd = sd, shape = shape)
  }
  switch(type,
         data = x,
         raw = x - mean + sd * (shape / sqrt(1 + shape^2)) * sqrt(2 / pi),
         standardized = (x - (mean + sd * (shape / sqrt(1 + shape^2)) * sqrt(2 / pi))) /
           (sd^2 * (1 - ((2 * (shape / sqrt(1 + shape^2))^2) / pi))),
         dev = dev_skewnorm(x, mean = mean, sd = sd, shape = shape, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
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
         standardized = res_student_standardized(x = x, mean = mean, sd = sd, theta = theta),
         dev = dev_student(x, mean = mean, sd = sd, theta = theta, res = TRUE),
         chk_subset(x, c("data", "raw", "dev", "standardized")))
}
