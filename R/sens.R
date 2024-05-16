#' Adjust Normal Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Normal distribution without changing the mean.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_norm(10, 3, 2)
#' sens_norm(10, 3, 0.8)
sens_norm <- function(mean, sd, sd_mult = 2) {
  chk::chk_number(mean)
  chk::chk_number(sd)
  chk::chk_gte(sd, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)
  new_sd <- sd * sd_mult
  return(list(mean = mean, sd = new_sd))
}

#' Adjust Student's t Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Student's t distribution. Because the variance of this distribution
#' is not defined for every degree of freedom, the adjustment to the standard
#' deviation is approximate, and the mean of the adjusted distribution can
#' be expected to have shifted.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_student(10, 3, 0.1, 2)
#' sens_student(10, 3, 0.1, 0.8)
sens_student <- function(mean, sd, theta, sd_mult = 2) {
  chk::chk_number(mean)
  chk::chk_number(sd)
  chk::chk_gte(sd, value = 0)
  chk::chk_number(theta)
  chk::chk_gte(theta, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)
  new_sd <- sd * sd_mult
  return(list(mean = mean, sd = new_sd, theta = theta))
}

#' Adjust Skew Normal Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Skew Normal distribution without changing the mean.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_skewnorm(10, 3, -1, 2)
#' sens_skewnorm(10, 3, 3, 0.8)
sens_skewnorm <- function(mean, sd, shape, sd_mult = 2) {
  chk::chk_number(mean)
  chk::chk_number(sd)
  chk::chk_gte(sd, value = 0)
  chk::chk_number(shape)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)
  new_sd <- sd * sd_mult
  original_mean <- mean + sd * (shape / sqrt(1 + shape^2)) * sqrt(2 / pi)
  new_mean <- mean + new_sd * (shape / sqrt(1 + shape^2)) * sqrt(2 / pi)
  diff_means <- new_mean - original_mean
  adjusted_mean <- mean - diff_means
  return(list(mean = adjusted_mean, sd = new_sd, shape = shape))
}

#' Adjust Log-Normal Distribution Parameters for Sensitivity Analysis
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Log-Normal distribution. With high values of `sdlog` (i.e., `> 9`),
#' and `sd_mult > 1`, the mean of the adjusted distribution can be expected to
#' have a mean value that is very different from the original mean, however,
#' the proportional difference in these values should not be very different.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_lnorm(0, 1, 2)
#' sens_lnorm(0, 1, 0.8)
sens_lnorm <- function(meanlog, sdlog, sd_mult = 2) {
  chk::chk_number(meanlog)
  chk::chk_number(sdlog)
  chk::chk_gte(sdlog, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  original_mean <- exp(meanlog + sdlog^2 / 2)
  original_sd <- sqrt((exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2))
  desired_mean <- original_mean
  desired_sd <- original_sd * sd_mult

  new_meanlog <- log(desired_mean^2 / sqrt(desired_mean^2 + desired_sd^2))
  new_sdlog <- sqrt(log(1 + desired_sd^2 / desired_mean^2))

  return(list(meanlog = new_meanlog, sdlog = new_sdlog))
}

#' Adjust Exponential Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the exponential distribution. Due to the parameterization of this
#' distribution, adjusting the standard deviation necessarily changes the mean
#' value.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_exp(10, 2)
#' sens_exp(10, 0.8)
sens_exp <- function(rate, sd_mult = 2) {
  chk::chk_number(rate)
  chk::chk_gt(rate, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  sd <- 1 / rate
  new_sd <- sd * sd_mult
  new_rate <- 1 / new_sd

  return(list(rate = new_rate))
}

#' Adjust Beta Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Beta distribution. The Beta distribution has a maximum variance of
#' `mean(x) * (1 - mean(x)`, where `mean(x) = alpha / (alpha + beta)`. If the
#' inputs produce a desired variance that is greater than the maximum possible
#' variance, or provides alpha and/or beta parameters that are `< 1` and thus
#' push more probability weight towards extreme probability values, this
#' function returns `alpha = 1` and `beta = 1` (the uniform distribution).
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_beta(10, 10, 2)
#' sens_beta(10, 10, 0.8)
sens_beta <- function(alpha, beta, sd_mult = 2) {
  chk::chk_number(alpha)
  chk::chk_gt(alpha, value = 0)
  chk::chk_number(beta)
  chk::chk_gt(beta, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  mean_x <- alpha / (alpha + beta)
  var_x <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))

  new_var_x <- (sqrt(var_x) * sd_mult)^2
  alpha_plus_beta <- mean_x * (1 - mean_x) / new_var_x - 1

  new_alpha <- mean_x * alpha_plus_beta
  new_beta <- (1 - mean_x) * alpha_plus_beta

  if (new_var_x >= (mean_x * (1 - mean_x)) || new_alpha < 1 || new_beta < 1) {
    new_alpha <- 1
    new_beta <- 1
  }

  return(list(alpha = new_alpha, beta = new_beta))
}

#' Adjust Poisson Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Poisson distribution. Due to the parameterization of this
#' distribution, adjusting the standard deviation necessarily changes the mean
#' value.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_pois(10, 2)
#' sens_pois(10, 0.8)
sens_pois <- function(lambda, sd_mult = 2) {
  chk::chk_number(lambda)
  chk::chk_gte(lambda, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)
  new_lambda <- lambda * sd_mult^2
  return(list(lambda = new_lambda))
}

#' Adjust Gamma Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Gamma distribution.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_gamma(10, 2, 2)
#' sens_gamma(10, 2, 0.2)
sens_gamma <- function(shape, rate, sd_mult = 2) {
  chk::chk_number(shape)
  chk::chk_gt(shape, value = 0)
  chk::chk_number(rate)
  chk::chk_gt(rate, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  var <- shape / rate^2
  new_var <- var * sd_mult^2

  new_rate <- shape / (rate * new_var)
  new_shape <- (shape * new_rate) / rate

  return(list(shape = new_shape, rate = new_rate))
}

#' Adjust Negative Binomial Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) the standard deviation of the Negative Binomial
#' distribution. This function does not currently have the option to reduce the
#' standard deviation.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_neg_binom(10, 0.1, 2)
sens_neg_binom <- function(lambda, theta, sd_mult = 2) {
  chk::chk_number(lambda)
  chk::chk_gte(lambda, value = 0)
  chk::chk_number(theta)
  chk::chk_gte(theta, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  if (sd_mult < 1) {
    stop("This function does not currently have the option to reduce the standard deviation.")
  }

  size <- 1 / theta
  original_sd <- sqrt(lambda + lambda^2 / size)
  desired_sd <- original_sd * sd_mult
  new_size <- lambda^2 / (desired_sd^2 - lambda)
  new_theta <- 1 / new_size

  return(list(lambda = lambda, theta = new_theta))
}

#' Adjust Gamma-Poisson Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) the standard deviation of the Negative Binomial
#' distribution. This function does not currently have the option to reduce the
#' standard deviation.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_gamma_pois(10, 0.1, 2)
sens_gamma_pois <- function(lambda, theta, sd_mult = 2) {
  sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
}

#' Adjust Zero-Inflated Gamma-Poisson Distribution Parameters for Sensitivity Analyses
#'
#' Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard deviation
#' of the Zero-Inflated Gamma-Poisson distribution.
#'
#' @inheritParams params
#'
#' @return A named list of the adjusted distribution's parameters.
#' @family sens_dist
#' @export
#'
#' @examples
#' sens_gamma_pois_zi(10, 0.1, 0.3, 2)
sens_gamma_pois_zi <- function(lambda, theta, prob, sd_mult = 2) {
  chk::chk_number(lambda)
  chk::chk_gte(lambda, value = 0)
  chk::chk_number(theta)
  chk::chk_gte(theta, value = 0)
  chk::chk_number(prob)
  chk::chk_range(prob, range = c(0, 1), inclusive = TRUE)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  if (sd_mult < 1) {
    stop("This function does not currently have the option to reduce the standard deviation.")
  }

  new_theta <- (sd_mult^2 * (1 + lambda * (prob + theta)) - (lambda * prob) - 1) / lambda
  return(list(lambda = lambda, theta = new_theta, prob = prob))
}
