#' Adjust Normal Distribution Parameters for Sensitivity Analyses
#'
#' @param mean
#' @param sd
#' @param sd_mult
#'
#' @return
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
  return(c(mean = mean, sd = new_sd))
}

#' Adjust Student's t Distribution Parameters for Sensitivity Analyses
#'
#' @param mean
#' @param sd
#' @param theta
#' @param sd_mult
#'
#' @return
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
  return(c(mean = mean, sd = new_sd, theta = theta))
}

#' Adjust Skew Normal Distribution Parameters for Sensitivity Analyses
#'
#' @param mean
#' @param sd
#' @param shape
#' @param sd_mult
#'
#' @return
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
  return(c(mean = mean, sd = new_sd, theta = theta))
}

#' Adjust Log Normal Distribution Parameters for Sensitivity Analysis
#'
#' @param meanlog
#' @param sdlog
#' @param sd_mult
#'
#' @return
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

  return(c(meanlog = new_meanlog, sdlog = new_sdlog))
}

#' Adjust Exponential Distribution Parameters for Sensitivity Analyses
#'
#' @param rate
#' @param sd_mult
#'
#' @return
#' @export
#'
#' @examples
#' sens_exp(10, 2)
#' sens_exp(10, 0.8)
sens_exp <- function(rate, sd_mult = 2) {
  chk::chk_number(rate)
  chk::chk_gte(rate, value = 0)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  sd <- 1 / rate
  new_sd <- sd * sd_mult
  new_rate <- 1 / new_sd

  return(rate = new_rate)
}

#' Adjust Beta Distribution Parameters for Sensitivity Analyses
#'
#' @param alpha
#' @param beta
#' @param sd_mult
#'
#' @return
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

  # Keep same mean and change variance
  new_var_x <- (sqrt(var_x) * sd_mult)^2
  alpha_plus_beta <- mean_x * (1 - mean_x) / new_var_x - 1

  new_alpha <- mean_x * alpha_plus_beta
  new_beta <- (1 - mean_x) * alpha_plus_beta

  # If new variance is > max variance
  # Or new_alpha and/or new_beta are < 1 (results in u-shaped dist if both < 1 and exponential-type dist if one < 1)
  # Set both new shape parameters to 1 (results in uniform distribution)
  if (new_var_x >= (mean_x * (1 - mean_x)) | new_alpha < 1 | new_beta < 1) {
    new_alpha <- 1
    new_beta <- 1
  }

  return(c(alpha = new_alpha, beta = new_beta))
}

#' Adjust Poisson Distribution Parameters for Sensitivity Analysis
#'
#' @param lambda
#' @param sd_mult
#'
#' @return
#' @export
#'
#' @examples
#' sens_pois(10, 2)
#' sens_pois(10, 0.8)
sens_pois <- function(lambda, sd_mult = 2) {
  chk::chk_number(lambda)
  chk::chk_gte(lambda, value = 0)
  new_lambda <- lambda * sd_mult^2
  return(lambda = new_lambda)
}

#' Adjust Negative Binomial Distribution Parameters for Sensitivity Analyses
#'
#' @param lambda
#' @param theta
#' @param sd_mult
#'
#' @return
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
  original_mean <- lambda
  original_sd <- sqrt(lambda + lambda^2 / size)
  desired_sd <- original_sd * sd_mult
  new_size <- lambda^2 / (desired_sd^2 - lambda)
  new_theta <- 1 / new_size

  return(c(lambda = lambda, theta = new_theta))
}

#' Adjust Gamma-Poisson Distribution Parameters for Sensitivity Analyses
#'
#' @param lambda
#' @param theta
#' @param sd_mult
#'
#' @return
#' @export
#'
#' @examples
#' sens_gamma_pois(10, 0.1, 2)
sens_gamma_pois <- function(lambda, theta, sd_mult = 2) {
  sens_neg_binom(lambda = lambda, theta = theta, sd_mult = sd_mult)
}

#' Adjust Zero-Inflated Poisson Distribution Parameters for Sensitivity Analyses
#'
#' @param lambda
#' @param prob
#' @param sd_mult
#'
#' @return
#' @export
#'
#' @examples
#' sens_pois_zi(10, 0.1, 2)
#' sens_pois_zi(10, 0.1, 0.8)
sens_pois_zi <- function(lambda, prob, sd_mult = 2) {
  chk::chk_number(lambda)
  chk::chk_gte(lambda, value = 0)
  chk::chk_number(prob)
  chk::chk_gte(prob, value = 0)
  chk::chk_range(prob, range = c(0, 1), inclusive = TRUE)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  if (prob == 0) {
    new_lambda <- lambda * sd_mult^2
    return(c(lambda = new_lambda, prob = prob))
  }

  # Solution to quadratic equation
  new_lambda <- (sqrt(4 * sd_mult^2 * lambda * prob * (lambda * prob + 1) + 1) + 1) / (2 * prob)

  return(c(lambda = new_lambda, prob = prob))
}

#' Adjust Zero-Inflated Gamma-Poisson Distribution Parameters for Sensitivity Analyses
#'
#' @param lambda
#' @param theta
#' @param prob
#' @param sd_mult
#'
#' @return
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
  chk::chk_gte(prob, value = 0)
  chk::chk_range(prob, range = c(0, 1), inclusive = TRUE)
  chk::chk_number(sd_mult)
  chk::chk_gt(sd_mult, value = 0)

  if (sd_mult < 1) {
    stop("This function does not currently have the ability to reduce the standard deviation.")
  }

  original_mean <- lambda * (1 - prob)
  original_sd <- sqrt(lambda * (1 - prob) * (1 + lambda * (prob + theta)))

  new_theta <- (sd_mult^2 * (1 + lambda * (prob + theta)) - (lambda * prob) - 1) / lambda
  return(c(lambda = lambda, theta = new_theta, prob = prob))
}
