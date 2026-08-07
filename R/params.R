#' Parameter Descriptions
#'
#' Default parameter descriptions which may be overridden in individual
#' functions.
#'
#' A flag is a non-missing logical scalar.
#'
#' A string is a non-missing character scalar.
#'
#' @keywords internal
#' @name params
#' @param ... Other arguments passed to methods.
#' @param alpha The first shape parameter of the beta distribution.
#' @param beta The second shape parameter of the beta distribution.
#' @param conf_level A numeric scalar between 0 and 1 specifying the confidence
#'   level.
#' @param directional A flag specifying whether probabilities less than 0.5
#'   should be returned as negative values.
#' @param group A vector identifying which rows belong to the same
#'   multinomial trial, i.e., whose `x` values must sum to `size` and whose
#'   `prob` values must sum to 1. This is for ordinary multinomial logistic
#'   regression, where every trial has the same fixed set of possible
#'   categories: every group must contain at least 2 rows (a trial needs at
#'   least 2 categories), and every group must have the same number of rows
#'   (the most common number of rows across the data). `group` must not
#'   contain `NA`, since there's no way to know which trial an unlabelled
#'   row belongs to. `NA` in `size` or `prob` for any row of a trial makes
#'   the result `NA` for every row of that trial (not just the row it
#'   appears in), since the categories within a single multinomial trial
#'   are not independent -- they're drawn/scored jointly.
#' @param lambda A non-negative numeric vector of means.
#' @param level A number > 0 and <= 1 specifying the probability coverage of the
#' interval.
#' @param log A flag specifying whether to return the log-transformed value.
#' @param lower.tail A flag specifying whether to return the lower or upper tail
#'   of the distribution.
#' @param min A numeric vector of the minimums.
#' @param mean A numeric vector of the means.
#' @param meanlog A numeric vector of the means on the log scale.
#' @param max A numeric vector of the maximums.
#' @param n A non-negative whole number of the number of random samples to
#'   generate.
#' @param na_rm A flag specifying whether to remove missing values.
#' @param nas A flag specifying whether to also fill missing values.
#' @param p A numeric vector of probabilities.
#' @param prob A numeric vector of values between 0 and 1 of the probability of
#'   success.
#' @param q A vector of quantiles.
#' @param rate A non-negative numeric vector of rate.
#' @param res A flag specifying whether to return the deviance residual as
#'   opposed to the deviance.
#' @param scale A non-negative numeric vector of the scale.
#' @param sd A non-negative numeric vector of the standard deviations.
#' @param sd_mult A non-negative multiplier on the standard deviation of the
#'   distribution.
#' @param sdlog A non-negative numeric vector of the standard deviations on the
#'   log scale.
#' @param shape A non-negative numeric vector of shape.
#' @param simulate A flag specifying whether to simulate residuals.
#' @param size A non-negative whole numeric vector of the number of trials.
#' @param skeptical A flag specifying whether or not to add one sample to the
#' empty side of the threshold when 100% of samples are on one side. Avoids
#' zero p-values and infinite s-values, and also imposes stronger bounds on
#' directional information than \[-n, n\], which assume the MCMC samples are
#' independent and representative.
#' @param theta A non-negative numeric vector of the dispersion for the mixture
#'   models (student, gamma-Poisson and beta-binomial).
#' @param threshold A number of the threshold value.
#' @param tlower A numeric vector of the lower truncation point.
#' @param tupper A numeric vector of the upper truncation point.
#' @param type A string of the residual type. 'raw' for raw residuals 'dev' for
#'   deviance residuals and 'data' for the data.
#' @param value A scalar of the value to replace values with.
#' @param x An object.
#' @aliases parameters arguments args
#' @usage NULL
#' @export
# nocov start
params <- function(...) NULL
# nocov end
