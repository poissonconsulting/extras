#' Parameter Descriptions
#'
#' @keywords internal
#' @param x An object.
#' @param ... Other arguments passed to methods.
#' @param nas A flag specifying whether to also fill missing values.
#' @param n A non-negative whole number of the number of random samples to generate.
#' @param conf_level A numeric scalar between 0 and 1
#' specifying the confidence level.
#' @param value A scalar of the value to replace values with.
#' @param directional A flag specifying whether probabilities less than 0.5
#' should be returned as negative values.
#' @param res A flag specifying whether to return the deviance residual as opposed to the deviance.
#' @param mean A numeric vector of the means.
#' @param sd A non-negative numeric vector of the standard deviations.
#' @param meanlog A numeric vector of the means on the log scale.
#' @param sdlog A non-negative numeric vector of the standard deviations on the log scale.
#' @param size A non-negative whole numeric vector of the number of trials.
#' @param prob A numeric vector of values between 0 and 1 of the probability of success.
#' @param lambda A non-negative numeric vector of means.
#' @param theta A non-negative numeric vector of the shape and rate.
#' @param shape A non-negative numeric vector of shape.
#' @param rate A non-negative numeric vector of rate.
#' @param type A string of the residual type ('raw' or 'dev').
#' @param na_rm A flag specifying whether to remove missing values.
#' @param threshold A number of the threshold value.
#' @param simulate A flag specifying whether to simulate residuals.
#' @name params
NULL
