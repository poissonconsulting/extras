#' Posterior summary
#'
#' A convenience function for returning a summary of a vector of MCMC samples
#' that includes the parameter estimate (median), a credible interval,
#' probability of direction relative to a threshold, directional information,
#' and KL divergence from a normal distribution.
#'
#' @param x A numeric vector of MCMC values.
#' @param conf_level A numeric value indicating the probability coverage of the
#' Bayesian credible interval.
#' Must be within the interval `0 < conf_level < 1`
#' @param ci_side A string indicating whether to estimate a one-sided credible
#' interval (`"left"` or `"right"`) or a two-sided credible interval
#' (`"two-sided"`; default).
#' @param format_threshold A logical flag indicating whether to format the
#' threshold using scientific notation in the output if `abs(log(abs(threshold))) > 3`.
#' Defaults to `TRUE`.
#' @inheritParams params
#' @return A [`data.frame`]
#' @family summary
#' @export
#' @examples
#' posterior_summary(rnorm(100), side = "right")

posterior_summary <- function(x, side = NULL, threshold = 0,
                              threshold_split = "proportional",
                              na_rm = FALSE, format_threshold = TRUE) {
  chk_numeric(x)
  chk_null_or(side, vld = vld_subset, values = c("left", "right"))
  chk_numeric(threshold)
  chk_subset(threshold_split, c("left", "right", "equal", "proportional", "exclude"))
  chk_logical(na_rm)
  chk_logical(format_threshold)

  if (format_threshold & (abs(log10(abs(threshold))) > 3 & threshold != 0)) {
    threshold_sting <- formatC(threshold, format = "e", digits = 3)
  } else {
    threshold_sting <- threshold
  }

  if(na_rm) {
    x <- x[!is.na(x)]
  }


  out <-
    data.frame(
      # assuming parameter name is provided outside posterior_summary()
      median = median(x),
      direction = paste("estimate", ifelse(side == "left", "<", ">"), threshold_sting),
      probability_direction = probability_direction(x = x, side = side, threshold = threshold),
      directional_information = directional_information(x = x, side = side, threshold = threshold,
                                                        threshold_split = threshold_split),
      mean = mean(x),
      sd = sd(x),
      normal_divergence = kl_divergence(x = x, ref_pars = c(mean(x), sd(x)),
                                        distribution = dnorm)
    )
  rownames(out) <- NULL
  out
}
