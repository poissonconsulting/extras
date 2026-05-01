#' Directional information
#'
#' Quantifies the information about direction in a posterior distribution based on the directional probability.
#' This function calculates such information using absolute difference in the the probability of direction (see [`probability_direction()`]), after converting each probability to bits (also see [`svalue()`].
#'
#' @param x A numeric vector of MCMC values.
#' @param side A string indicating whether to calculate
#' the directional information relative to the left side (`"left"`; `x < threshold`),
#' or the right side (`"right"`; `x > threshold`). Positive information suggests
#' greater evidence for the specified side.
#' Defaults to `"median"`, which uses the side of the median of `x` via
#' [`direction()`].
#' @param threshold_split A string indicating how to deal
#' with threshold values:
#'
#' - `"left"` to include them on the left side,
#' - `"right"` to include them on the right side,
#' - `"equal"` to split them equally between the left and side,
#' - `"proportional"` (default) to split them between the left and right sides
#'    proportionally to the values of `x` on the left and right sides,
#' - `"exclude"` to drop the values of `x` equal to `threshold`
#'   (identical to using `"proportional"`).
#'
#' @inheritParams params
#' @return A number indicating the directional information.
#' @family summary
#' @references
#' Kery, M., and Schaub, M. 2011.
#' Bayesian population analysis using WinBUGS: a hierarchical perspective.
#' Academic Press, Boston. Available from <https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.
#' @export
#' @examples
#'
#' directional_information(0)
#' directional_information(1) # one coin flip of information
#' directional_information(c(1, 1)) # two coin flips
#' directional_information(c(1, 1, -1)) # x[2] and x[3] cancel out
#' directional_information(c(1, 1, -1, -1)) # both sides cancel out
#' directional_information(rnorm(1e3, mean = 0))
#' directional_information(rnorm(1e3, mean = 1))
#' directional_information(rnorm(1e3, mean = 10)) # all coin flips are positive
#' directional_information(rnorm(1e3, mean = -10)) # all coin flips are negative
#' directional_information(rnorm(1e3, mean = 1e3)) # only quantiles matter
#' directional_information(rnorm(1e6, mean = 1e3)) # more `x` implies more info

directional_information <- function(x, side = "median", threshold = 0,
                                    threshold_split = "proportional",
                                    na_rm = FALSE) {
  chk_numeric(x)
  chk_subset(side, c("left", "right", "median"))
  chk_number(threshold)
  chk_subset(threshold_split, c("left", "right", "equal", "proportional", "exclude"))
  chk_flag(na_rm)

  if (anyNA(x)) {
    if (vld_true(na_rm)) {
      x <- as.vector(x)
      x <- x[!is.na(x)]
    } else {
      return(NA_real_)
    }
  }

  if (any(is.infinite(x))) {
    return(NA_real_)
  }

  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }

  if (side == "median") {
    side <- direction(x)
  }

  if (all(x == threshold)) {
    return(0)
  }

  p_l <- sum(x < threshold) / n # exclude threshold samples
  p_r <- sum(x > threshold) / n # exclude threshold samples
  p_t <- sum(x == threshold) / n

  p_lr <- p_l + p_r

  if (threshold_split == "left") {
    p_l <- p_l + p_t
  } else if (threshold_split == "right") {
    p_r <- p_r + p_t
  } else if (threshold_split == "equal") {
    p_l <- p_l + p_t / 2
    p_r <- p_r + p_t / 2
  } else { # proportional and exclude are effectively the same
    p_l <- p_l + p_t * (p_l / p_lr)
    p_r <- p_r + p_t * (p_r / p_lr)
  }

  if (side == "left") {
    i <- log2(p_l) - log2(p_r)
  } else {
    i <- log2(p_r) - log2(p_l)
  }

  i <- min(i, n) # max information difference is a bit for each sample
  i <- max(i, -n)
  # equivalent to, if o is the odds ratio:
  # if (is.infinite(o)) {
  #   if (o > 0) {
  #     o <- n # = n / (n+1) / (1 / (n+1))
  #   } else {
  #     o <- 1 / n # = 1 / (n+1) / (n / (n+1))
  #   }
  # }
  i
}
