#' Fill Missing Values
#'
#' Fills all of an object's missing values
#' while preserving the object's dimensionality and class.
#'
#' It should only be defined for objects with values of consistent class ie
#' not standard data.frames.
#'
#' @inheritParams params
#' @return The modified object.
#' @family fill
#' @export
fill_na <- function(x, value, ...) UseMethod("fill_na")

#' @describeIn fill_na Fill Missing Values for logical Objects
#' @export
#' @examples
#'
#' # logical
#' fill_na(c(TRUE, NA))
fill_na.logical <- function(x, value = FALSE, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.logical(value)
  x[is.na(x)] <- value
  x
}

#' @describeIn fill_na Fill Missing Values for integer Objects
#' @export
#' @examples
#'
#' # integer
#' fill_na(c(1L, NA), 0)
fill_na.integer <- function(x, value = 0L, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.integer(value)
  x[is.na(x)] <- value
  x
}

#' @describeIn fill_na Fill Missing Values for numeric Objects
#' @export
#' @examples
#'
#' # numeric
#' fill_na(c(1, NA), Inf)
fill_na.numeric <- function(x, value = 0, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.numeric(value)
  x[is.na(x)] <- value
  x
}

#' @describeIn fill_na Fill Missing Values for character Objects
#' @export
#' @examples
#'
#' # character
#' fill_na(c("text", NA))
#' fill_na(matrix(c("text", NA)), value = Inf)
fill_na.character <- function(x, value = "0", ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.character(value)
  x[is.na(x)] <- value
  x
}
