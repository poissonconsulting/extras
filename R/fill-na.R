#' @export
universals::fill_na

#' @inherit universals::fill_na
#' @export
#' @examples
#' fill_na(c(TRUE, NA))
fill_na.logical <- function(x, value = FALSE, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.logical(value)
  x[is.na(x)] <- value
  x
}

#' @inherit universals::fill_na
#' @export
#' @examples
#' fill_na(c(1L, NA), 0)
fill_na.integer <- function(x, value = 0L, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.integer(value)
  x[is.na(x)] <- value
  x
}

#' @inherit universals::fill_na
#' @export
#' @examples
#' fill_na(c(1, NA), Inf)
fill_na.numeric <- function(x, value = 0, ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.numeric(value)
  x[is.na(x)] <- value
  x
}

#' @inherit universals::fill_na
#' @export
#' @examples
#' fill_na(c("text", NA))
#' fill_na(matrix(c("text", NA)), value = Inf)
fill_na.character <- function(x, value = "0", ...) {
  chk_scalar(value)
  chk_unused(...)
  value <- as.character(value)
  x[is.na(x)] <- value
  x
}
