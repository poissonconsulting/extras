#' @export
universals::fill_all

fill_all_impl <- function(x, value, nas) {
  is_na <- is.na(x)
  x[] <- value
  if (!nas) {
    is.na(x[is_na]) <- TRUE
  }
  x
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
#' @examples
#' fill_all(c(TRUE, NA, FALSE))
#' fill_all(c(TRUE, NA, FALSE, nas = FALSE))
#' fill_all(c(TRUE, NA, FALSE, value = NA))
fill_all.logical <- function(x, value = FALSE, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)

  value <- as.logical(value)
  fill_all_impl(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
#' @examples
#' fill_all(matrix(1:4, nrow = 2), value = -1)
fill_all.integer <- function(x, value = 0L, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)

  value <- as.integer(value)
  fill_all_impl(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
#' @examples
#' fill_all(c(1,4,NA), value = TRUE)
#' fill_all(c(1,4,NA), value = TRUE, nas = FALSE)
fill_all.numeric <- function(x, value = 0, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)

  value <- as.numeric(value)
  fill_all_impl(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
#' @examples
#' fill_all(c("some", "words"), value = TRUE)
fill_all.character <- function(x, value = "0", nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)

  value <- as.character(value)
  fill_all_impl(x, value, nas)
}
