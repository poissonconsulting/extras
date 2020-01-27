#' @export
universals::fill_all

.fill_all <- function(x, value, nas) {
  y <- rep(value, length(x))
  if(ndims(x) > 1)
    dim(y) <- dim(x)
  if(!nas) {
    is.na(y[is.na(x)]) <- TRUE
  }
  y
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
fill_all.logical <- function(x, value = FALSE, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)
  
  value <- as.logical(value)
  .fill_all(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
fill_all.integer <- function(x, value = 0L, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)
  
  value <- as.integer(value)
  .fill_all(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
fill_all.numeric <- function(x, value = 0, nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)
  
  value <- as.numeric(value)
  .fill_all(x, value, nas)
}

#' @inherit universals::fill_all
#' @inheritParams params
#' @export
fill_all.character <- function(x, value = "0", nas = TRUE, ...) {
  chk_scalar(value)
  chk_flag(nas)
  chk_unused(...)
  
  value <- as.character(value)
  .fill_all(x, value, nas)
}
