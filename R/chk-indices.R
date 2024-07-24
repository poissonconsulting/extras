#' Check Indices
#'
#' Checks if an object is a list of indices ie vectors of one or more positive integer values.
#'
#' @inheritParams params
#' @inheritParams chk::chk_flag
#' @return
#' The `chk_` function throws an informative error if the test fails.
#'
#' The `vld_` function returns a flag indicating whether the test was met.
#' @export
#' @examples
#' x <- list(c(2L, 1L))
#' chk_indices(x)
#' y <- c(2L, 1L)
#' try(chk_indices(y))
chk_indices <- function(x, x_name = NULL) {
  if (vld_indices(x)) {
    return(invisible())
  }
  if (is.null(x_name)) x_name <- deparse_backtick_chk(substitute(x))
  chk_list(x, x_name = x_name)
  chk_all(x, chk_index, x_name = x_name)
}

#' @describeIn chk_indices Validate Indices
#'
#' @export
#' @examples
#' vld_indices(c(3L, 1L))
#' vld_indices(list(c(3L, 1L)))
vld_indices <- function(x) {
  vld_list(x) &&
    vld_all(x, vld_index)
}
