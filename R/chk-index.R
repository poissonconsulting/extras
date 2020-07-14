#' Check Index
#'
#' Checks if an object is a vector of one or more positive integer values.
#'
#' @inheritParams params
#' @inheritParams chk::chk_flag
#' @return
#' The `chk_` function throws an informative error if the test fails.
#'
#' The `vld_` function returns a flag indicating whether the test was met.
#' @export
#' @examples
#' x <- c(2L, 1L)
#' chk_index(x)
#' y <- c(2L, -1L)
#' try(chk_index(y))
chk_index <- function(x, x_name = NULL) {
  if (vld_index(x)) {
    return(invisible())
  }
  if (is.null(x_name)) x_name <- deparse_backtick_chk(substitute(x))

  chk_integer(x, x_name = x_name)
  chk_gt(x, x_name = x_name)
  chk_not_any_na(x, x_name = x_name)
  chk_not_empty(x, x_name = x_name)
}

#' @describeIn chk_index Validate Index
#'
#' @export
#' @examples
#' vld_index(c(-1))
#' vld_index(c(3L, 1L))
vld_index <- function(x) {
  vld_integer(x) & vld_gt(x) & vld_not_any_na(x) & vld_not_empty(x)
}
