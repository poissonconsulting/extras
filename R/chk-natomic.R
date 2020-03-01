#' Check Numeric Atomic Object
#'
#' Checks if numeric atomic object using
#' `is.numeric(x) && is.atomic(x)`.
#'
#' @inheritParams chk::chk_true
#' @return
#' The `chk_` function throws an informative error if the test fails.
#'
#' The `vld_` function returns a flag indicating whether the test was met.
#' @export
#'
#' @examples
#'
#' # chk_natomic
#' chk_natomic(1)
#' chk_natomic(matrix(1L))
#' try(chk_natomic(TRUE))
chk_natomic <- function(x, x_name = NULL) {
  if (vld_natomic(x)) {
    return(invisible())
  }
  if (is.null(x_name)) x_name <- deparse_backtick_chk(substitute(x))
  abort_chk(
    x_name, " must be a numeric (integer or double)",
    " atomic (vector, matrix or array) object."
  )
}

#' @describeIn chk_natomic Validate Numeric Atomic Object
#'
#' @export
#' @examples
#'
#' #' vld_natomic
#' vld_natomic(1)
#' vld_natomic(matrix(1L))
#' try(vld_natomic(TRUE))
vld_natomic <- function(x) is.numeric(x) && is.atomic(x)
