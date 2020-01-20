#' Deprecated Functions
#'
#' Deprecated `pkgtemplate` functions.
#'
#' @keywords internal
#' @name pkgtemplate_deprecated
NULL

#' @describeIn pkgtemplate_deprecated Check Function Template
#'
#' \lifecycle{soft-deprecated}
#'
#' @export
#'
#' @examples
#'
#' check_function_template("1")
check_function_template <- function(x, x_name = NULL) {
  deprecate_soft("0.0.0.9000",
    what = "check_function_template()",
    with = "chk_function_template()"
  )
  if (is.null(x_name)) x_name <- deparse_backtick(substitute(x))
  chk_function_template(x, x_name = x_name)
}
