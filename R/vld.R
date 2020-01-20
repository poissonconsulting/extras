#' Validate Function Template
#'
#' A vld function template.
#'
#' Validates character using
#'
#' `is.character(x)`.
#'
#' @inheritParams chk::chk_flag
#' @return A flag indicating whether the condition was met.
#' @seealso [chk_function_template()]
#' @export
#' @examples
#' vld_function_template("1")
#' vld_function_template(1)
vld_function_template <- function(x) is.character(x)
