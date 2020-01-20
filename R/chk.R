#' Check Function Template
#'
#' A chk function template.
#'
#' Checks if character using `[vld_function_template](x)`.
#'
#' @inheritParams chk::chk_flag
#' @return `NULL`, invisibly. Called for the side effect of throwing an error
#'   if the condition is not met.
#' @export
#' @examples
#' chk_function_template("1")
#' try(chk_function_template(1))
chk_function_template <- function(x, x_name = NULL) {
  if (vld_function_template(x)) {
    return(invisible())
  }
  if (is.null(x_name)) x_name <- deparse_backtick(substitute(x))
  abort_chk(x_name, " must be a character.")
}
