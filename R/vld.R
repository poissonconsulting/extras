#' Validate Parameter Names
#'
#' Validates a character vector of parameter names.
#' 
#' The character vector must consist of unique, non-missing values.
#' 
#' @inheritParams params
#' @inheritParams chk::chk_flag
#' @return A flag indicating whether the condition was met.
# @seealso [chk_pars()]
#' @export
#' @examples
#' vld_pars(c("x", "a1._", "X"))
#' vld_pars(c("x[1]", "a1", "a1", "._0"))
vld_pars <- function(x) {
  vld_s3_class(x, "character") &&
  vld_not_any_na(x) &&
  vld_unique(x) &&
  vld_match(x, p0("^[[:alpha:]][[:alnum:]._]*$"))
}
