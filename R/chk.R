#' Check Parameter Names
#'
#' Checks if valid parameter names using `[vld_pars](x)`.
#'
#' @inheritParams params
#' @inheritParams chk::chk_flag
#' @return `NULL`, invisibly. Called for the side effect of throwing an error
#'   if the condition is not met.
#' @export
#' @examples
#' x <- c("x", "a1._", "X")
#' chk_pars(x)
#' y <- c("x[1]", "a1", "a1", "._0")
#' try(chk_pars(y))
chk_pars <- function(x, x_name = NULL) {
  if (vld_pars(x)) {
    return(invisible())
  }
  if (is.null(x_name)) x_name <- deparse_backtick_chk(substitute(x))

  chk_s3_class(x, "character", x_name = x_name)
  chk_not_any_na(x, x_name = x_name)
  chk_unique(x, x_name = x_name)
  chk_match(x, p0("^[[:alpha:]][[:alnum:]._]*$"), x_name = x_name)
}

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

