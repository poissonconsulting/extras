#' Parameter Pattern
#'
#' @inheritParams params
#' @return A string of the regular expression for a parameter name.
#' @export
#'
#' @examples
#' par_pattern()
#' par_pattern(partial = TRUE)
par_pattern <- function(partial = FALSE) {
  if(isTRUE(partial)) return ("[[:alpha:]][[:alnum:]._]*")
  p0("^", par_pattern(TRUE), "$")
}
