#' Parameter Pattern
#'
#' @inheritParams params
#' @return A string of the regular expression for a parameter name.
#' @export
#'
#' @examples
#' par_pattern()
par_pattern <- function() {
  "[[:alpha:]][[:alnum:]._]*"
}
