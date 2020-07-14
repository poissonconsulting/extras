#' Parameter Pattern
#'
#' @inheritParams params
#' @return A string of the regular expression for a parameter name.
#' @export
#'
#' @examples
#' par_pattern()
#' par_pattern(ht = TRUE)
#' par_pattern(ds = TRUE)
par_pattern <- function(ht = FALSE, ds = FALSE) {
  x <- "[[:alpha:]][[:alnum:]._]*"
  if(isTRUE(ht)) x <- paste0("^", x)
  if(isTRUE(ds)) x <- paste0(x, "$")
  x
}
