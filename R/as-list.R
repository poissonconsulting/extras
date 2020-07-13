#' As List
#'
#' Coerces an object to an list.
#' All attributes are removed except any names.
#'
#' @inheritParams params
#' @return A list.
#' @export
as_list <- function(x, ...) {
  UseMethod("as_list")
}

#' @rdname as_list
#' @export
#' @examples
#' as_list(1:3)
#' as_list(c(x = 1, y = 2))
as_list.default <- function(x, ...) {
  x <- as.list(x)
  names <- names(x)
  attributes(x) <- NULL
  if(!is.null(names))
    names(x) <- names
  x
}
