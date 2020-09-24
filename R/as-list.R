#' As List
#'
#' Coerces an object to an list.
#' All attributes are removed except any names.
#'
#' @inheritParams params
#' @return A list.
#' @export
as_list_unnamed <- function(x, ...) {
  UseMethod("as_list_unnamed")
}

#' @rdname as_list_unnamed
#' @export
#' @examples
#' as_list_unnamed(1:3)
#' as_list_unnamed(c(x = 1, y = 2))
as_list_unnamed.default <- function(x, ...) {
  x <- as.list(x)
  names <- names(x)
  attributes(x) <- NULL
  if(!is.null(names))
    names(x) <- names
  x
}
