#' @importFrom universals dims
#' @export
universals::dims

#' Dimensions of a default object
#' @inheritParams universals::dims
#' @seealso \code{\link[universals]{dims}()}
#' @export
#' @examples 
#' dims(character(0))
#' dims(1:3)
dims.default <- function(x, ...) {
  if (is.vector(x) || is.factor(x)) length(x) else dim(x)
}

#' Dimensions of a data frame
#' @inheritParams universals::dims
#' @seealso \code{\link[universals]{dims}()}
#' @export
#' @examples 
#' dims(data.frame())
#' dims(data.frame(x = 1:3))
dims.data.frame <- function(x, ...) {
  return(c(nrow(x), ncol(x)))
}
