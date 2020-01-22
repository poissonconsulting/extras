#' @importFrom universals dims
#' @export
universals::dims

#' Dimensions of an Object
#' 
#' @inherit universals::dims
#' @export
#' @examples 
#' dims(character(0))
#' dims(1:3)
dims.default <- function(x, ...) {
  if (is.vector(x)) length(x) else dim(x)
}

#' Dimensions of a Factor
#' 
#' @inherit universals::dims
#' @export
#' @examples 
#' dims(factor("a"))
dims.factor <- function(x, ...) {
  length(x)
}

#' Dimensions of a Data Frame
#' 
#' @inherit universals::dims
#' @param x A data.frame.
#' @export
#' @examples 
#' dims(data.frame())
#' dims(data.frame(x = 1:3))
dims.data.frame <- function(x, ...) {
  return(c(nrow(x), ncol(x)))
}
