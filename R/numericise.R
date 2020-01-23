#' Numericise (or Numericize)
#'
#' Attempts to coerce a non-numeric R object to
#' an numeric atomic object.
#' If possible the dimensionality is preserved.
#'
#' Date, POSIXct and hms objects are floored first.
#'
#' @param x An R object.
#' @param ... Unused
#' @return The modified object.
#' @aliases numericize
#' @export
#' @examples
#' numericize(TRUE)
#' numericize("1.9")
#' numericize(factor(c("beta", "alpha")))
#' numericize(matrix(c(TRUE, FALSE, NA, TRUE), 2))
#' numericize(as.Date("1970-02-03"))
#' numericize(as.POSIXct("1970-02-03", tz = "GMT"))
numericise <- function(x, ...) UseMethod("numericise")

#' @rdname numericise
#' @details `numericize()` is an alias for numericise.
#' @export
numericize <- function(x, ...) UseMethod("numericise")

#' @describeIn numericise Numericise default object
#' @export
numericise.default <- function(x, ...) as.double(x)

#' @describeIn numericise Numericise logical vector
#' @export
numericise.logical <- function(x, ...) as.integer(x)

#' @describeIn numericise Numericise integer vector
#' @export
numericise.integer <- function(x, ...) x

#' @describeIn numericise Numericise double vector
#' @export
numericise.double <- function(x, ...) x

#' @describeIn numericise Numericise factor
#' @export
numericise.factor <- function(x, ...) as.integer(x)

#' @describeIn numericise Numericise Date
#' @export
numericise.Date <- function(x, ...) {
  x <- unclass(x)
  x <- floor(x)
  as.integer(x)
}

#' @describeIn numericise Numericise POSIXct
#' @export
numericise.POSIXct <- function(x, ...) {
  x <- unclass(x)
  x <- floor(x)
  as.integer(x)
}

#' @describeIn numericise Numericise hms
#' @export
numericise.hms <- function(x, ...) {
  x <- unclass(x)
  x <- floor(x)
  as.integer(x)
}

#' @describeIn numericise Numericise matrix
#' @export
numericise.matrix <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dims <- dims(x)
  x <- numericize(as.vector(x))
  dim(x) <- dims
  x
}

#' @describeIn numericise Numericise array
#' @export
numericise.array <- function(x, ...) {
  if (is.logical(x)) {
    mode(x) <- "integer"
    return(x)
  }
  dims <- dims(x)
  x <- numericize(as.vector(x))
  dim(x) <- dims
  x
}

#' @describeIn numericise Numericise list
#' @export
numericise.list <- function(x, ...) lapply(x, numericise)

#' @describeIn numericise Numericise data.frame
#' @export
numericise.data.frame <- function(x, ...) {
  x[] <- lapply(x, numericise)
  x
}
