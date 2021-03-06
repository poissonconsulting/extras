% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as-list.R
\name{as_list}
\alias{as_list}
\alias{as_list.default}
\title{As List}
\usage{
as_list(x, ...)

\method{as_list}{default}(x, ...)
}
\arguments{
\item{x}{An object.}

\item{...}{Other arguments passed to methods.}
}
\value{
A list.
}
\description{
Coerces an object to an list.
All attributes are removed except any names.
}
\examples{
as_list(1:3)
as_list(c(x = 1, y = 2))
}
