% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportional.R
\name{proportional_change}
\alias{proportional_change}
\title{Proportional Change}
\usage{
proportional_change(x, x2)
}
\arguments{
\item{x}{A numeric object (vector, matrix or array) of non-negative numbers.}

\item{x2}{A second numeric object of non-negative numbers.}
}
\value{
A numeric object of the proportional change.
}
\description{
Calculates the proportional change for two sets of numbers.
}
\examples{
proportional_change(1, 2)
proportional_change(2, 1)
}
\seealso{
Other proportional: 
\code{\link{proportional_difference}()}
}
\concept{proportional}
