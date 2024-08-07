% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log.R
\name{log2<-}
\alias{log2<-}
\title{Log Base 2 Transformation}
\usage{
log2(x) <- value
}
\arguments{
\item{x}{An object.}

\item{value}{A numeric atomic object.}
}
\value{
Called for the side effect of updating \code{x}.
}
\description{
Replaces a object with the base 2 exponent of value.
}
\details{
A wrapper on \code{\link{exp2}}\code{(value)}.
}
\examples{
x <- NULL
log2(x) <- c(0.5, 5)
x
}
\seealso{
Other translations: 
\code{\link{exp10}()},
\code{\link{exp2}()},
\code{\link{fabs}()},
\code{\link{ilog}()},
\code{\link{ilog10}()},
\code{\link{ilog2}()},
\code{\link{ilogit}()},
\code{\link{inv_logit}()},
\code{\link{invlogit}()},
\code{\link{log10<-}()},
\code{\link{log<-}()},
\code{\link{logit}()},
\code{\link{logit<-}()},
\code{\link{phi}()},
\code{\link{pow}()},
\code{\link{step}()}
}
\concept{translations}
