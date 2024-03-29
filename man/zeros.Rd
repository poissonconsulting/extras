% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zeros.R
\name{zeros}
\alias{zeros}
\title{Zeros}
\usage{
zeros(x, na_rm = FALSE)
}
\arguments{
\item{x}{A numeric object of MCMC values.}

\item{na_rm}{A flag specifying whether to remove missing values.}
}
\value{
A non-negative integer.
}
\description{
The number of zeros in an numeric object.
}
\examples{
zeros(c(0:2))
}
\seealso{
Other summary: 
\code{\link{kurtosis}()},
\code{\link{lower}()},
\code{\link{pvalue}()},
\code{\link{pzeros}()},
\code{\link{skewness}()},
\code{\link{svalue}()},
\code{\link{upper}()},
\code{\link{variance}()},
\code{\link{xtr_mean}()},
\code{\link{xtr_median}()},
\code{\link{xtr_sd}()},
\code{\link{zscore}()}
}
\concept{summary}
