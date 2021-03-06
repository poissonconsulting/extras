% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/res.R
\name{res_pois}
\alias{res_pois}
\title{Poisson Residuals}
\usage{
res_pois(x, lambda = 1, type = "dev", simulate = FALSE)
}
\arguments{
\item{x}{A non-negative whole numeric vector of values.}

\item{lambda}{A non-negative numeric vector of means.}

\item{type}{A string of the residual type ('raw' or 'dev').}

\item{simulate}{A flag specifying whether to simulate residuals.}
}
\value{
An numeric vector of the corresponding residuals.
}
\description{
Poisson Residuals
}
\examples{
res_pois(c(1,3.5,4), 3)
}
\seealso{
Other res_dist: 
\code{\link{res_bern}()},
\code{\link{res_binom}()},
\code{\link{res_gamma_pois}()},
\code{\link{res_lnorm}()},
\code{\link{res_neg_binom}()},
\code{\link{res_norm}()}
}
\concept{res_dist}
