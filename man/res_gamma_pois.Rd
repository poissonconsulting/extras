% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/res.R
\name{res_gamma_pois}
\alias{res_gamma_pois}
\title{Gamma Poisson Residuals}
\usage{
res_gamma_pois(x, lambda = 1, theta = 0, type = "dev", simulate = FALSE)
}
\arguments{
\item{x}{A non-negative whole numeric vector of values.}

\item{lambda}{A non-negative numeric vector of means.}

\item{theta}{A non-negative numeric vector of the shape and rate.}

\item{type}{A string of the residual type ('raw' or 'dev').}

\item{simulate}{A flag specifying whether to simulate residuals.}
}
\value{
An numeric vector of the corresponding residuals.
}
\description{
Gamma Poisson Residuals
}
\examples{
res_gamma_pois(c(0, 1, 2), 1, 1)
}
\seealso{
Other res_dist: 
\code{\link{res_bern}()},
\code{\link{res_binom}()},
\code{\link{res_lnorm}()},
\code{\link{res_neg_binom}()},
\code{\link{res_norm}()},
\code{\link{res_pois}()}
}
\concept{res_dist}
