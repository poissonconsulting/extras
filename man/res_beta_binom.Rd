% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/res.R
\name{res_beta_binom}
\alias{res_beta_binom}
\title{Beta-Binomial Residuals}
\usage{
res_beta_binom(
  x,
  size = 1,
  prob = 0.5,
  theta = 0,
  type = "dev",
  simulate = FALSE
)
}
\arguments{
\item{x}{A non-negative whole numeric vector of values.}

\item{size}{A non-negative whole numeric vector of the number of trials.}

\item{prob}{A numeric vector of values between 0 and 1 of the probability of
success.}

\item{theta}{A non-negative numeric vector of the dispersion for the mixture
models (student, gamma-Poisson and beta-binomial).}

\item{type}{A string of the residual type. 'raw' for raw residuals 'dev' for
deviance residuals and 'data' for the data.}

\item{simulate}{A flag specifying whether to simulate residuals.}
}
\value{
An numeric vector of the corresponding residuals.
}
\description{
This parameterization of the beta-binomial distribution uses an expected
probability parameter, \code{prob}, and a dispersion parameter, \code{theta}. The
parameters of the underlying beta mixture are \code{alpha = (2 * prob) / theta}
and \code{beta = (2 * (1 - prob)) / theta}. This parameterization of \code{theta} is
unconventional, but has useful properties when modelling. When \code{theta = 0},
the beta-binomial reverts to the binomial distribution. When \code{theta = 1} and
\code{prob = 0.5}, the parameters of the beta distribution become \code{alpha = 1} and
\code{beta = 1}, which correspond to a uniform distribution for the beta-binomial
probability parameter.
}
\examples{
res_beta_binom(c(0, 1, 2), 4, 0.5, 0.1)
}
\seealso{
Other res_dist: 
\code{\link{res_bern}()},
\code{\link{res_binom}()},
\code{\link{res_gamma}()},
\code{\link{res_gamma_pois}()},
\code{\link{res_gamma_pois_zi}()},
\code{\link{res_lnorm}()},
\code{\link{res_neg_binom}()},
\code{\link{res_norm}()},
\code{\link{res_pois}()},
\code{\link{res_pois_zi}()},
\code{\link{res_skewnorm}()},
\code{\link{res_student}()}
}
\concept{res_dist}
