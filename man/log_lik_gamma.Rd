% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log-lik.R
\name{log_lik_gamma}
\alias{log_lik_gamma}
\title{Gamma Log-Likelihood}
\usage{
log_lik_gamma(x, shape = 1, rate = 1)
}
\arguments{
\item{x}{A numeric vector of values.}

\item{shape}{A non-negative numeric vector of shape.}

\item{rate}{A non-negative numeric vector of rate.}
}
\value{
An numeric vector of the corresponding log-likelihoods.
}
\description{
Gamma Log-Likelihood
}
\examples{
log_lik_gamma(c(0, 1, 2), 1, 2)
}
\seealso{
Other log_lik_dist: 
\code{\link{log_lik_bern}()},
\code{\link{log_lik_beta_binom}()},
\code{\link{log_lik_binom}()},
\code{\link{log_lik_gamma_pois}()},
\code{\link{log_lik_gamma_pois_zi}()},
\code{\link{log_lik_lnorm}()},
\code{\link{log_lik_neg_binom}()},
\code{\link{log_lik_norm}()},
\code{\link{log_lik_pois}()},
\code{\link{log_lik_pois_zi}()},
\code{\link{log_lik_skewnorm}()},
\code{\link{log_lik_student}()}
}
\concept{log_lik_dist}
