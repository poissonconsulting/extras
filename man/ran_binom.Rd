% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ran.R
\name{ran_binom}
\alias{ran_binom}
\title{Binomial Random Samples}
\usage{
ran_binom(n = 1, size = 1, prob = 0.5)
}
\arguments{
\item{n}{A non-negative whole number of the number of random samples to
generate.}

\item{size}{A non-negative whole numeric vector of the number of trials.}

\item{prob}{A numeric vector of values between 0 and 1 of the probability of
success.}
}
\value{
A numeric vector of the random samples.
}
\description{
Binomial Random Samples
}
\examples{
ran_binom(10)
}
\seealso{
Other ran_dist: 
\code{\link{ran_bern}()},
\code{\link{ran_beta_binom}()},
\code{\link{ran_gamma}()},
\code{\link{ran_gamma_pois}()},
\code{\link{ran_gamma_pois_zi}()},
\code{\link{ran_lnorm}()},
\code{\link{ran_neg_binom}()},
\code{\link{ran_norm}()},
\code{\link{ran_pois}()},
\code{\link{ran_pois_zi}()},
\code{\link{ran_skewnorm}()},
\code{\link{ran_student}()}
}
\concept{ran_dist}
