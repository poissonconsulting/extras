% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odds.R
\name{inv_odds}
\alias{inv_odds}
\title{Inverse Odds}
\usage{
inv_odds(x)
}
\arguments{
\item{x}{A numeric object (vector, matrix or array) of odds.}
}
\value{
A numeric object of the the probabilities for each odd.
}
\description{
Calculates the probabilities for odds.
}
\examples{
inv_odds(c(0, 1, 9, 9999))
}
\seealso{
Other odds: 
\code{\link{log_odds}()},
\code{\link{log_odds<-}()},
\code{\link{log_odds_ratio}()},
\code{\link{odds}()},
\code{\link{odds<-}()},
\code{\link{odds_ratio}()}
}
\concept{odds}
