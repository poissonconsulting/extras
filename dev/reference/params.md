# Parameter Descriptions

Default parameter descriptions which may be overridden in individual
functions.

## Arguments

- ...:

  Other arguments passed to methods.

- alpha:

  The first shape parameter of the beta distribution.

- beta:

  The second shape parameter of the beta distribution.

- conf_level:

  A numeric scalar between 0 and 1 specifying the confidence level.

- directional:

  A flag specifying whether probabilities less than 0.5 should be
  returned as negative values.

- lambda:

  A non-negative numeric vector of means.

- log:

  A flag specifying whether to return the log-transformed value.

- lower.tail:

  A flag specifying whether to return the lower or upper tail of the
  distribution.

- min:

  A numeric vector of the minimums.

- mean:

  A numeric vector of the means.

- meanlog:

  A numeric vector of the means on the log scale.

- max:

  A numeric vector of the maximums.

- n:

  A non-negative whole number of the number of random samples to
  generate.

- na_rm:

  A flag specifying whether to remove missing values.

- nas:

  A flag specifying whether to also fill missing values.

- p:

  A vector of probabilities.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- q:

  A vector of quantiles.

- rate:

  A non-negative numeric vector of rate.

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

- scale:

  A non-negative numeric vector of the scale.

- sd:

  A non-negative numeric vector of the standard deviations.

- sd_mult:

  A non-negative multiplier on the standard deviation of the
  distribution.

- sdlog:

  A non-negative numeric vector of the standard deviations on the log
  scale.

- shape:

  A non-negative numeric vector of shape.

- simulate:

  A flag specifying whether to simulate residuals.

- size:

  A non-negative whole numeric vector of the number of trials.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- threshold:

  A number of the threshold value.

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- value:

  A scalar of the value to replace values with.

- x:

  An object.

## Details

A flag is a non-missing logical scalar.

A string is a non-missing character scalar.
