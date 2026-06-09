# Zero-Inflated Gamma-Poisson Deviances

Zero-Inflated Gamma-Poisson Deviances

## Usage

``` r
dev_gamma_pois_zi(x, lambda = 1, theta = 0, prob = 0, res = FALSE)
```

## Arguments

- x:

  A non-negative whole numeric vector of values.

- lambda:

  A non-negative numeric vector of means.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

## Value

An numeric vector of the corresponding deviances or deviance residuals.

## Examples

``` r
dev_gamma_pois_zi(c(1, 3, 4), 3, 2)
#> [1] 0.34466900 0.00000000 0.03962673
```
