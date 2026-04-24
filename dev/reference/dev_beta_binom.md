# Beta-Binomial Deviances

This parameterization of the beta-binomial distribution uses an expected
probability parameter, `prob`, and a dispersion parameter, `theta`. The
parameters of the underlying beta mixture are
`alpha = (2 * prob) / theta` and `beta = (2 * (1 - prob)) / theta`. This
parameterization of `theta` is unconventional, but has useful properties
when modelling. When `theta = 0`, the beta-binomial reverts to the
binomial distribution. When `theta = 1` and `prob = 0.5`, the parameters
of the beta distribution become `alpha = 1` and `beta = 1`, which
correspond to a uniform distribution for the beta-binomial probability
parameter.

## Usage

``` r
dev_beta_binom(x, size = 1, prob = 0.5, theta = 0, res = FALSE)
```

## Arguments

- x:

  A non-negative whole numeric vector of values.

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- res:

  A flag specifying whether to return the deviance residual as opposed
  to the deviance.

## Value

An numeric vector of the corresponding deviances or deviance residuals.

## See also

Other dev_dist:
[`dev_bern()`](https://poissonconsulting.github.io/extras/dev/reference/dev_bern.md),
[`dev_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_binom.md),
[`dev_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma.md),
[`dev_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_gamma_pois.md),
[`dev_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_lnorm.md),
[`dev_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/dev_neg_binom.md),
[`dev_norm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_norm.md),
[`dev_pois()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois.md),
[`dev_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/dev_pois_zi.md),
[`dev_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/dev_skewnorm.md),
[`dev_student()`](https://poissonconsulting.github.io/extras/dev/reference/dev_student.md)

## Examples

``` r
dev_beta_binom(c(0, 1, 2), 10, 0.5, 0.1)
#> [1] 10.758196  5.136943  2.608386
```
