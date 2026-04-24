# Beta-Binomial Residuals

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
res_beta_binom(
  x,
  size = 1,
  prob = 0.5,
  theta = 0,
  type = "dev",
  simulate = FALSE
)
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

- type:

  A string of the residual type. 'raw' for raw residuals 'dev' for
  deviance residuals and 'data' for the data.

- simulate:

  A flag specifying whether to simulate residuals.

## Value

An numeric vector of the corresponding residuals.

## See also

Other res_dist:
[`res_bern()`](https://poissonconsulting.github.io/extras/dev/reference/res_bern.md),
[`res_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_binom.md),
[`res_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma.md),
[`res_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois.md),
[`res_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_gamma_pois_zi.md),
[`res_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_lnorm.md),
[`res_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/res_neg_binom.md),
[`res_norm()`](https://poissonconsulting.github.io/extras/dev/reference/res_norm.md),
[`res_pois()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois.md),
[`res_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/res_pois_zi.md),
[`res_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/res_skewnorm.md),
[`res_student()`](https://poissonconsulting.github.io/extras/dev/reference/res_student.md)

## Examples

``` r
res_beta_binom(c(0, 1, 2), 4, 0.5, 0.1)
#> [1] -2.2434148 -0.9346019  0.0000000
```
