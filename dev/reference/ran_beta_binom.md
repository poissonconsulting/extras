# Beta-Binomial Random Samples

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
ran_beta_binom(n = 1, size = 1, prob = 0.5, theta = 0)
```

## Arguments

- n:

  A non-negative whole number of the number of random samples to
  generate.

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

## Value

A numeric vector of the random samples.

## See also

Other ran_dist:
[`ran_bern()`](https://poissonconsulting.github.io/extras/dev/reference/ran_bern.md),
[`ran_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_binom.md),
[`ran_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma.md),
[`ran_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois.md),
[`ran_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois_zi.md),
[`ran_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_lnorm.md),
[`ran_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_neg_binom.md),
[`ran_norm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_norm.md),
[`ran_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois_zi.md),
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_beta_binom(10, 1, 0.5, 0)
#>  [1] 0 0 0 0 0 0 0 0 1 0
```
