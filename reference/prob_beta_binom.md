# Beta-Binomial Cumulative Distribution Function

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
prob_beta_binom(x, size = 1, prob = 0.5, theta = 0)
```

## Arguments

- x:

  A numeric vector of quantiles.

- size:

  A non-negative whole numeric vector of the number of trials.

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

## Value

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/reference/prob_bern.md),
[`prob_beta()`](https://poissonconsulting.github.io/extras/reference/prob_beta.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/reference/prob_binom.md),
[`prob_exp()`](https://poissonconsulting.github.io/extras/reference/prob_exp.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois.md),
[`prob_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_gamma_pois_zi.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/reference/prob_neg_binom.md),
[`prob_norm()`](https://poissonconsulting.github.io/extras/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/reference/prob_pois_zi.md),
[`prob_skewlnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewlnorm.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/reference/prob_unif.md)

## Examples

``` r
prob_beta_binom(c(0, 1, 2), 3, 0.5, 0)
#> [1] 0.125 0.500 0.875
```
