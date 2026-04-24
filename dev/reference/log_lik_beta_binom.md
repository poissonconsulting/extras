# Beta-Binomial Log-Likelihood

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
log_lik_beta_binom(x, size = 1, prob = 0.5, theta = 0, memoize = FALSE)
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

- memoize:

  Whether or not to memoize the function.

## Value

An numeric vector of the corresponding log-likelihoods.

## See also

Other log_lik_dist:
[`log_lik_bern()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_bern.md),
[`log_lik_beta()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_beta.md),
[`log_lik_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_binom.md),
[`log_lik_exp()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_exp.md),
[`log_lik_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma.md),
[`log_lik_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois.md),
[`log_lik_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois_zi.md),
[`log_lik_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_lnorm.md),
[`log_lik_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_neg_binom.md),
[`log_lik_norm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_norm.md),
[`log_lik_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois.md),
[`log_lik_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois_zi.md),
[`log_lik_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewnorm.md),
[`log_lik_student()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_student.md),
[`log_lik_unif()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_unif.md)

## Examples

``` r
log_lik_beta_binom(c(0, 1, 2), 3, 0.5, 0)
#> [1] -2.0794415 -0.9808293 -0.9808293
```
