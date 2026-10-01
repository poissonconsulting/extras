# Skew Normal Log-Likelihood

Skew Normal Log-Likelihood

## Usage

``` r
log_lik_skewnorm(
  x,
  location = 0,
  scale = 1,
  shape = 0,
  tlower = -Inf,
  tupper = Inf,
  ...,
  mean,
  sd
)
```

## Arguments

- x:

  A numeric vector of values.

- location:

  A numeric vector of the location parameter.

- scale:

  A non-negative numeric vector of the scale parameter.

- shape:

  A numeric vector of shape. Negative values result in leftward skew,
  while positive values result in rightward skew.

- tlower:

  A numeric vector of the lower truncation point.

- tupper:

  A numeric vector of the upper truncation point.

- ...:

  Unused.

- mean:

  **\[deprecated\]** A numeric vector of the location parameter.
  Described as "a numeric vector of the means" prior to to v. 0.10.1.
  Will be removed in a future version.

- sd:

  **\[deprecated\]** A non-negative numeric vector of the scale
  parameter. Described as "a non-negative numeric vector of the standard
  deviations" prior to v. 0.10.1. Will be removed in a future version.

## Value

An numeric vector of the corresponding log-likelihoods.

## See also

Other log_lik_dist:
[`log_lik_bern()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_bern.md),
[`log_lik_beta()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_beta.md),
[`log_lik_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_beta_binom.md),
[`log_lik_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_binom.md),
[`log_lik_exp()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_exp.md),
[`log_lik_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma.md),
[`log_lik_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois.md),
[`log_lik_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_gamma_pois_zi.md),
[`log_lik_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_lnorm.md),
[`log_lik_multinom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_multinom.md),
[`log_lik_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_neg_binom.md),
[`log_lik_norm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_norm.md),
[`log_lik_pois()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois.md),
[`log_lik_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_pois_zi.md),
[`log_lik_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewlnorm.md),
[`log_lik_student()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_student.md),
[`log_lik_unif()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_unif.md)

## Examples

``` r
log_lik_skewnorm(c(-2:2))
#> [1] -2.9189385 -1.4189385 -0.9189385 -1.4189385 -2.9189385
log_lik_skewnorm(c(-2:2), shape = -2)
#> [1]  -2.2258230  -0.7488043  -0.9189385  -4.5089757 -12.5858928
log_lik_skewnorm(c(-2:2), shape = 2)
#> [1] -12.5858928  -4.5089757  -0.9189385  -0.7488043  -2.2258230
```
