# Skew-Lognormal Log-Likelihood

Skew-Lognormal Log-Likelihood

## Usage

``` r
log_lik_skewlnorm(
  x,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  tlower = 0,
  tupper = Inf,
  ...,
  meanlog,
  sdlog,
  shape
)
```

## Arguments

- x:

  A numeric vector of values.

- locationlog:

  A numeric vector of location parameters of `log(x)`.

- scalelog:

  A non-negative numeric vector of scale parameters of `log(x)`.

- shapelog:

  A numeric vector of shape parameters of `log(x)`. Negative values
  result in leftward skew, while positive values result in rightward
  skew.

- tlower:

  A numeric vector of the lower truncation point.

- tupper:

  A numeric vector of the upper truncation point.

- ...:

  Unused.

- meanlog:

  **\[deprecated\]** A numeric vector of location parameters of
  `log(x)`. Described as "a numeric vector of the means on the log
  scale" prior to v. 0.10.1. Will be removed in a future version.

- sdlog:

  **\[deprecated\]** A non-negative numeric vector of scale parameters
  of `log(x)`. Described as "a non-negative numeric vector of the
  standard deviations on the log scale" prior to v. 0.10.1. Will be
  removed in a future version.

- shape:

  **\[deprecated\]** A numeric vector of shape parameters of `log(x)`.
  Will be removed in a future version.

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
[`log_lik_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_skewnorm.md),
[`log_lik_student()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_student.md),
[`log_lik_unif()`](https://poissonconsulting.github.io/extras/dev/reference/log_lik_unif.md)

## Examples

``` r
log_lik_skewlnorm(1:5)
#> [1] -0.9189385 -1.8523122 -2.6210253 -3.2661389 -3.8235216
log_lik_skewlnorm(1:5, shapelog = -2)
#> [1]  -0.9189385  -3.6501479  -6.1964185  -8.4580739 -10.4790080
log_lik_skewlnorm(1:5, shapelog = 2)
#> [1] -0.9189385 -1.2456259 -1.9419793 -2.5757762 -3.1310181
```
