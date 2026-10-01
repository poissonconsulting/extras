# Skew-Lognormal Cumulative Distribution Function

Skew-Lognormal Cumulative Distribution Function

## Usage

``` r
prob_skewlnorm(
  x,
  locationlog = 0,
  scalelog = 1,
  shapelog = 0,
  ...,
  meanlog,
  sdlog,
  shape
)
```

## Arguments

- x:

  A numeric vector of quantiles.

- locationlog:

  A numeric vector of location parameters of `log(x)`.

- scalelog:

  A non-negative numeric vector of scale parameters of `log(x)`.

- shapelog:

  A numeric vector of shape parameters of `log(x)`. Negative values
  result in leftward skew, while positive values result in rightward
  skew.

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

An numeric vector of the corresponding probabilities.

## See also

Other prob_dist:
[`prob_bern()`](https://poissonconsulting.github.io/extras/dev/reference/prob_bern.md),
[`prob_beta()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta.md),
[`prob_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_beta_binom.md),
[`prob_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_binom.md),
[`prob_exp()`](https://poissonconsulting.github.io/extras/dev/reference/prob_exp.md),
[`prob_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma.md),
[`prob_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma_pois.md),
[`prob_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/prob_gamma_pois_zi.md),
[`prob_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_lnorm.md),
[`prob_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/prob_neg_binom.md),
[`prob_norm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_norm.md),
[`prob_pois()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois.md),
[`prob_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/prob_pois_zi.md),
[`prob_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/prob_skewnorm.md),
[`prob_student()`](https://poissonconsulting.github.io/extras/dev/reference/prob_student.md),
[`prob_unif()`](https://poissonconsulting.github.io/extras/dev/reference/prob_unif.md)

## Examples

``` r
prob_skewlnorm(1:5)
#> [1] 0.5000000 0.7558914 0.8640314 0.9171715 0.9462397
prob_skewlnorm(1:5, shapelog = -2)
#> [1] 0.8524164 0.9899035 0.9991047 0.9998950 0.9999845
prob_skewlnorm(1:5, shapelog = 2)
#> [1] 0.1475836 0.5218793 0.7289581 0.8344480 0.8924949
```
