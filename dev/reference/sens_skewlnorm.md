# Adjust Skew-Lognormal Distribution Parameters for Sensitivity Analyses

Expands (`scale_mult > 1`) or reduces (`scale_mult < 1`) the scale
parameter of the Skew-Lognormal distribution while preserving its mean
and `shapelog`. The adjustment is made on the natural scale (i.e. for
`x`, not for `log(x)`), mirroring
[`sens_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_lnorm.md),
to which it reduces when `shapelog = 0`.

## Usage

``` r
sens_skewlnorm(
  locationlog,
  scalelog,
  shapelog,
  scale_mult = 2,
  ...,
  meanlog,
  sdlog,
  shape,
  sd_mult
)
```

## Arguments

- locationlog:

  A numeric vector of location parameters of `log(x)`.

- scalelog:

  A non-negative numeric vector of scale parameters of `log(x)`.

- shapelog:

  A numeric vector of shape parameters of `log(x)`. Negative values
  result in leftward skew, while positive values result in rightward
  skew.

- scale_mult:

  A non-negative multiplier on the scale of the distribution.

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

- sd_mult:

  **\[deprecated\]** A non-negative multiplier on the scale of the
  distribution.

## Value

A named list of the adjusted distribution's parameters.

## See also

Other sens_dist:
[`sens_beta()`](https://poissonconsulting.github.io/extras/dev/reference/sens_beta.md),
[`sens_exp()`](https://poissonconsulting.github.io/extras/dev/reference/sens_exp.md),
[`sens_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/sens_gamma.md),
[`sens_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/sens_gamma_pois.md),
[`sens_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/sens_gamma_pois_zi.md),
[`sens_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_lnorm.md),
[`sens_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/sens_neg_binom.md),
[`sens_norm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_norm.md),
[`sens_pois()`](https://poissonconsulting.github.io/extras/dev/reference/sens_pois.md),
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_skewlnorm(0, 1, 2, 2)
#> $locationlog
#> [1] -0.6406637
#> 
#> $scalelog
#> [1] 1.441698
#> 
#> $shapelog
#> [1] 2
#> 
sens_skewlnorm(0, 1, 2, 0.8)
#> $locationlog
#> [1] 0.1720714
#> 
#> $scalelog
#> [1] 0.8620703
#> 
#> $shapelog
#> [1] 2
#> 
```
