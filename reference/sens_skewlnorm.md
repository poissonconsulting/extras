# Adjust Skew-Lognormal Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Skew-Lognormal distribution while preserving its mean
and `shape`. The adjustment is made on the natural scale (i.e. for `x`,
not for `log(x)`), mirroring
[`sens_lnorm()`](https://poissonconsulting.github.io/extras/reference/sens_lnorm.md),
to which it reduces when `shape = 0`.

## Usage

``` r
sens_skewlnorm(meanlog, sdlog, shape, sd_mult = 2)
```

## Arguments

- meanlog:

  A numeric vector of the means on the log scale.

- sdlog:

  A non-negative numeric vector of the standard deviations on the log
  scale.

- shape:

  A non-negative numeric vector of shape.

- sd_mult:

  A non-negative multiplier on the standard deviation of the
  distribution.

## Value

A named list of the adjusted distribution's parameters.

## See also

Other sens_dist:
[`sens_beta()`](https://poissonconsulting.github.io/extras/reference/sens_beta.md),
[`sens_exp()`](https://poissonconsulting.github.io/extras/reference/sens_exp.md),
[`sens_gamma()`](https://poissonconsulting.github.io/extras/reference/sens_gamma.md),
[`sens_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/sens_gamma_pois.md),
[`sens_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/sens_gamma_pois_zi.md),
[`sens_lnorm()`](https://poissonconsulting.github.io/extras/reference/sens_lnorm.md),
[`sens_neg_binom()`](https://poissonconsulting.github.io/extras/reference/sens_neg_binom.md),
[`sens_norm()`](https://poissonconsulting.github.io/extras/reference/sens_norm.md),
[`sens_pois()`](https://poissonconsulting.github.io/extras/reference/sens_pois.md),
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/reference/sens_skewnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/reference/sens_student.md)

## Examples

``` r
sens_skewlnorm(0, 1, 2, 2)
#> $meanlog
#> [1] -0.6406637
#> 
#> $sdlog
#> [1] 1.441698
#> 
#> $shape
#> [1] 2
#> 
sens_skewlnorm(0, 1, 2, 0.8)
#> $meanlog
#> [1] 0.1720714
#> 
#> $sdlog
#> [1] 0.8620703
#> 
#> $shape
#> [1] 2
#> 
```
