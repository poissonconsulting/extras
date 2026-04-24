# Adjust Skew Normal Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Skew Normal distribution without changing the mean.

## Usage

``` r
sens_skewnorm(mean, sd, shape, sd_mult = 2)
```

## Arguments

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- shape:

  A non-negative numeric vector of shape.

- sd_mult:

  A non-negative multiplier on the standard deviation of the
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
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_skewnorm(10, 3, -1, 2)
#> $mean
#> [1] 11.69257
#> 
#> $sd
#> [1] 6
#> 
#> $shape
#> [1] -1
#> 
sens_skewnorm(10, 3, 3, 0.8)
#> $mean
#> [1] 10.45416
#> 
#> $sd
#> [1] 2.4
#> 
#> $shape
#> [1] 3
#> 
```
