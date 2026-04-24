# Adjust Gamma Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Gamma distribution.

## Usage

``` r
sens_gamma(shape, rate, sd_mult = 2)
```

## Arguments

- shape:

  A non-negative numeric vector of shape.

- rate:

  A non-negative numeric vector of rate.

- sd_mult:

  A non-negative multiplier on the standard deviation of the
  distribution.

## Value

A named list of the adjusted distribution's parameters.

## See also

Other sens_dist:
[`sens_beta()`](https://poissonconsulting.github.io/extras/dev/reference/sens_beta.md),
[`sens_exp()`](https://poissonconsulting.github.io/extras/dev/reference/sens_exp.md),
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
sens_gamma(10, 2, 2)
#> $shape
#> [1] 2.5
#> 
#> $rate
#> [1] 0.5
#> 
sens_gamma(10, 2, 0.2)
#> $shape
#> [1] 250
#> 
#> $rate
#> [1] 50
#> 
```
