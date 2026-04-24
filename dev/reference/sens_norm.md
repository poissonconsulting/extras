# Adjust Normal Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Normal distribution without changing the mean.

## Usage

``` r
sens_norm(mean, sd, sd_mult = 2)
```

## Arguments

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

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
[`sens_pois()`](https://poissonconsulting.github.io/extras/dev/reference/sens_pois.md),
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_norm(10, 3, 2)
#> $mean
#> [1] 10
#> 
#> $sd
#> [1] 6
#> 
sens_norm(10, 3, 0.8)
#> $mean
#> [1] 10
#> 
#> $sd
#> [1] 2.4
#> 
```
