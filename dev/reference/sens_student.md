# Adjust Student's t Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Student's t distribution. Because the variance of this
distribution is not defined for every degree of freedom, the adjustment
to the standard deviation is approximate, and the mean of the adjusted
distribution can be expected to have shifted.

## Usage

``` r
sens_student(mean, sd, theta, sd_mult = 2)
```

## Arguments

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

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
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewnorm.md)

## Examples

``` r
sens_student(10, 3, 0.1, 2)
#> $mean
#> [1] 10
#> 
#> $sd
#> [1] 6
#> 
#> $theta
#> [1] 0.1
#> 
sens_student(10, 3, 0.1, 0.8)
#> $mean
#> [1] 10
#> 
#> $sd
#> [1] 2.4
#> 
#> $theta
#> [1] 0.1
#> 
```
