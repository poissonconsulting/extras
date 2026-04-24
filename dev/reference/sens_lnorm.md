# Adjust Log-Normal Distribution Parameters for Sensitivity Analysis

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Log-Normal distribution. With high values of `sdlog`
(i.e., `> 9`), and `sd_mult > 1`, the mean of the adjusted distribution
can be expected to have a mean value that is very different from the
original mean, however, the proportional difference in these values
should not be very different.

## Usage

``` r
sens_lnorm(meanlog, sdlog, sd_mult = 2)
```

## Arguments

- meanlog:

  A numeric vector of the means on the log scale.

- sdlog:

  A non-negative numeric vector of the standard deviations on the log
  scale.

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
[`sens_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/sens_neg_binom.md),
[`sens_norm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_norm.md),
[`sens_pois()`](https://poissonconsulting.github.io/extras/dev/reference/sens_pois.md),
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_lnorm(0, 1, 2)
#> $meanlog
#> [1] -0.5317277
#> 
#> $sdlog
#> [1] 1.436473
#> 
sens_lnorm(0, 1, 0.8)
#> $meanlog
#> [1] 0.1291027
#> 
#> $sdlog
#> [1] 0.861275
#> 
```
