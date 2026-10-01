# Adjust Skew Normal Distribution Parameters for Sensitivity Analyses

Expands (`scale_mult > 1`) or reduces (`scale_mult < 1`) the scale
parameter of the Skew Normal distribution without changing the mean.

## Usage

``` r
sens_skewnorm(location, scale, shape, scale_mult = 2, ..., mean, sd, sd_mult)
```

## Arguments

- location:

  A numeric vector of the location parameter.

- scale:

  A non-negative numeric vector of the scale parameter.

- shape:

  A numeric vector of shape. Negative values result in leftward skew,
  while positive values result in rightward skew.

- scale_mult:

  A non-negative multiplier on the scale of the distribution.

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
[`sens_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewlnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_skewnorm(10, 3, -1, 2)
#> $location
#> [1] 11.69257
#> 
#> $scale
#> [1] 6
#> 
#> $shape
#> [1] -1
#> 
sens_skewnorm(10, 3, 3, 0.8)
#> $location
#> [1] 10.45416
#> 
#> $scale
#> [1] 2.4
#> 
#> $shape
#> [1] 3
#> 
```
