# Adjust Beta Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Beta distribution. The Beta distribution has a maximum
variance of `mean(x) * (1 - mean(x)`, where
`mean(x) = alpha / (alpha + beta)`. If the inputs produce a desired
variance that is greater than the maximum possible variance, or provides
alpha and/or beta parameters that are `< 1` and thus push more
probability weight towards extreme probability values, this function
returns `alpha = 1` and `beta = 1` (the uniform distribution).

## Usage

``` r
sens_beta(alpha, beta, sd_mult = 2)
```

## Arguments

- alpha:

  The first shape parameter of the beta distribution.

- beta:

  The second shape parameter of the beta distribution.

- sd_mult:

  A non-negative multiplier on the standard deviation of the
  distribution.

## Value

A named list of the adjusted distribution's parameters.

## See also

Other sens_dist:
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
sens_beta(10, 10, 2)
#> $alpha
#> [1] 2.125
#> 
#> $beta
#> [1] 2.125
#> 
sens_beta(10, 10, 0.8)
#> $alpha
#> [1] 15.90625
#> 
#> $beta
#> [1] 15.90625
#> 
```
