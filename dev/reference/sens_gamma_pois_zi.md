# Adjust Zero-Inflated Gamma-Poisson Distribution Parameters for Sensitivity Analyses

Expands (`sd_mult > 1`) or reduces (`sd_mult < 1`) the standard
deviation of the Zero-Inflated Gamma-Poisson distribution.

## Usage

``` r
sens_gamma_pois_zi(lambda, theta, prob, sd_mult = 2)
```

## Arguments

- lambda:

  A non-negative numeric vector of means.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

- prob:

  A numeric vector of values between 0 and 1 of the probability of
  success.

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
[`sens_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_lnorm.md),
[`sens_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/sens_neg_binom.md),
[`sens_norm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_norm.md),
[`sens_pois()`](https://poissonconsulting.github.io/extras/dev/reference/sens_pois.md),
[`sens_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/sens_skewnorm.md),
[`sens_student()`](https://poissonconsulting.github.io/extras/dev/reference/sens_student.md)

## Examples

``` r
sens_gamma_pois_zi(10, 0.1, 0.3, 2)
#> $lambda
#> [1] 10
#> 
#> $theta
#> [1] 1.6
#> 
#> $prob
#> [1] 0.3
#> 
```
