# Student's t Random Samples

Student's t Random Samples

## Usage

``` r
ran_student(n = 1, mean = 0, sd = 1, theta = 0)
```

## Arguments

- n:

  A non-negative whole number of the number of random samples to
  generate.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- theta:

  A non-negative numeric vector of the dispersion for the mixture models
  (student, gamma-Poisson and beta-binomial).

## Value

A numeric vector of the random samples.

## See also

Other ran_dist:
[`ran_bern()`](https://poissonconsulting.github.io/extras/dev/reference/ran_bern.md),
[`ran_beta_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_beta_binom.md),
[`ran_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_binom.md),
[`ran_gamma()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma.md),
[`ran_gamma_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois.md),
[`ran_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_gamma_pois_zi.md),
[`ran_lnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_lnorm.md),
[`ran_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_neg_binom.md),
[`ran_norm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_norm.md),
[`ran_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois_zi.md),
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewnorm.md)

## Examples

``` r
ran_student(10, theta = 1 / 2)
#>  [1] -0.3094994 -0.1240871 -0.2873568 -0.4468556  1.7071837 -0.3961069
#>  [7]  0.6339260  0.8147481 -2.0029255 -0.7542160
```
