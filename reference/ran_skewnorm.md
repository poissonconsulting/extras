# Skew Normal Random Samples

Skew Normal Random Samples

## Usage

``` r
ran_skewnorm(n = 1, mean = 0, sd = 1, shape = 0)
```

## Arguments

- n:

  A non-negative whole number of the number of random samples to
  generate.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- shape:

  A numeric vector of shape.

## Value

A numeric vector of the random samples.

## See also

Other ran_dist:
[`ran_bern()`](https://poissonconsulting.github.io/extras/reference/ran_bern.md),
[`ran_beta_binom()`](https://poissonconsulting.github.io/extras/reference/ran_beta_binom.md),
[`ran_binom()`](https://poissonconsulting.github.io/extras/reference/ran_binom.md),
[`ran_gamma()`](https://poissonconsulting.github.io/extras/reference/ran_gamma.md),
[`ran_gamma_pois()`](https://poissonconsulting.github.io/extras/reference/ran_gamma_pois.md),
[`ran_gamma_pois_zi()`](https://poissonconsulting.github.io/extras/reference/ran_gamma_pois_zi.md),
[`ran_lnorm()`](https://poissonconsulting.github.io/extras/reference/ran_lnorm.md),
[`ran_neg_binom()`](https://poissonconsulting.github.io/extras/reference/ran_neg_binom.md),
[`ran_norm()`](https://poissonconsulting.github.io/extras/reference/ran_norm.md),
[`ran_pois()`](https://poissonconsulting.github.io/extras/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/reference/ran_pois_zi.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/reference/ran_student.md)

## Examples

``` r
ran_skewnorm(10, shape = -1)
#>  [1]  1.1488493 -1.0576383 -1.7706145 -1.1805689 -0.4948229 -0.4146651
#>  [7] -2.1106997 -0.5657500 -1.0593348 -0.5968130
ran_skewnorm(10, shape = 0)
#>  [1]  0.4314644  0.4896186  0.2866172 -1.1371789 -0.1864812  1.1594164
#>  [7] -0.1794256 -1.0604289  1.6750532  0.7599024
ran_skewnorm(10, shape = 1)
#>  [1]  1.23042949  2.39665789 -0.05533427 -0.14119500 -0.13616026  0.16254137
#>  [7]  0.68906027  1.61610437  1.52232370  0.25063858
```
