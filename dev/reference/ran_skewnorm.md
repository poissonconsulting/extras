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
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_skewnorm(10, shape = -1)
#>  [1] -0.70002617 -0.78493479  0.24170628 -0.42787810  0.01671717 -0.21711286
#>  [7] -1.19927257 -0.26506244 -0.27922284  0.01525054
ran_skewnorm(10, shape = 0)
#>  [1]  0.9357843  0.2630667 -0.7882588  0.3636526  0.5176691 -0.9740696
#>  [7]  0.9608648  1.0359308 -1.2753349  2.2117695
ran_skewnorm(10, shape = 1)
#>  [1] -0.07414401  2.26363687  1.80121012  1.92229035  1.55895409  0.51395162
#>  [7]  0.17280030  1.34122708  1.07141313  1.31051920
```
