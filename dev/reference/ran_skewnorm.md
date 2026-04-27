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
#>  [1] -1.2841816 -0.9001683  0.7834634 -0.6299220  0.1543285 -0.4338478
#>  [7]  0.6111167  0.2770969 -1.0913339 -0.3690964
ran_skewnorm(10, shape = 0)
#>  [1]  1.83264870 -1.40759814  1.12237467 -0.93919547 -0.03242176  0.66373912
#>  [7]  0.24126000 -0.06058839 -0.28093385 -0.15060721
ran_skewnorm(10, shape = 1)
#>  [1]  0.68000584  1.37759365  0.65267147 -1.73287211 -0.14573193  1.05994606
#>  [7]  0.56786482  2.16934296  0.88317858 -0.04613098
```
