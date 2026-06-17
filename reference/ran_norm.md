# Normal Random Samples

Normal Random Samples

## Usage

``` r
ran_norm(n = 1, mean = 0, sd = 1)
```

## Arguments

- n:

  A non-negative whole number of the number of random samples to
  generate.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

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
[`ran_pois()`](https://poissonconsulting.github.io/extras/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/reference/ran_pois_zi.md),
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/reference/ran_skewnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/reference/ran_student.md)

## Examples

``` r
ran_norm(10)
#>  [1]  0.15517007 -1.49792119 -0.72809647 -0.49033117  0.55795526 -0.95207248
#>  [7] -0.27749055  0.03120879  1.84763796 -2.36796952
```
