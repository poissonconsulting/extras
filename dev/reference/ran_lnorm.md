# Log-Normal Random Samples

Log-Normal Random Samples

## Usage

``` r
ran_lnorm(n = 1, meanlog = 0, sdlog = 1)
```

## Arguments

- n:

  A non-negative whole number of the number of random samples to
  generate.

- meanlog:

  A numeric vector of the means on the log scale.

- sdlog:

  A non-negative numeric vector of the standard deviations on the log
  scale.

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
[`ran_neg_binom()`](https://poissonconsulting.github.io/extras/dev/reference/ran_neg_binom.md),
[`ran_norm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_norm.md),
[`ran_pois()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois.md),
[`ran_pois_zi()`](https://poissonconsulting.github.io/extras/dev/reference/ran_pois_zi.md),
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_lnorm(10)
#>  [1] 1.6300795 0.1827839 0.2297563 1.3286327 3.8088237 1.2670562 3.7370382
#>  [8] 1.6886169 1.8344561 0.8958918
```
