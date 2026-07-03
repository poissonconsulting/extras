# Skew-Lognormal Random Samples

Skew-Lognormal Random Samples

## Usage

``` r
ran_skewlnorm(n = 1, meanlog = 0, sdlog = 1, shape = 0)
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
[`ran_skewnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_skewlnorm(10, shape = -1)
#>  [1] 3.1545608 0.3472750 0.1702284 0.3071040 0.6096789 0.6605615 0.1211532
#>  [8] 0.5679340 0.3466864 0.5505635
ran_skewlnorm(10, shape = 0)
#>  [1] 1.5395104 1.6316938 1.3319142 0.3207225 0.8298741 3.1880723 0.8357501
#>  [8] 0.3463072 5.3390789 2.1380676
ran_skewlnorm(10, shape = 1)
#>  [1]  3.4226992 10.9863972  0.9461688  0.8683200  0.8727028  1.1764970
#>  [7]  1.9918429  5.0334436  4.5828620  1.2848456
```
