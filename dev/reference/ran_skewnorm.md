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
[`ran_skewlnorm()`](https://poissonconsulting.github.io/extras/dev/reference/ran_skewlnorm.md),
[`ran_student()`](https://poissonconsulting.github.io/extras/dev/reference/ran_student.md)

## Examples

``` r
ran_skewnorm(10, shape = -1)
#>  [1] -1.4598895 -1.7300920 -0.3785464 -0.6900374 -0.9216302 -1.4594648
#>  [7] -0.9637518 -0.9933685 -0.7869584 -1.3534472
ran_skewnorm(10, shape = 0)
#>  [1] -0.3432754 -0.4327656  0.1801635  1.7244790 -0.6515033 -0.1581936
#>  [7] -1.4571486 -0.9542045  1.1945719 -0.9032592
ran_skewnorm(10, shape = 1)
#>  [1]  1.05591563 -0.57251003  0.07059691  0.13657298  0.97029838  1.84298062
#>  [7] -0.38607216  1.73498890  0.92340047  0.71429037
```
