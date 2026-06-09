# Zeros

The number of zeros in an numeric object.

## Usage

``` r
zeros(x, na_rm = FALSE)
```

## Arguments

- x:

  A numeric object of MCMC values.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A non-negative integer.

## See also

Other summary:
[`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/reference/lower.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md),
[`upper()`](https://poissonconsulting.github.io/extras/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/reference/xtr_sd.md),
[`zscore()`](https://poissonconsulting.github.io/extras/reference/zscore.md)

## Examples

``` r
zeros(c(0:2))
#> [1] 1
```
