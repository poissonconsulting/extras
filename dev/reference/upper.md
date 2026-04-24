# Upper Credible Limit

Calculates the quantile-based upper credible limit.

## Usage

``` r
upper(x, conf_level = 0.95, na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- conf_level:

  A numeric scalar between 0 and 1 specifying the confidence level.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A number.

## Details

By default it returns the 95% credible limit which corresponds to the
97.5% quantile.

## See also

Other summary:
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/dev/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/dev/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/dev/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/dev/reference/svalue.md),
[`variance()`](https://poissonconsulting.github.io/extras/dev/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/dev/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/dev/reference/zscore.md)

## Examples

``` r
upper(as.numeric(0:100))
#> [1] 97.5
```
