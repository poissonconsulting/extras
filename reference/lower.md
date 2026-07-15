# Lower Credible Limit

Calculates the quantile-based lower credible limit.

## Usage

``` r
lower(x, conf_level = 0.95, na_rm = FALSE)
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
2.5% quantile.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/reference/directional-information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/reference/probability_direction.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md),
[`upper()`](https://poissonconsulting.github.io/extras/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/reference/zscore.md)

## Examples

``` r
lower(as.numeric(0:100))
#> [1] 2.5
```
