# Direction of a distribution

The direction of a distribution is the side (left/right) that the
distribution's center falls on, relative to a threshold. The center can
be calculated using a user-specified function, including the median
(default), mean, geometric mean, mode, or any other custom function. By
convention, values below the threshold fall to the left, while values
above the threshold fall to the right. Center estimates equal to the
threshold are assumed to fall to the right.

## Usage

``` r
direction(x, estimate = xtr_median, threshold = 0, na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values or any other numeric vector of
  samples.

- estimate:

  A function for estimating the center of the distribution. Defaults to
  [`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
  but can also be
  [`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md)
  or any custom function that returns a number (a non-missing numeric
  vector of length 1). If `na_rm` is true, `NA` values are dropped
  before calling the function.

- threshold:

  A number of the threshold value.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A string indicating if at least half of the observations are above the
threshold (`"right"`) or not (`"left"`), or `NA_character_` in the case
of missing values when `na_rm = FALSE`.

## See also

Other summary:
[`directional_information()`](https://poissonconsulting.github.io/extras/dev/reference/directional-information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/dev/reference/probability_direction.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/dev/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/dev/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/dev/reference/skewness.md),
[`svalue()`](https://poissonconsulting.github.io/extras/dev/reference/svalue.md),
[`upper()`](https://poissonconsulting.github.io/extras/dev/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/dev/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/dev/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/dev/reference/zscore.md)

## Examples

``` r
direction(c(1, 2, 3))
#> [1] "right"
direction(c(-1))
#> [1] "left"
direction(c(0, 0, 0))
#> [1] "right"
direction(c(-100, 1, 1))
#> [1] "right"
direction(c(-100, 1, 1), mean)
#> [1] "left"
direction(c(100, 0.01, 0.01), function(.x) exp(mean(log(.x))))
#> [1] "right"
```
