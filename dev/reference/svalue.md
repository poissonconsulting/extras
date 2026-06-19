# Surprisal Value

The surprisal value (Greenland 2019) is the
[pvalue](https://poissonconsulting.github.io/extras/dev/reference/pvalue.md)
expressed in terms of how many consecutive heads would have to be thrown
on a fair coin in a single attempt to achieve the same probability:
\\-\log_2(p)\\, where \\p\\ is the p-value of interest.

## Usage

``` r
svalue(x, side = "both", threshold = 0, na_rm = FALSE)
```

## Arguments

- x:

  A numeric object of MCMC values.

- side:

  A character indicating whether to calculate s-values using p-values
  for the left tail (`"left"`), right tail (`"right"`), or both tails
  (`"both"`; default).

- threshold:

  A number of the threshold value.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A non-negative number. If `x` has `NA` values but `na_rm` is `FALSE`,
returns `NA_real`.

## References

Greenland, S. 2019. Valid P-Values Behave Exactly as They Should: Some
Misleading Criticisms of P-Values and Their Resolution With S -Values.
The American Statistician 73(sup1): 106–114.
[doi:10.1080/00031305.2018.1529625](https://doi.org/10.1080/00031305.2018.1529625)
.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/dev/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/dev/reference/directional_information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/dev/reference/probability_direction.md),
[`pvalue()`](https://poissonconsulting.github.io/extras/dev/reference/pvalue.md),
[`pzeros()`](https://poissonconsulting.github.io/extras/dev/reference/pzeros.md),
[`skewness()`](https://poissonconsulting.github.io/extras/dev/reference/skewness.md),
[`upper()`](https://poissonconsulting.github.io/extras/dev/reference/upper.md),
[`variance()`](https://poissonconsulting.github.io/extras/dev/reference/variance.md),
[`xtr_mean()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_mean.md),
[`xtr_median()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_median.md),
[`xtr_sd()`](https://poissonconsulting.github.io/extras/dev/reference/xtr_sd.md),
[`zeros()`](https://poissonconsulting.github.io/extras/dev/reference/zeros.md),
[`zscore()`](https://poissonconsulting.github.io/extras/dev/reference/zscore.md)

## Examples

``` r
svalue(as.numeric(0:100))
#> [1] 6.658211
svalue(as.numeric(0:100), side = "left")
#> [1] 6.658211
svalue(as.numeric(0:100), side = "right")
#> [1] 0
svalue(rnorm(1e4, mean = 1), side = "left")
#> [1] 2.653809
svalue(rnorm(1e4, mean = 1), side = "right")
#> [1] 0.240418
```
