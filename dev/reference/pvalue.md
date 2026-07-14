# Bayesian P-Value

A Bayesian p-value (p) is here defined in terms of the quantile-based
(1-p) \* 100% credible interval (CRI) that just includes a threshold
(Kery and Schaub 2011).

## Usage

``` r
pvalue(x, ..., side = "both", threshold = 0, skeptical = TRUE, na_rm = FALSE)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- ...:

  Unused.

- side:

  A character vector of length 1 indicating whether to calculate
  p-values for the left tail (`"left"`), right tail (`"right"`), or
  two-sided (`"both"`; default).

- threshold:

  A number of the threshold value.

- skeptical:

  A flag specifying whether or not to add one sample to the empty side
  of the threshold when 100% of samples are on one side. Avoids zero
  p-values and infinite s-values, and also imposes stronger bounds on
  directional information than \[-n, n\], which assume the MCMC samples
  are independent and representative.

- na_rm:

  A flag specifying whether to remove missing values.

## Value

A number between 0 and 1. If `x` has `NA` values but `na_rm` is `FALSE`,
returns `NA_real`.

## Details

A p-value of 0.05 indicates that the 95% CRI just includes the threshold
value.

Note that the function contains the sample-size correction \\p\_{c} = p
\* n / (n + 1)\\ to avoid p-values of 0. The function can still return
p-values of 1.

When `skeptical = TRUE` (default), a floor of \\1 / (n + 1)\\ is applied
to avoid p-values of 0 when all samples are on one side of the
threshold. When `skeptical = FALSE`, p-values of 0 are allowed.

To use as a measure of certainty in the direction of the estimate (i.e.,
positive or negative), see
[`probability_direction()`](https://poissonconsulting.github.io/extras/dev/reference/probability_direction.md).

For p-values converted to bits, see
[`svalue()`](https://poissonconsulting.github.io/extras/dev/reference/svalue.md).

To convert MCMC objects to information, see
[`directional_information()`](https://poissonconsulting.github.io/extras/dev/reference/directional-information.md).

## References

Kery, M., and Schaub, M. 2011. Bayesian population analysis using
WinBUGS: a hierarchical perspective. Academic Press, Boston. Available
from
<https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/dev/reference/direction.md),
[`directional_information()`](https://poissonconsulting.github.io/extras/dev/reference/directional-information.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/dev/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/dev/reference/lower.md),
[`probability_direction()`](https://poissonconsulting.github.io/extras/dev/reference/probability_direction.md),
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
x <- rnorm(1e6, qnorm(0.05, lower.tail = TRUE))
pvalue(x) # should be 0.05 * 2
#> [1] 0.099858
pvalue(x, side = "left") # should be 0.95
#> [1] 0.950071
pvalue(x, side = "right") # should be 0.05
#> [1] 0.049929
pvalue(rep(1, 10)) # skeptical = TRUE (default) avoids p = 0
#> [1] 0.09090909
pvalue(rep(1, 10), skeptical = FALSE) # skeptical = FALSE allows p = 0
#> [1] 0
```
