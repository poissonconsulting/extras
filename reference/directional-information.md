# Directional information

The occurrence of an event (or lack thereof) transmits more or less
information depending on the event's probability.

## Usage

``` r
directional_information(
  x,
  ...,
  side = "median",
  threshold = 0,
  threshold_split = "proportional",
  skeptical = TRUE,
  na_rm = FALSE
)

p2info(p, n = Inf)
```

## Arguments

- x:

  A numeric vector of MCMC values.

- ...:

  Unused.

- side:

  A string indicating whether to calculate the directional information
  relative to the left side (`"left"`; `x < threshold`), or the right
  side (`"right"`; `x > threshold`). Positive information suggests
  greater evidence for the specified side. Defaults to `"median"`, which
  uses the side of the median of `x` via
  [`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md).

- threshold:

  A number of the threshold value.

- threshold_split:

  A string indicating how to deal with threshold values:

  - `"left"` to include them on the left side,

  - `"right"` to include them on the right side,

  - `"equal"` to split them equally between the left and side,

  - `"proportional"` (default) to split them between the left and right
    sides proportionally to the values of `x` on the left and right
    sides,

  - `"exclude"` to drop the values of `x` equal to `threshold`
    (identical to using `"proportional"`).

- skeptical:

  A flag specifying whether or not to add one sample to the empty side
  of the threshold when 100% of samples are on one side. Avoids zero
  p-values and infinite s-values, and also imposes stronger bounds on
  directional information than \[-n, n\], which assume the MCMC samples
  are independent and representative.

- na_rm:

  A flag specifying whether to remove missing values.

- p:

  A numeric vector of probabilities of direction.

- n:

  A numeric vector of the number of posterior samples used to estimate
  each value of `p`. Used to limit the information to be within the
  interval \\\[-n, n\]\\.

## Value

A number indicating the directional information in bits. If `x` has `NA`
values but `na_rm` is `FALSE`, returns `NA_real`.

## Details

Quantifies the information about direction in a posterior distribution
based on the directional probability. This function calculates such
information using the difference in the probability of direction (see
[`probability_direction()`](https://poissonconsulting.github.io/extras/reference/probability_direction.md)),
after converting each probability to bits (also see
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md).

When `skeptical = TRUE` (default), one sample is added to the empty
side, giving bounds of \\\pm \log_2(n)\\ rather than \\\pm n\\, to mimic
the behaviour of
[`pvalue()`](https://poissonconsulting.github.io/extras/reference/pvalue.md)
and
[`svalue()`](https://poissonconsulting.github.io/extras/reference/svalue.md).
When `skeptical = FALSE`, information is instead clamped to \\\[-n,
n\]\\, which is assumes the MCMC samples are independent and
representative.

## Functions

- `directional_information()`: Calculate the directional information
  from a posterior distribution.

- `p2info()`: Calculate the information from a vector of probabilities.

## References

Kery, M., and Schaub, M. 2011. Bayesian population analysis using
WinBUGS: a hierarchical perspective. Academic Press, Boston. Available
from
<https://www.vogelwarte.ch/en/research/population-biology/book-bpa/>.

## See also

Other summary:
[`direction()`](https://poissonconsulting.github.io/extras/reference/direction.md),
[`kurtosis()`](https://poissonconsulting.github.io/extras/reference/kurtosis.md),
[`lower()`](https://poissonconsulting.github.io/extras/reference/lower.md),
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

directional_information(0)
#> [1] 0
directional_information(1) # one coin flip of information
#> [1] 0
directional_information(c(1, 1)) # two coin flips
#> [1] 1
directional_information(c(1, 1, -1)) # x[2] and x[3] cancel out
#> [1] 1
directional_information(c(1, 1, -1, -1)) # both sides cancel out
#> [1] 0
directional_information(rnorm(1e3, mean = 0))
#> [1] 0.0577155
directional_information(rnorm(1e3, mean = 1))
#> [1] 2.328864
directional_information(rnorm(1e3, mean = 10)) # all coin flips are positive
#> [1] 9.965784
directional_information(rnorm(1e3, mean = -10)) # all coin flips are negative
#> [1] 9.965784
directional_information(rnorm(1e3, mean = 1e3)) # only quantiles matter
#> [1] 9.965784
directional_information(rnorm(1e6, mean = 1e3)) # more `x` implies more info
#> [1] 19.93157
directional_information(rep(1, 1000)) # skeptical = TRUE (default) gives log2(n)
#> [1] 9.965784
directional_information(rep(1, 1000), skeptical = FALSE) # skeptical = FALSE gives n
#> [1] 1000

p2info(seq(0, 1, by = 0.1))
#>  [1]       -Inf -3.1699250 -2.0000000 -1.2223924 -0.5849625  0.0000000
#>  [7]  0.5849625  1.2223924  2.0000000  3.1699250        Inf
p2info(seq(0, 1, by = 0.1), n = 10) # limit information to be in [-10, 10]
#>  [1] -10.0000000  -3.1699250  -2.0000000  -1.2223924  -0.5849625   0.0000000
#>  [7]   0.5849625   1.2223924   2.0000000   3.1699250  10.0000000
```
