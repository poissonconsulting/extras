# Inverse Logistic Transformation

Inverse logistically transforms a numeric atomic object.

## Usage

``` r
invlogit(x)
```

## Arguments

- x:

  A numeric atomic object.

## Value

A numeric atomic object.

## Details

A wrapper on [`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html).

## See also

Other translations:
[`exp10()`](https://poissonconsulting.github.io/extras/reference/exp10.md),
[`exp2()`](https://poissonconsulting.github.io/extras/reference/exp2.md),
[`fabs()`](https://poissonconsulting.github.io/extras/reference/fabs.md),
[`ilog()`](https://poissonconsulting.github.io/extras/reference/ilog.md),
[`ilog10()`](https://poissonconsulting.github.io/extras/reference/ilog10.md),
[`ilog2()`](https://poissonconsulting.github.io/extras/reference/ilog2.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/reference/inv_logit.md),
[`log10<-()`](https://poissonconsulting.github.io/extras/reference/log10-set.md),
[`log2<-()`](https://poissonconsulting.github.io/extras/reference/log2-set.md),
[`log<-()`](https://poissonconsulting.github.io/extras/reference/log-set.md),
[`logit()`](https://poissonconsulting.github.io/extras/reference/logit.md),
[`logit<-()`](https://poissonconsulting.github.io/extras/reference/logit-set.md),
[`phi()`](https://poissonconsulting.github.io/extras/reference/phi.md),
[`pow()`](https://poissonconsulting.github.io/extras/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/reference/step.md)

## Examples

``` r
invlogit(c(-1, 0, 5))
#> [1] 0.2689414 0.5000000 0.9933071
```
