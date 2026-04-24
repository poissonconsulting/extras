# Absolute

Computes the absolute value of `x`. Used in TMB as replacement for
[`abs()`](https://rdrr.io/r/base/MathFun.html) which is seemingly
ambiguous.

## Usage

``` r
fabs(x)
```

## Arguments

- x:

  An existing R object.

## Value

A numeric vector of the corresponding absolute values.

## Details

A wrapper on [`abs`](https://rdrr.io/r/base/MathFun.html)`()`.

## See also

Other translations:
[`exp10()`](https://poissonconsulting.github.io/extras/dev/reference/exp10.md),
[`exp2()`](https://poissonconsulting.github.io/extras/dev/reference/exp2.md),
[`ilog()`](https://poissonconsulting.github.io/extras/dev/reference/ilog.md),
[`ilog10()`](https://poissonconsulting.github.io/extras/dev/reference/ilog10.md),
[`ilog2()`](https://poissonconsulting.github.io/extras/dev/reference/ilog2.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/dev/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/dev/reference/inv_logit.md),
[`invlogit()`](https://poissonconsulting.github.io/extras/dev/reference/invlogit.md),
`log10<-()`, `log2<-()`, `log<-()`,
[`logit()`](https://poissonconsulting.github.io/extras/dev/reference/logit.md),
`logit<-()`,
[`phi()`](https://poissonconsulting.github.io/extras/dev/reference/phi.md),
[`pow()`](https://poissonconsulting.github.io/extras/dev/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/dev/reference/step.md)

## Examples

``` r
fabs(c(0, -1, 2))
#> [1] 0 1 2
```
