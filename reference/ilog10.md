# Inverse Log Base 10 Transformation

Inverse log transforms a numeric atomic object with base 10.

## Usage

``` r
ilog10(x)
```

## Arguments

- x:

  An object.

## Value

A numeric atomic object.

## Details

A wrapper on
[`exp10`](https://poissonconsulting.github.io/extras/reference/exp10.md)`(value)`.

## See also

Other translations:
[`exp10()`](https://poissonconsulting.github.io/extras/reference/exp10.md),
[`exp2()`](https://poissonconsulting.github.io/extras/reference/exp2.md),
[`fabs()`](https://poissonconsulting.github.io/extras/reference/fabs.md),
[`ilog()`](https://poissonconsulting.github.io/extras/reference/ilog.md),
[`ilog2()`](https://poissonconsulting.github.io/extras/reference/ilog2.md),
[`ilogit()`](https://poissonconsulting.github.io/extras/reference/ilogit.md),
[`inv_logit()`](https://poissonconsulting.github.io/extras/reference/inv_logit.md),
[`invlogit()`](https://poissonconsulting.github.io/extras/reference/invlogit.md),
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
x <- c(2, 4.5)
ilog10(x)
#> [1]   100.00 31622.78
```
