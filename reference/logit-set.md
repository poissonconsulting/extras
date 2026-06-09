# Logistic Transformation

Logistic Transformation

## Usage

``` r
logit(x) <- value
```

## Arguments

- x:

  An existing object.

- value:

  A numeric atomic object of the value to inverse logistically
  transform.

## Value

Called for the side effect of updating `x`.

## Details

A wrapper on `stats::plogis(value)`.

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
[`invlogit()`](https://poissonconsulting.github.io/extras/reference/invlogit.md),
[`log10<-()`](https://poissonconsulting.github.io/extras/reference/log10-set.md),
[`log2<-()`](https://poissonconsulting.github.io/extras/reference/log2-set.md),
[`log<-()`](https://poissonconsulting.github.io/extras/reference/log-set.md),
[`logit()`](https://poissonconsulting.github.io/extras/reference/logit.md),
[`phi()`](https://poissonconsulting.github.io/extras/reference/phi.md),
[`pow()`](https://poissonconsulting.github.io/extras/reference/pow.md),
[`step()`](https://poissonconsulting.github.io/extras/reference/step.md)

## Examples

``` r
x <- 1
logit(x) <- 0.5
x
#> [1] 0.6224593
```
