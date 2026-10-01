# Skew-Normal Distribution

Skew-Normal Distribution

## Usage

``` r
dskewnorm(x, location = 0, scale = 1, shape = 0, log = FALSE, ..., mean, sd)

pskewnorm(q, location = 0, scale = 1, shape = 0, ..., mean, sd)

qskewnorm(p, location = 0, scale = 1, shape = 0, ..., mean, sd)

rskewnorm(n = 1, location = 0, scale = 1, shape = 0, ..., mean, sd)
```

## Arguments

- x:

  A numeric vector of values.

- location:

  A numeric vector of the location parameter.

- scale:

  A non-negative numeric vector of the scale parameter.

- shape:

  A numeric vector of shape. Negative values result in leftward skew,
  while positive values result in rightward skew.

- log:

  A flag specifying whether to return the log-transformed value.

- ...:

  Unused.

- mean:

  **\[deprecated\]** A numeric vector of the location parameter.
  Described as "a numeric vector of the means" prior to to v. 0.10.1.
  Will be removed in a future version.

- sd:

  **\[deprecated\]** A non-negative numeric vector of the scale
  parameter. Described as "a non-negative numeric vector of the standard
  deviations" prior to v. 0.10.1. Will be removed in a future version.

- q:

  A vector of quantiles.

- p:

  A numeric vector of probabilities.

- n:

  A non-negative whole number of the number of random samples to
  generate.

## Value

`dskewnorm` gives the density, `pskewnorm` gives the distribution
function, `qskewnorm` gives the quantile function, and `rskewnorm`
generates random deviates. `pskewnorm` and `qskewnorm` use the lower
tail probability.

## Examples

``` r
dskewnorm(x = -2:2, location = 0, scale = 1, shape = 0.1)
#> [1] 0.04543235 0.22269638 0.39894228 0.26124507 0.06254958
dskewnorm(x = -2:2, location = 0, scale = 1, shape = -1)
#> [1] 0.105525330 0.407161596 0.398942280 0.076779853 0.002456603
qskewnorm(p = c(0.1, 0.4), location = 0, scale = 1, shape = 0.1)
#> [1] -1.1980898 -0.1731883
qskewnorm(p = c(0.1, 0.4), location = 0, scale = 1, shape = -1)
#> [1] -1.6322188 -0.7540709
pskewnorm(q = -2:2, location = 0, scale = 1, shape = 0.1)
#> [1] 0.01848493 0.13944469 0.46827448 0.82213418 0.97298466
pskewnorm(q = -2:2, location = 0, scale = 1, shape = -1)
#> [1] 0.0449827 0.2921390 0.7500000 0.9748285 0.9994824
rskewnorm(n = 3, location = 0, scale = 1, shape = 0.1)
#> [1]  0.06119718 -0.91936053  0.55349209
rskewnorm(n = 3, location = 0, scale = 1, shape = -1)
#> [1] -1.494791 -1.172111 -1.274561
```
