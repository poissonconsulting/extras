# Skew-Normal Distribution

Skew-Normal Distribution

## Usage

``` r
dskewnorm(x, mean = 0, sd = 1, shape = 0, log = FALSE)

pskewnorm(q, mean = 0, sd = 1, shape = 0)

qskewnorm(p, mean = 0, sd = 1, shape = 0)

rskewnorm(n = 1, mean = 0, sd = 1, shape = 0)
```

## Arguments

- x:

  A numeric vector of values.

- mean:

  A numeric vector of the means.

- sd:

  A non-negative numeric vector of the standard deviations.

- shape:

  A numeric vector of values.

- log:

  A flag specifying whether to return the log-transformed value.

- q:

  A vector of quantiles.

- p:

  A vector of probabilities.

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
dskewnorm(x = -2:2, mean = 0, sd = 1, shape = 0.1)
#> [1] 0.04543235 0.22269638 0.39894228 0.26124507 0.06254958
dskewnorm(x = -2:2, mean = 0, sd = 1, shape = -1)
#> [1] 0.105525330 0.407161596 0.398942280 0.076779853 0.002456603
qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = 0.1)
#> [1] -1.1980898 -0.1731883
qskewnorm(p = c(0.1, 0.4), mean = 0, sd = 1, shape = -1)
#> [1] -1.6322188 -0.7540709
pskewnorm(q = -2:2, mean = 0, sd = 1, shape = 0.1)
#> [1] 0.01848493 0.13944469 0.46827448 0.82213418 0.97298466
pskewnorm(q = -2:2, mean = 0, sd = 1, shape = -1)
#> [1] 0.0449827 0.2921390 0.7500000 0.9748285 0.9994824
rskewnorm(n = 3, mean = 0, sd = 1, shape = 0.1)
#> [1] -0.1800752 -0.6041470  1.5313254
rskewnorm(n = 3, mean = 0, sd = 1, shape = -1)
#> [1] -0.3666041 -0.5070603 -0.0122818
```
