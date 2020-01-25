
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extras

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/poissonconsulting/extras.svg?branch=master)](https://travis-ci.com/poissonconsulting/extras)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/extras?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/extras)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/extras/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/extras?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- [![Tinyverse status](https://tinyverse.netlify.com/badge/extras)](https://CRAN.R-project.org/package=extras) -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/extras)](https://cran.r-project.org/package=extras) -->
<!-- ![CRAN downloads](https://cranlogs.r-pkg.org/badges/extras) -->
<!-- badges: end -->

`extras` provides extra functions for data modeling.

It includes functions to numericise objects; simple methods for S3
generics in the universals package; and R translations of common JAGS
and TMB functions.

## Installation

<!-- To install the latest release from [CRAN](https://cran.r-project.org) -->

To install the developmental version from
[GitHub](https://github.com/poissonconsulting/extras)

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/extras")
```

To install the latest developmental release from the Poisson drat
[repository](https://github.com/poissonconsulting/drat)

``` r
# install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("extras")
```

## Demonstration

### Numericise Objects

The `numericise()` (or `numericize()`) function converts simple objects
to a list of numeric atomic object suitable for input to an analytic
engine.

``` r
library(extras)

data <- data.frame(logical = TRUE, real = 1.1, integer = 3L, 
                   factor = factor("b", levels = c("a", "b")),
                   date = as.Date("2001-01-01"),
                   datetime = as.POSIXct("2001-03-01"))

numericise(data)
#>   logical real integer factor  date  datetime
#> 1       1  1.1       3      2 11323 983433600
```

### Simple universals Methods

The `extras` package provides simple methods for S3 generics in the
universals package. For example the `dims()` and `ndims()` functions are
defined for vectors.

``` r
x <- 1:10
dim(x) # the base R function
#> NULL
dims(x) 
#> [1] 10
ndims(x)
#> [1] 1
```

## JAGS and TMB Functions

`extras` also provides R versions of common JAGS and TMB (C++) functions
such as `logit<-` and `pow()`.

``` r
mu <- NULL
logit(mu) <- 2
mu
#> [1] 0.8807971
```

## Information

For more information see the [Get
Started](https://poissonconsulting.github.io/extras/articles/extras.html)
vignette.

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/extras/issues).

[Pull requests](https://github.com/poissonconsulting/extras/pulls) are
always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/poissonconsulting/extras/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
