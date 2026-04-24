# As List

Coerces an object to an list. All attributes are removed except any
names.

## Usage

``` r
as_list(x, ...)

# Default S3 method
as_list(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Other arguments passed to methods.

## Value

A list.

## Examples

``` r
as_list(1:3)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
as_list(c(x = 1, y = 2))
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
```
