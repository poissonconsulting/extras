# As List

Coerces an object to an list. All attributes are removed except any
names.

## Usage

``` r
as_list_unnamed(x, ...)

# Default S3 method
as_list_unnamed(x, ...)
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
as_list_unnamed(1:3)
#> Warning: `as_list_unnamed()` was deprecated in extras 0.1.1.
#> ℹ Please use `as_list()` instead.
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
as_list_unnamed(c(x = 1, y = 2))
#> $x
#> [1] 1
#> 
#> $y
#> [1] 2
#> 
```
