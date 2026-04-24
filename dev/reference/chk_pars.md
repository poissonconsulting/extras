# Check Parameter Names

Checks if valid parameter names.

## Usage

``` r
chk_pars(x, x_name = NULL)

vld_pars(x)
```

## Arguments

- x:

  An object.

- x_name:

  A string of the name of object x or NULL.

## Value

The `chk_` function throws an informative error if the test fails.

The `vld_` function returns a flag indicating whether the test was met.

## Details

The character vector must consist of values that start with an alpha and
only include alphanumeric characters and '\_' or '.'.

Missing values and duplicates are permitted.

## Functions

- `vld_pars()`: Validate Parameter Names

## Examples

``` r
x <- c("x", "a1._", "X")
chk_pars(x)
y <- c("x[1]", "a1", "a1", "._0")
try(chk_pars(y))
#> Error in chk_pars(y) : 
#>   `y` must have values matching regular expression '^[[:alpha:]][[:alnum:]._]*$'.
vld_pars(c("x", "a1._", "X"))
#> [1] TRUE
vld_pars(c("x[1]", "a1", "a1", "._0"))
#> [1] FALSE
```
