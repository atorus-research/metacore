# Is DatasetMeta object

Is DatasetMeta object

## Usage

``` r
is_DatasetMeta(x)
```

## Arguments

- x:

  object to check

## Value

`TRUE` if DatasetMeta, `FALSE` if not

## Examples

``` r
load(metacore_example("pilot_ADaM.rda"))
adsl <- select_dataset(metacore, "ADSL", quiet = TRUE)
#> Warning: The `quiet` argument of `select_dataset()` is deprecated as of metacore 0.3.0.
#> ℹ Please use the `verbose` argument instead.
is_DatasetMeta("DUMMY") # Expect FALSE
#> [1] FALSE
is_DatasetMeta(metacore) # Expect FALSE
#> [1] FALSE
is_DatasetMeta(adsl) # Expect TRUE
#> [1] TRUE
```
