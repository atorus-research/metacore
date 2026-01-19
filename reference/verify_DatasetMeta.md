# Verify that the Class Type of an object is DatasetMeta with warnings

This function that is a wrapper to the functions `is_metacore` and
`is_DatasetMeta`.

This function is not intended to be called directly by the user. It is
used as a guard clause in many features of the `{metatools}` package
that are intended only to be used with the subsetted Metacore object of
class type `DatasetMeta`. If either of the wrapped functions return
`FALSE `then execution is stopped and an appropriate error message is
displayed.

## Usage

``` r
verify_DatasetMeta(metacore)
```

## Arguments

- metacore:

  An object whose class type needs to be checked.

## Value

Logical: TRUE if the class type of `metacore` is `DatasetMeta`,
otherwise abort with errors.

## Examples

``` r
load(metacore_example("pilot_ADaM.rda"))
adsl <- select_dataset(metacore, "ADSL", quiet = TRUE)
if (FALSE) { # \dontrun{
verify_DatasetMeta("DUMMY") # Expect error
verify_DatasetMeta(metacore) # Expect error
} # }
verify_DatasetMeta(adsl) # Expect valid, i.e., return TRUE
#> [1] TRUE
```
