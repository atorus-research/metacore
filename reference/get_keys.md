# Get Dataset Keys

Returns the dataset keys for a given dataset

## Usage

``` r
get_keys(metacode, dataset)
```

## Arguments

- metacode:

  metacore object

- dataset:

  A dataset name

## Value

a 2-column tibble with dataset key variables and key sequence

## Examples

``` r
if (FALSE) { # \dontrun{
meta_ex <- spec_to_metacore(metacore_example("p21_mock.xlsx"))
get_keys(meta_ex, "AE")
get_keys(meta_ex, AE)
} # }
```
