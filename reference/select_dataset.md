# Select metacore object to single dataset

Select metacore object to single dataset

## Usage

``` r
select_dataset(.data, dataset, simplify = FALSE, quiet = FALSE)
```

## Arguments

- .data:

  the metacore object of dataframes

- dataset:

  the specific dataset to subset by

- simplify:

  return a single dataframe

- quiet:

  Option to quietly load in, this will suppress warnings, but not
  errors. Expects either `TRUE` or `FALSE`. Default behaviour is
  `FALSE`.

## Value

a filtered subset of the metacore object
