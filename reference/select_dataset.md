# Select metacore object to single dataset

Select metacore object to single dataset

## Usage

``` r
select_dataset(
  .data,
  dataset,
  simplify = FALSE,
  quiet = deprecated(),
  verbose = "message"
)
```

## Arguments

- .data:

  the metacore object of dataframes

- dataset:

  the specific dataset to subset by

- simplify:

  return a single dataframe

- quiet:

  **\[superseded\]** Option to quietly load in, this will suppress
  warnings, but not errors. Expects either `TRUE` or `FALSE`. Default
  behaviour is `FALSE`.

- verbose:

  A character string specifying the desired verbosity level. Must be one
  of:

  "message"

  :   (default) Messages and warnings are handled normally.

  "warn"

  :   Messages are suppressed, but warnings are allowed.

  "collapse"

  :   Warnings are collapsed into a single message indicating the number
      of suppressed warnings.

  "silent"

  :   Both messages and warnings are suppressed.

## Value

a filtered subset of the metacore object
