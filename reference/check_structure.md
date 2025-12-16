# Column Validation Function

Column Validation Function

## Usage

``` r
check_structure(.data, col, func, any_na_acceptable, nm)
```

## Arguments

- .data:

  the dataframe to check the column for

- col:

  the column to test

- func:

  the function to use to assert column structure

- any_na_acceptable:

  boolean, testing if the column can have missing

- nm:

  name of column to check (for warning and error clarification)
