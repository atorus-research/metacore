# Optional checks to consistency of metadata

These functions check to see if values (e.g labels, formats) that should
be consistent for a variable across all data are actually consistent.

## Usage

``` r
check_inconsistent_labels(metacore)

check_inconsistent_types(metacore)

check_inconsistent_formats(metacore)
```

## Arguments

- metacore:

  metacore object to check

## Value

If all variables are consistent it will return a message. If there are
inconsistencies it will return a message and a dataset of the variables
with inconsistencies.

## Examples

``` r
## EXAMPLE WITH DUPLICATES
# Loads in a metacore obj called metacore
load(metacore_example("pilot_ADaM.rda"))
check_inconsistent_labels(metacore)
#> Warning: Mismatch labels detected
#> # A tibble: 4 × 4
#>   variable label                     n_vars ls_of_vars
#>   <chr>    <chr>                      <int> <list>    
#> 1 ANL01FL  Analysis Flag 01               1 <chr [1]> 
#> 2 ANL01FL  Analysis Record Flag 01        1 <chr [1]> 
#> 3 USUBJID  Unique Subject Identified      3 <chr [3]> 
#> 4 USUBJID  Unique Subject Identifier      2 <chr [2]> 

check_inconsistent_types(metacore)
#> Warning: Mismatch types detected
#> # A tibble: 6 × 4
#>   variable type    n_vars ls_of_vars
#>   <chr>    <chr>    <int> <list>    
#> 1 AVAL     float        1 <chr [1]> 
#> 2 AVAL     integer      2 <chr [2]> 
#> 3 BASE     float        1 <chr [1]> 
#> 4 BASE     integer      1 <chr [1]> 
#> 5 CHG      float        1 <chr [1]> 
#> 6 CHG      integer      1 <chr [1]> 

## EXAMPLE WITHOUT DUPLICATES
# Loads in a metacore obj called metacore
load(metacore_example("pilot_SDTM.rda"))
check_inconsistent_labels(metacore)
#> No mismatch labels detected

check_inconsistent_formats(metacore)
#> No mismatch formats detected

check_inconsistent_types(metacore)
#> No mismatch types detected
```
