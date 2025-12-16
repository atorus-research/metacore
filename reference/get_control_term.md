# Get Control Term

Returns the control term (a vector for permitted values and a tibble for
code lists) for a given variable. The dataset can be optionally
specified if there is different control terminology for different
datasets

## Usage

``` r
get_control_term(metacode, variable, dataset = NULL)
```

## Arguments

- metacode:

  metacore object

- variable:

  A variable name to get the controlled terms for. This can either be a
  string or just the name of the variable

- dataset:

  A dataset name. This is not required if there is only one set of
  control terminology across all datasets

## Value

a vector for permitted values and a 2-column tibble for codelists

## Examples

``` r
if (FALSE) { # \dontrun{
meta_ex <- spec_to_metacore(metacore_example("p21_mock.xlsx"))
get_control_term(meta_ex, QVAL, SUPPAE)
get_control_term(meta_ex, "QVAL", "SUPPAE")
} # }
```
