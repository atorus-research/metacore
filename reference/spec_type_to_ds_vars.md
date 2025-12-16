# Spec to ds_vars

Creates the ds_vars from a list of datasets (optionally filtered by the
sheet input). The named vector `cols` is used to determine which is the
correct sheet and renames the columns

## Usage

``` r
spec_type_to_ds_vars(
  doc,
  cols = c(dataset = "[D|d]ataset|[D|d]omain", variable =
    "[V|v]ariable [[N|n]ame]?|[V|v]ariables?", order =
    "[V|v]ariable [O|o]rder|[O|o]rder", mandatory = "[K|k]eep|[M|m]andatory"),
  key_seq_sep_sheet = TRUE,
  key_seq_cols = c(dataset = "Dataset", key_seq = "Key Variables"),
  sheet = "[V|v]ar|Datasets"
)
```

## Arguments

- doc:

  Named list of datasets @seealso
  [`read_all_sheets()`](https://atorus-research.github.io/metacore/reference/read_all_sheets.md)
  for exact format

- cols:

  Named vector of column names. The column names can be regular
  expressions for more flexibility. But, the names must follow the given
  pattern

- key_seq_sep_sheet:

  A boolean to indicate if the key sequence is on a separate sheet. If
  set to false add the key_seq column name to the `cols` vector.

- key_seq_cols:

  names vector to get the key_sequence for each dataset

- sheet:

  Regular expression for the sheet names

## Value

a dataset formatted for the metacore object

## See also

Other spec builders:
[`spec_type_to_codelist()`](https://atorus-research.github.io/metacore/reference/spec_type_to_codelist.md),
[`spec_type_to_derivations()`](https://atorus-research.github.io/metacore/reference/spec_type_to_derivations.md),
[`spec_type_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_spec.md),
[`spec_type_to_value_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_value_spec.md),
[`spec_type_to_var_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_var_spec.md)
