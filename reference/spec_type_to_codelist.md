# Spec to codelist

Creates the value_spec from a list of datasets (optionally filtered by
the sheet input). The named vector `*_cols` is used to determine which
is the correct sheet and renames the columns.

## Usage

``` r
spec_type_to_codelist(
  doc,
  codelist_cols = c(code_id = "ID", name = "[N|n]ame", code = "^[C|c]ode|^[T|t]erm",
    decode = "[D|d]ecode"),
  permitted_val_cols = NULL,
  dict_cols = c(code_id = "ID", name = "[N|n]ame", dictionary = "[D|d]ictionary", version
    = "[V|v]ersion"),
  sheets = NULL,
  simplify = FALSE
)
```

## Arguments

- doc:

  Named list of datasets @seealso
  [`read_all_sheets()`](https://atorus-research.github.io/metacore/reference/read_all_sheets.md)
  for exact format

- codelist_cols:

  Named vector of column names that make up the codelist. The column
  names can be regular expressions for more flexibility. But, the names
  must follow the given pattern

- permitted_val_cols:

  Named vector of column names that make up the permitted value The
  column names can be regular expressions for more flexibility. This is
  optional, can be left as null if there isn't a permitted value sheet

- dict_cols:

  Named vector of column names that make up the dictionary value The
  column names can be regular expressions for more flexibility. This is
  optional, can be left as null if there isn't a permitted value sheet

- sheets:

  Optional, regular expressions of the sheets

- simplify:

  Boolean value, if true will convert code/decode pairs that are all
  equal to a permitted value list. True by default

## Value

a dataset formatted for the metacore object

## See also

Other spec builders:
[`spec_type_to_derivations()`](https://atorus-research.github.io/metacore/reference/spec_type_to_derivations.md),
[`spec_type_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_spec.md),
[`spec_type_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_vars.md),
[`spec_type_to_value_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_value_spec.md),
[`spec_type_to_var_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_var_spec.md)
