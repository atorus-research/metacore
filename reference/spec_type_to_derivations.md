# Spec to derivation

Creates the derivation table from a list of datasets (optionally
filtered by the sheet input). The named vector `cols` is used to
determine which is the correct sheet and renames the columns. The
derivation will be used for "derived" origins, the comments for
"assigned" origins, and predecessor for "predecessor" origins.

## Usage

``` r
spec_type_to_derivations(
  doc,
  cols = c(derivation_id = "ID", derivation = "[D|d]efinition|[D|d]escription"),
  sheet = "Method|Derivations?",
  var_cols = c(dataset = "[D|d]ataset|[D|d]omain", variable = "[N|n]ame|[V|v]ariables?",
    origin = "[O|o]rigin", predecessor = "[P|p]redecessor", comment = "[C|c]omment")
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

- sheet:

  Regular expression for the sheet name

- var_cols:

  Named vector of the name(s) of the origin, predecessor and comment
  columns. These do not have to be on the specified sheet.

## Value

a dataset formatted for the metacore object

## See also

Other spec builders:
[`spec_type_to_codelist()`](https://atorus-research.github.io/metacore/reference/spec_type_to_codelist.md),
[`spec_type_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_spec.md),
[`spec_type_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_vars.md),
[`spec_type_to_value_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_value_spec.md),
[`spec_type_to_var_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_var_spec.md)
