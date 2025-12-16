# Spec to value_spec

Creates the value_spec from a list of datasets (optionally filtered by
the sheet input). The named vector `cols` is used to determine which is
the correct sheet and renames the columns

## Usage

``` r
spec_type_to_value_spec(
  doc,
  cols = c(dataset = "[D|d]ataset|[D|d]omain", variable = "[N|n]ame|[V|v]ariables?",
    origin = "[O|o]rigin", type = "[T|t]ype", code_id = "[C|c]odelist|Controlled Term",
    sig_dig = "[S|s]ignificant", where = "[W|w]here", derivation_id = "[M|m]ethod",
    predecessor = "[P|p]redecessor"),
  sheet = NULL,
  where_sep_sheet = TRUE,
  where_cols = c(id = "ID", where = c("Variable", "Comparator", "Value")),
  var_sheet = "[V|v]ar"
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

- where_sep_sheet:

  Boolean value to control if the where information in a separate
  dataset. If the where information is on a separate sheet, set to true
  and provide the column information with the `where_cols` inputs.

- where_cols:

  Named list with an id and where field. All columns in the where field
  will be collapsed together

- var_sheet:

  Name of sheet with the Variable information on it. Metacore expects
  each variable will have a row in the value_spec. Because many
  specification only have information in the value tab this is added. If
  the information already exists in the value tab of your specification
  set to NULL

## Value

a dataset formatted for the metacore object

## See also

Other spec builders:
[`spec_type_to_codelist()`](https://atorus-research.github.io/metacore/reference/spec_type_to_codelist.md),
[`spec_type_to_derivations()`](https://atorus-research.github.io/metacore/reference/spec_type_to_derivations.md),
[`spec_type_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_spec.md),
[`spec_type_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_vars.md),
[`spec_type_to_var_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_var_spec.md)
