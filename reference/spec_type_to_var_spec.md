# Spec to var_spec

Creates the var_spec from a list of datasets (optionally filtered by the
sheet input). The named vector `cols` is used to determine which is the
correct sheet and renames the columns. (Note: the keep column will be
converted logical)

## Usage

``` r
spec_type_to_var_spec(
  doc,
  cols = c(variable = "[N|n]ame|[V|v]ariables?", length = "[L|l]ength", label =
    "[L|l]abel", type = "[T|t]ype", dataset = "[D|d]ataset|[D|d]omain", format =
    "[F|f]ormat"),
  sheet = "[V|v]ar"
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

## Value

a dataset formatted for the metacore object

## See also

Other spec builders:
[`spec_type_to_codelist()`](https://atorus-research.github.io/metacore/reference/spec_type_to_codelist.md),
[`spec_type_to_derivations()`](https://atorus-research.github.io/metacore/reference/spec_type_to_derivations.md),
[`spec_type_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_spec.md),
[`spec_type_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/spec_type_to_ds_vars.md),
[`spec_type_to_value_spec()`](https://atorus-research.github.io/metacore/reference/spec_type_to_value_spec.md)
