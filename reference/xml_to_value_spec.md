# XML to value spec

Takes a define xml and pulls out the value level metadata including
codelist_id's, defines_id's, and where clause. There is one row per
variable expect when there is a where clause, at which point there is
one row per value.

## Usage

``` r
xml_to_value_spec(doc)
```

## Arguments

- doc:

  xml document

## Value

tibble with the value level information

## See also

Other xml builder:
[`xml_to_codelist()`](https://atorus-research.github.io/metacore/reference/xml_to_codelist.md),
[`xml_to_derivations()`](https://atorus-research.github.io/metacore/reference/xml_to_derivations.md),
[`xml_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_spec.md),
[`xml_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_vars.md),
[`xml_to_var_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_var_spec.md)
