# XML to derivation table

This reads in a xml document and gets all the derivations/comments.
These can be cross referenced to variables using the derivation_id's

## Usage

``` r
xml_to_derivations(doc)
```

## Arguments

- doc:

  xml document

## Value

dataframe with derivation id's and derivations

## See also

Other xml builder:
[`xml_to_codelist()`](https://atorus-research.github.io/metacore/reference/xml_to_codelist.md),
[`xml_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_spec.md),
[`xml_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_vars.md),
[`xml_to_value_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_value_spec.md),
[`xml_to_var_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_var_spec.md)
