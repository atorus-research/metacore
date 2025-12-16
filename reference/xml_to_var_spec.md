# XML to variable spec

Takes a define xml and returns a dataset with specifications for each
variable. The variable will just be the variable, unless the
specification for that variable differ between datasets

## Usage

``` r
xml_to_var_spec(doc)
```

## Arguments

- doc:

  define xml document

## Value

data frame with variable, length, label columns

## See also

Other xml builder:
[`xml_to_codelist()`](https://atorus-research.github.io/metacore/reference/xml_to_codelist.md),
[`xml_to_derivations()`](https://atorus-research.github.io/metacore/reference/xml_to_derivations.md),
[`xml_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_spec.md),
[`xml_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_vars.md),
[`xml_to_value_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_value_spec.md)
