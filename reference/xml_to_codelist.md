# XML to code list

Reads in a define xml and creates a code_list table. The code_list table
is a nested tibble where each row is a code list or permitted value
list. The code column contains a vector of a tibble depending on if it
is a permitted values or code list

## Usage

``` r
xml_to_codelist(doc)
```

## Arguments

- doc:

  xml document

## Value

a tibble containing the code list and permitted value information

## See also

Other xml builder:
[`xml_to_derivations()`](https://atorus-research.github.io/metacore/reference/xml_to_derivations.md),
[`xml_to_ds_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_spec.md),
[`xml_to_ds_vars()`](https://atorus-research.github.io/metacore/reference/xml_to_ds_vars.md),
[`xml_to_value_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_value_spec.md),
[`xml_to_var_spec()`](https://atorus-research.github.io/metacore/reference/xml_to_var_spec.md)
