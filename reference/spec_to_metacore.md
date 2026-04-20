# Specification document to metacore object

This function takes the location of an excel specification document and
reads it in as a meta core object. At the moment it only supports
specification in the format of pinnacle 21 specifications. But, the
section level spec builder can be used as building blocks for bespoke
specification documents.

## Usage

``` r
spec_to_metacore(
  path,
  quiet = deprecated(),
  where_sep_sheet = TRUE,
  verbose = "message"
)
```

## Arguments

- path:

  string of file location

- quiet:

  **\[superseded\]** Option to quietly load in, this will suppress
  warnings, but not errors. Expects either `TRUE` or `FALSE`. Default
  behaviour is `FALSE`. From v0.3.0 this argument is deprecated in
  favour of `verbose`.

- where_sep_sheet:

  Option to tell if the where is in a separate sheet, like in older p21
  specs or in a single sheet like newer p21 specs.

- verbose:

  A character string specifying the desired verbosity level. Must be one
  of:

  "message"

  :   (default) Messages and warnings are handled normally.

  "warn"

  :   Messages are suppressed, but warnings are allowed.

  "collapse"

  :   Warnings are collapsed into a single message indicating the number
      of suppressed warnings.

  "silent"

  :   Both messages and warnings are suppressed.

## Value

given a spec document it returns a metacore object

## Examples

``` r
# Run `spec_to_metacore` with `verbose = "collapse"`
spec_path <- metacore_example("p21_mock.xlsx")
metacore <- spec_to_metacore(
path = spec_path,
verbose = "collapse"
)
#> ✔ Metadata successfully imported
#> ℹ To use the Metacore object with metatools package, first subset a dataset
#>   using `metacore::select_dataset()`
#> ℹ Operation performed with 5 suppressed warnings. Set `verbose = "warn"` to
#>   show.

# Run `spec_to_metacore` with `verbose = "warn"`
metacore <- spec_to_metacore(
path = spec_path,
verbose = "warn"
)
#> Warning: The following word in value_spec$origin is not allowed:
#> ℹ edt
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing values.
#> Warning: The following derivations are never used:
#> ℹ SUPPAE.QVAL, SUPPDM.QVAL
#> Warning: The following codelists are never used:
#> ℹ DRUG DICTIONARY, MEDICAL HISTORY DICTIONARY
```
