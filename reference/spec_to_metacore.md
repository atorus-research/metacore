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
  behaviour is `FALSE`.

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
