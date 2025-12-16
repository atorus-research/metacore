# Specification document to metacore object

This function takes the location of an excel specification document and
reads it in as a meta core object. At the moment it only supports
specification in the format of pinnacle 21 specifications. But, the
section level spec builder can be used as building blocks for bespoke
specification documents.

## Usage

``` r
spec_to_metacore(path, quiet = FALSE, where_sep_sheet = TRUE)
```

## Arguments

- path:

  string of file location

- quiet:

  Option to quietly load in; when `TRUE`, messages, warnings, and other
  non-error console output are suppressed, but errors are still raised.

- where_sep_sheet:

  Option to tell if the where is in a separate sheet, like in older p21
  specs or in a single sheet like newer p21 specs

## Value

given a spec document it returns a metacore object
