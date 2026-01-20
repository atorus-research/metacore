# Define XML to DataDef Object

Given a path, this function converts the define xml to a
DataDef/Metacore object.

## Usage

``` r
define_to_metacore(path, quiet = deprecated(), verbose = "message")
```

## Arguments

- path:

  location of the define xml as a string

- quiet:

  **\[superseded\]** Option to quietly load in, this will suppress
  warnings, but not errors. Expects either `TRUE` or `FALSE`. Default
  behaviour is `FALSE`.

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

Metacore/DataDef object
