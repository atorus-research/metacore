# Create table

This function creates a table from excel sheets. This is mainly used
internally for building spec readers, but is exported so others who need
to build spec readers can use it.

## Usage

``` r
create_tbl(doc, cols)
```

## Arguments

- doc:

  list of sheets from a excel doc

- cols:

  vector of regex to get a datasets base on which columns it has. If the
  vector is named it will also rename the columns

## Value

dataset (or list of datasets if not specific enough)
