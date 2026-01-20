# Example

``` r
library(metacore)
#> Attaching package `metacore`
#> 
#> As of metacore 0.3.0 the `keep` variable in the `ds_vars` table has been renamed to `mandatory`. Please see release documentation for details.
library(xml2)
```

[CDISC](https://www.cdisc.org/) standards provide a standard for
submission of data set metadata through a document known as define.xml.
The define provides a great deal of useful information that is both
machine readable and can be viewed through your web browser. While many
organizations wait to produce a define until the datasets are finalized,
it can still be advantageous to be able to read metadata directly from a
define. For this purpose, we developed readers that can go directly from
a define.xml to a metacore object.

To do this, we’ve built separate reader function for each of the
metacore tables. For more information on the structure of the metacore
tables, check out the README.

We start by reading the define from disk using the `xmlTreeParse()`
function from the `XML` package.

``` r
doc <- read_xml(metacore_example("SDTM_define.xml"))
xml_ns_strip(doc)
```

Next, we use the metacore readers for each of the separate tables
necessary for a metacore object.

``` r
ds_spec2 <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)
var_spec <- xml_to_var_spec(doc)
value_spec <- xml_to_value_spec(doc)
code_list <- xml_to_codelist(doc)
derivations <- xml_to_derivations(doc)
```

Great! Now we’re ready to create our metacore object.

``` r
test <- metacore(ds_spec2, ds_vars, var_spec, value_spec, derivations, code_list)
#> Warning: The following word in value_spec$origin is not allowed:
#> ℹ edt
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing
#> values.
#> Warning: `common` from the `var_spec` table only contains missing
#> values.
#> Warning: `dataset` from the `supp` table only contains missing values.
#> Warning: `variable` from the `supp` table only contains missing values.
#> Warning: `idvar` from the `supp` table only contains missing values.
#> Warning: `qeval` from the `supp` table only contains missing values.
#> Warning: The following derivations are never used:
#> ℹ MT.SUPPAE.QVAL MT.SUPPDM.QVAL
#> Warning: The following codelists are never used:
#> ℹ DRUG DICTIONARY MEDICAL HISTORY DICTIONARY
#> ✔ Metadata successfully imported
#> ℹ To use the Metacore object with metatools package, first subset a dataset
#>   using `metacore::select_dataset()`
```

Something to note about a metacore object is that it inherently holds
all data from your source of metadata, be it your specification,
define.xml, database, etc. So that means you have *all* the metadata. In
your program, it’s likely that you’ll just want to keep metadata
relevant to the dataset you’re currently programming. We’ve made process
easily, with functions that filter metadata down to information only
relevant to a specific dataset.

``` r
# a metacore object with all your dataframes
subset <- test %>% select_dataset("DM")
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing
#> values.
#> Warning: `format` from the `var_spec` table only contains missing
#> values.
#> Warning: `common` from the `var_spec` table only contains missing
#> values.
#> Warning: `sig_dig` from the `value_spec` table only contains missing
#> values.
#> Warning: `where` from the `value_spec` table only contains missing
#> values.
#> Warning: `dataset` from the `supp` table only contains missing values.
#> Warning: `variable` from the `supp` table only contains missing values.
#> Warning: `idvar` from the `supp` table only contains missing values.
#> Warning: `qeval` from the `supp` table only contains missing values.
#> ✔ DM dataset successfully selected
subset$ds_spec
#> # A tibble: 1 × 3
#>   dataset structure              label       
#>   <chr>   <chr>                  <chr>       
#> 1 DM      One record per subject Demographics

# a simplified dataframe
subset_t <- test %>% select_dataset("DM", simplify = TRUE)
#> Joining with `by = join_by(variable)`
#> Joining with `by = join_by(dataset, variable, type)`
#> Joining with `by = join_by(derivation_id)`
#> Joining with `by = join_by(code_id)`
#> Joining with `by = join_by(dataset, variable)`
```

As can be seen above, the metacore object can be filtered directly, or
by using the `simplify = TRUE` argument, a simplified data frame can be
returned.

``` r
subset_t
#> # A tibble: 25 × 21
#>    dataset variable key_seq order mandatory core  supp_flag type    length label
#>    <chr>   <chr>      <int> <int> <lgl>     <chr> <lgl>     <chr>    <int> <chr>
#>  1 DM      STUDYID        1     1 TRUE      NA    NA        text        12 Stud…
#>  2 DM      DOMAIN        NA     2 TRUE      NA    NA        text         2 Doma…
#>  3 DM      USUBJID        2     3 TRUE      NA    NA        text        11 Uniq…
#>  4 DM      SUBJID        NA     4 TRUE      NA    NA        text         4 Subj…
#>  5 DM      RFSTDTC       NA     5 FALSE     NA    NA        date        10 Subj…
#>  6 DM      RFENDTC       NA     6 FALSE     NA    NA        date        10 Subj…
#>  7 DM      RFXSTDTC      NA     7 FALSE     NA    NA        dateti…     20 Date…
#>  8 DM      RFXENDTC      NA     8 FALSE     NA    NA        dateti…     20 Date…
#>  9 DM      RFICDTC       NA     9 FALSE     NA    NA        dateti…     20 Date…
#> 10 DM      RFPENDTC      NA    10 FALSE     NA    NA        dateti…     20 Date…
#> # ℹ 15 more rows
#> # ℹ 11 more variables: format <chr>, common <lgl>, code_id <chr>,
#> #   derivation_id <chr>, origin <chr>, where <chr>, sig_dig <int>,
#> #   derivation <chr>, codes <list>, idvar <chr>, qeval <chr>
```
