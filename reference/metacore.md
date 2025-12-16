# R6 Class wrapper to create your own metacore object

R6 Class wrapper to create your own metacore object

## Usage

``` r
metacore(
  ds_spec = tibble(dataset = character(), structure = character(), label = character()),
  ds_vars = tibble(dataset = character(), variable = character(), mandatory = logical(),
    key_seq = integer(), order = integer(), core = character(), supp_flag = logical()),
  var_spec = tibble(variable = character(), label = character(), length = integer(), type
    = character(), common = character(), format = character()),
  value_spec = tibble(dataset = character(), variable = character(), where = character(),
    type = character(), sig_dig = integer(), code_id = character(), origin = character(),
    derivation_id = integer()),
  derivations = tibble(derivation_id = integer(), derivation = character()),
  codelist = tibble(code_id = character(), name = character(), type = character(), codes
    = list()),
  supp = tibble(dataset = character(), variable = character(), idvar = character(), qeval
    = character()),
  quiet = FALSE
)
```

## Arguments

- ds_spec:

  contains each dataset in the study, with the labels for each

- ds_vars:

  information on what variables are in each dataset + plus dataset
  specific variable information

- var_spec:

  variable information that is shared across all datasets

- value_spec:

  parameter specific information, as data is long the specs for wbc
  might be difference the hgb

- derivations:

  contains derivation, it allows for different variables to have the
  same derivation

- codelist:

  contains the code/decode information

- supp:

  contains the idvar and qeval information for supplemental variables

- quiet:

  Option to quietly load in, this will suppress warnings, but not
  errors. Expects either `TRUE` or `FALSE`. Default behaviour is
  `FALSE`.
