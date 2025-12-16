# Get path to metacore example

metacore comes bundled with a number of sample files in its
`inst/extdata` directory. This function make them easy to access. When
testing or writing examples in other packages, it is best to use the
'pilot_ADaM.rda' example as it loads fastest.

## Usage

``` r
metacore_example(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Examples

``` r
metacore_example()
#> [1] "ADaM_define_CDISC_pilot3.xml" "SDTM_define.xml"             
#> [3] "SDTM_spec_CDISC_pilot.xlsx"   "mock_spec.xlsx"              
#> [5] "p21_mock.xlsx"                "pilot_ADaM.rda"              
#> [7] "pilot_SDTM.rda"              
metacore_example("mock_spec.xlsx")
#> [1] "/home/runner/work/_temp/Library/metacore/extdata/mock_spec.xlsx"
```
