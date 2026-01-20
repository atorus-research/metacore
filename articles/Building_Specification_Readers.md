# Building Specification Readers

``` r
library(metacore)
#> Attaching package `metacore`
#> 
#> As of metacore 0.3.0 the `keep` variable in the `ds_vars` table has been renamed to `mandatory`. Please see release documentation for details.
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
library(stringr)
```

The first thing to do when trying to build a specification reader is to
try the default. By default metacore can read in specifications that are
in the Pinnacle 21 specification format. If your document isn’t in that
format, it is still worth trying the default readers, as the error
messages can be helpful.

``` r
spec_to_metacore(metacore_example("mock_spec.xlsx"))
#> Error in `create_tbl()`:
#> ! Unable to rename the following columns in Domains
#> ✖ label matches 2 columns: c("Label", "Description")
#> ℹ Please check your regular expression
```

As we can see, the mock spec we are using here doesn’t match the format.
Therefore we will have to build a bespoke reader. Before we start, it is
important to understand the structure of the metacore object. Each
object acts as its own database for all dataset related metadata. The
object has 7 tables and their general purpose are as follows:

- **ds_spec**: Contains dataset level information

- **ds_vars**: Bridges the dataset and variable level information

- **var_spec**: Contains variable level information

- **value_spec**: Contains value level information

- **derivations**: Contains all derivations

- **codelist**: Contains information about code/decodes, permitted
  values and external libraries

- **supp**: Contains information specific to supplemental variables

Here is a schema of how all this fits together

![Schema diagram showing the relationships between all metacore
tables](../reference/figures/labeled_schema.png "Metacore Schema")

Schema diagram showing the relationships between all metacore tables

ds_spec is connected to ds_vars by the ‘dataset’ variable and ds_vars is
connected to var_spec by the ‘variable’ variable, etc. For more
information on the make-up of metacore objects please see the README.

Now that we understand what makes a metacore object, we can start to
develop the reader.

First, we need to identify what type of specification format you have.
At the moment we support a specification where each tab contains
information relevant to a different table, such as a domain tab, a
variable tab, etc. To test this you can use the `spec_type` function.

``` r
metacore:::spec_type(metacore_example("mock_spec.xlsx"))
#> [1] "by_type"
```

Given we have the type style of specification, we can attempt to run
with the lower level built-in specification to metacore build. There are
6 lower level specification builders to match each of the 6 datasets
needed, spec_type_to\_\*. Even if these fail, the error messages should
help identify the issues.

But, before we start any of that, we need to read in our document using
the `read_all_sheets` function. This function reads in a multisheet
excel file into a named list, where the name of each dataset is the name
of the tab. The lower level specification builders do assume the
provided `doc` is a named list. This mock specification has 5 tabs,
domain, variables, value level metadata, controlled terms, and
computational method. So it looks like we might need to split the
information in these tabs to get the 6 tables needed for the metacore
object.

``` r
doc <- read_all_sheets(metacore_example("mock_spec.xlsx"))
doc %>% map(head)
#> $Domains
#> # A tibble: 4 × 10
#>   `Domain Name` Label    Repeating `Is Reference?` Class Source `Data Structure`
#>   <chr>         <chr>    <chr>     <chr>           <chr> <chr>  <chr>           
#> 1 ADDS          Disposi… Y         N               BDS   DS     One record per …
#> 2 ADRACE        Race An… Y         N               OTHER DM, S… One record per …
#> 3 ADTRT         Treatme… Y         N               OTHER ADSL   One record per …
#> 4 ADSL          Subject… N         N               ADSL  NA     One record per …
#> # ℹ 3 more variables: `Key Variables` <chr>, Description <chr>,
#> #   Documentation <chr>
#> 
#> $Variables
#> # A tibble: 6 × 18
#>   `Domain Name` `Therapeutic Area` Indication `Variable Name` Label        Type 
#>   <chr>         <chr>              <chr>      <chr>           <chr>        <chr>
#> 1 ADAE          CORE               CORE       STUDYID         Study Ident… Text 
#> 2 ADAE          CORE               CORE       USUBJID         Unique Subj… Text 
#> 3 ADAE          CORE               CORE       SUBJID          Subject Ide… Text 
#> 4 ADAE          CORE               CORE       SITEID          Study Site … Text 
#> 5 ADAE          CORE               CORE       AGE             Age          Inte…
#> 6 ADAE          CORE               CORE       SEX             Sex          Text 
#> # ℹ 12 more variables: `Max Length` <chr>, `Variable Order` <chr>,
#> #   Mandatory <chr>, `Signficant Digits` <chr>, `Sort Order` <chr>,
#> #   Required <chr>, Origin <chr>, `Value Level Metadata` <chr>,
#> #   `Controlled Term or Format` <chr>, `Computational Method` <chr>,
#> #   `Definition / Comments` <chr>, `Additional Information` <chr>
#> 
#> $Value_level_Metadata
#> # A tibble: 0 × 13
#> # ℹ 13 variables: VLM Name <chr>, Therapeutic Area <chr>, Indication <chr>,
#> #   Parameter Code <chr>, Parameter <chr>, Type <chr>, Max Length <chr>,
#> #   Signficant Digits <chr>, Origin <chr>, Controlled Term <chr>,
#> #   Computational Method <chr>, Mandatory <chr>, Definition/Comments <chr>
#> 
#> $Controlled_Terms
#> # A tibble: 6 × 8
#>   `Codelist Name` `Codelist Code` `Coded Value`           Description / Decode…¹
#>   <chr>           <chr>           <chr>                   <chr>                 
#> 1 SEX             SEX             F                       Female                
#> 2 SEX             SEX             M                       Male                  
#> 3 SEX             SEX             U                       Unknown               
#> 4 RACE            RACE            BLACK OR AFRICAN AMERI… NA                    
#> 5 RACE            RACE            AMERICAN INDIAN OR ALA… NA                    
#> 6 RACE            RACE            ASIAN                   NA                    
#> # ℹ abbreviated name: ¹​`Description / Decoded Value`
#> # ℹ 4 more variables: `Code Info` <chr>, `Extension?` <chr>, Rank <chr>,
#> #   Source <chr>
#> 
#> $Computational_Method
#> # A tibble: 6 × 3
#>   `Computational Method Name` `Definition / Comments`     Additional Informati…¹
#>   <chr>                       <chr>                       <chr>                 
#> 1 TRTP                        Compare ADT/ASTDT to the s… NA                    
#> 2 TRTA                        Compare ADT/ASTDT to the s… NA                    
#> 3 TPERIOD                     Latest value of APERIOD fr… NA                    
#> 4 TPERADY                     Contains the number of day… NA                    
#> 5 TPERSTDY                    Contains the number of day… NA                    
#> 6 TPERENDY                    Contains the number of day… NA                    
#> # ℹ abbreviated name: ¹​`Additional Information`
```

Let’s start with making the ds_spec (dataset specification) table using
`spec_type_to_ds_spec`. The ds_spec table is made of 3 columns: the
dataset name, the dataset structure, and the dataset label. If we look
at our specification document, it looks like all this information is in
the Domains tab. Now we know what we need, we can start building the
table by trying the `spec_type_to_ds_spec` function.

This function takes in our named list of datasets (doc), a named vector
of columns (cols) and a sheet name (sheet). But, only doc is needed, the
other inputs have defaults. So we can try with just the default and see
what we get.

``` r
spec_type_to_ds_spec(doc)
#> Error in `create_tbl()`:
#> ! Unable to rename the following columns in Domains
#> ✖ label matches 2 columns: c("Label", "Description")
#> ℹ Please check your regular expression
```

The error tells us there is an issue with the label column in the
Domains table. Meaning, we need to change the default vector for the
cols input because the default regular expression isn’t specific enough.
First, let’s check the column names in the Domain tab

``` r
doc$Domains %>% names()
#>  [1] "Domain Name"    "Label"          "Repeating"      "Is Reference?" 
#>  [5] "Class"          "Source"         "Data Structure" "Key Variables" 
#>  [9] "Description"    "Documentation"
```

If we look at the default input for cols,
`"label" = "[L|l]abel|[D|d]escription"`, we can see the label is
matching to the `Label` and the `Description` columns .

We only need the Domain `Name`, `Label`, and `Data Structure` columns.
So we can update the expressions to be more specific.

``` r
ds_spec <- spec_type_to_ds_spec(doc,
  cols = c(
    "dataset" = "Name",
    "structure" = "Data Structure",
    "label" = "Label"
  )
)
head(ds_spec)
#> # A tibble: 4 × 3
#>   dataset structure                                                    label    
#>   <chr>   <chr>                                                        <chr>    
#> 1 ADDS    One record per subject per datetime per parameter per result Disposit…
#> 2 ADRACE  One record per subject per analysis sequence number          Race Ana…
#> 3 ADTRT   One record per subject per period                            Treatmen…
#> 4 ADSL    One record per subject                                       Subject-…
```

Regular expressions are used to match the columns, so if you needed a
more flexible input, you could do that. Now, we have the ds_spec table
we can move on to the ds_vars table.

The ds_vars table has 7 columns:

- dataset: dataset name

- variable: variable name

- key_seq: integers controlling the sort order of each dataset

- order: integer controlling the column order of each dataset

- mandatory: from Define-XML v2.1. A Boolean value indicating if NULL
  values are permitted. `mandatory = TRUE` indicates that NULL values
  are not permitted.

- core: ADaM core (Expected, Required, Permissible)

- supp_flag: boolean to determine if the variable is in the
  supplementals

When we look back at our specification document we can see all this
information is in the variable tab. The inputs for the
`spec_type_to_ds_vars` function are the same as before, but with
slightly different defaults. By default ds_vars only checks sheets
labeled “Variable” (this is because all the settings are defaulted to
read in P21 formatted specs). But, those default work for our
specifications cause all the information is in the variable tab; so we
can try with just the defaults again.

``` r
spec_type_to_ds_vars(doc)
#> Error in `create_tbl()`:
#> ! Unable to rename the following columns in Variables
#> ✖ variable matches 2 columns: c("Variable Name", "Variable Order")
#> ✖ order matches 2 columns: c("Variable Order", "Sort Order")
#> ℹ Please check your regular expression
```

This error means it is trying to match the sheet entitled Variable, the
variable column matches to two different columns. This is the same error
we had before. We just need to have a quick look at the columns and
adjust the regular expression to be more specific. Additionally, for the
key sequence variable isn’t in the variable tab. We saw this information
above in the domain tab. So we will need to do two things to fix this.
First, adjust the dataset name in the `key_seq_cols` argument. Second,
change the sheets to include the variable and the domain sheet.

``` r
doc$Variables %>% head()
#> # A tibble: 6 × 18
#>   `Domain Name` `Therapeutic Area` Indication `Variable Name` Label        Type 
#>   <chr>         <chr>              <chr>      <chr>           <chr>        <chr>
#> 1 ADAE          CORE               CORE       STUDYID         Study Ident… Text 
#> 2 ADAE          CORE               CORE       USUBJID         Unique Subj… Text 
#> 3 ADAE          CORE               CORE       SUBJID          Subject Ide… Text 
#> 4 ADAE          CORE               CORE       SITEID          Study Site … Text 
#> 5 ADAE          CORE               CORE       AGE             Age          Inte…
#> 6 ADAE          CORE               CORE       SEX             Sex          Text 
#> # ℹ 12 more variables: `Max Length` <chr>, `Variable Order` <chr>,
#> #   Mandatory <chr>, `Signficant Digits` <chr>, `Sort Order` <chr>,
#> #   Required <chr>, Origin <chr>, `Value Level Metadata` <chr>,
#> #   `Controlled Term or Format` <chr>, `Computational Method` <chr>,
#> #   `Definition / Comments` <chr>, `Additional Information` <chr>

ds_vars <- spec_type_to_ds_vars(doc,
  cols = c(
    "dataset" = "Domain",
    "variable" = "[V|v]ariable [N|n]ame",
    "order" = "[V|v]ariable [O|o]rder",
    "mandatory" = "[M|m]andatory"
  ),
  key_seq_cols = c(
    "dataset" = "Domain Name",
    "key_seq" = "Key"
  ),
  sheet = "[V|v]ar|Domains"
)

head(ds_vars)
#> # A tibble: 6 × 7
#>   dataset variable order mandatory key_seq core  supp_flag
#>   <chr>   <chr>    <dbl> <lgl>       <int> <chr> <lgl>    
#> 1 ADAE    STUDYID     10 TRUE           NA NA    NA       
#> 2 ADAE    USUBJID     20 TRUE           NA NA    NA       
#> 3 ADAE    SUBJID      25 NA             NA NA    NA       
#> 4 ADAE    SITEID      30 NA             NA NA    NA       
#> 5 ADAE    AGE         40 NA             NA NA    NA       
#> 6 ADAE    SEX         50 NA             NA NA    NA
```

The next table we have is var_spec, the table of variable level
metadata. var_spec is separate from ds_vars because, in accordance with
CDISC standards, labels and lengths should be the same for a given
variable across all datasets. So, we are able to normalize the data to
only have one row per variable, which ensures this rule and helps reduce
the size of the object. There are 6 columns in var_spec:

- variable: variable name

- length: variable length

- label: variable label

- type: variable Class

- format: variable format

- common: boolean if variable is common across ADaM

Looking back at our specification we see this will also be built using
the Variable tab. So, we know we need to edit the regular expression for
the variable to make it more specific. Additionally, if you look at the
default for `cols` you see there is a dataset input. This is because
some standards aren’t 100% consistent, some variables (e.g. visit) have
different lengths depending on the dataset. **So to accommodate this
some of the variables in var_spec are in the ds.variable format**. These
builders will do this conversion for you , but the dataset is needed.
The other thing the builders can automatically deal with is the common
variable. If given a dataset column, the builder function will
automatically figure out which variables are common to all dataset. This
is good because we don’t have a common variable in our specs.

``` r
var_spec <- spec_type_to_var_spec(doc, cols = c(
  "variable" = "Variable Name",
  "length" = "[L|l]ength",
  "label" = "[L|l]abel",
  "type" = "[T|t]ype",
  "dataset" = "[D|d]ataset|[D|d]omain",
  "format" = "Format"
))
head(var_spec)
#> # A tibble: 6 × 6
#>   variable length label                            type    format common
#>   <chr>     <int> <chr>                            <chr>   <chr>  <lgl> 
#> 1 STUDYID       9 Study Identifier                 Text    NA     TRUE  
#> 2 USUBJID      30 Unique Subject Identifier        Text    NA     TRUE  
#> 3 SUBJID       20 Subject Identifier for the Study Text    NA     FALSE 
#> 4 SITEID       10 Study Site Identifier            Text    NA     TRUE  
#> 5 AGE           8 Age                              Integer NA     TRUE  
#> 6 SEX           1 Sex                              Text    SEX    TRUE
```

There is one issue here: the format column is also the codelist names.
This is because the information came from the “Controlled Term or
Format” column of my spec document. So the final step of preparing
var_spec table is to remove the controlled terms. It is easy here
because all the formats end in a full stop (.), but the controlled terms
don’t.

``` r
var_spec <- var_spec %>%
  mutate(format = if_else(str_detect(format, "\\."), format, ""))
```

The next dataset is value_spec, which contains the value level metadata.
It is made up of 8 columns:

- dataset: dataset name

- variable: variable name

- origin: origin of data

- type: value type

- sig_dig: significant digits of the value

- code_id: id used to cross-reference the code/decode

- where: value of the variable

- derivation_id: id used to cross-reference the derivation

By default, `spec_type_to_value_spec` is set up to have the where
information on a different sheet because that is the format of a P21
spec, but in our spec we don’t have that. In fact, we don’t have any
value level metadata in our spec. But, that is fine - the default
builders will just pull what information it can from the variable tab.
Additionally this spec doesn’t have a predecessor column, so we can just
use the method column.

``` r
value_spec <- spec_type_to_value_spec(doc,
  cols = c(
    "dataset" = "VLM Name|Domain",
    "variable" = "VLM Name|Variable Name",
    "origin" = "[O|o]rigin",
    "type" = "[T|t]ype",
    "code_id" = "Controlled Term",
    "where" = "Parameter Code",
    "derivation_id" = "Method",
    "predecessor" = "Method"
  ),
  where_sep_sheet = FALSE
)
head(value_spec)
#> # A tibble: 6 × 8
#>   dataset variable origin      type    code_id derivation_id where sig_dig
#>   <chr>   <chr>    <chr>       <chr>   <chr>   <chr>         <chr>   <int>
#> 1 ADAE    STUDYID  Predecessor Text    NA      pred.NA       TRUE       NA
#> 2 ADAE    USUBJID  Predecessor Text    NA      pred.NA       TRUE       NA
#> 3 ADAE    SUBJID   Predecessor Text    NA      pred.NA       TRUE       NA
#> 4 ADAE    SITEID   Predecessor Text    NA      pred.NA       TRUE       NA
#> 5 ADAE    AGE      Predecessor Integer NA      pred.NA       TRUE       NA
#> 6 ADAE    SEX      Predecessor Text    SEX     pred.NA       TRUE       NA
```

The derivation table is relatively simple by comparison. It just has two
columns, the derivation id and the derivation. But, the derivation comes
from the supplied derivation, predecessor, or comment column depending
on the origin. In this mock we don’t have a predecessor column so we can
set to comment as well.

``` r
derivation <- spec_type_to_derivations(doc,
  cols = c(
    "derivation_id" = "Name",
    "derivation" = "[D|d]efinition|[D|d]escription"
  ),
  var_cols = c(
    "dataset" = "Domain Name",
    "variable" = "Variable Name|VLM",
    "origin" = "[O|o]rigin",
    "predecessor" = "Comment",
    "comment" = "Comment"
  )
)
head(derivation)
#> # A tibble: 6 × 2
#>   derivation_id derivation                                                      
#>   <chr>         <chr>                                                           
#> 1 TRTP          Compare ADT/ASTDT to the series of ADSL.APxxSDT and ADSL.APxxED…
#> 2 TRTA          Compare ADT/ASTDT to the series of ADSL.APxxSDT and ADSL.APxxED…
#> 3 TPERIOD       Latest value of APERIOD from ADTRT where ADTRT.TRSDT/ADTRT.TRSD…
#> 4 TPERADY       Contains the number of days from the first exposure date in the…
#> 5 TPERSTDY      Contains the number of days from the first exposure date in the…
#> 6 TPERENDY      Contains the number of days from the first exposure date in the…
```

The final table is codelist. This table contains all the code/decode
pairs, all lists of permitted values and information about external
libraries. What is somewhat special about the structure of this table is
there isn’t just a code and a decode column, but rather a codes column
that contains a list of code/decode tables, permitted value vectors and
external dictionary vectors. So there is one row per code (i.e. a row
for country and one for yes/no codes etc.). This structure makes it
easier to see all the codes at once and allows some code to be numeric
and others to be character.

By default the `spec_type_to_codelist` function expects codelists and
external dictionaries. But, in the specification we only have codelist
so `dict_cols` needs to be set to null.

``` r
codelist <- spec_type_to_codelist(doc,
  codelist_cols = c(
    "code_id" = "Codelist Code",
    "name" = "Codelist Name",
    "code" = "Coded Value",
    "decode" = "Decoded Value"
  ),
  simplify = TRUE,
  dict_cols = NULL
)
head(codelist)
#> # A tibble: 6 × 4
#>   code_id name    type        codes           
#>   <chr>   <chr>   <chr>       <list>          
#> 1 SEX     SEX     code_decode <tibble [3 × 2]>
#> 2 RACE    RACE    code_decode <tibble [5 × 2]>
#> 3 COUNTRY COUNTRY code_decode <tibble [2 × 2]>
#> 4 NY      NY      code_decode <tibble [2 × 2]>
#> 5 DATEFL  DATEFL  code_decode <tibble [2 × 2]>
#> 6 AESEV   AESEV   code_decode <tibble [3 × 2]>
```

Now we have all the tables we need we can make the metacore object

``` r
metacore(ds_spec, ds_vars, var_spec, value_spec,
  derivation, codelist,
  verbose = "message"
)
#> Warning: `core` from the `ds_vars` table only contains missing values.
#> Warning: `supp_flag` from the `ds_vars` table only contains missing
#> values.
#> Warning: `sig_dig` from the `value_spec` table only contains missing
#> values.
#> Warning: `dataset` from the `supp` table only contains missing values.
#> Warning: `variable` from the `supp` table only contains missing values.
#> Warning: `idvar` from the `supp` table only contains missing values.
#> Warning: `qeval` from the `supp` table only contains missing values.
#> Warning: The following variables have derivation ids not found in the derivations table:
#> ℹ ASTDT, ASTTM, ASTDTM, AENDT, AENTM, AENDTM, AFTRTSTC, ALTRTSTC, APFTRSTC,
#>   ASTDT, AFTRTST, AFTRTSTC, ALTRTST, ALTRTSTC, ASTDT, ASTTM, ASTDTM, AENDT,
#>   AENTM, AENDTM, CMBASE, CMBASECD, DOSCUMA, AVISIT, ATPT, ADT, ATM, ADTM,
#>   APERADY, APERSTDY, APERENDY, ADT, ATM, ADTM, ADT, ATM, ADTM
#> Warning: The following derivations are never used:
#> ℹ pred.AE.STUDYID, pred.AE.USUBJID, pred.AE.SUBJID, pred.ADSL.SITEID,
#>   pred.ADSL.AGE, pred.ADSL.SEX, pred.ADSL.RACE, pred.ADSL.ACOUNTRY,
#>   pred.ADSL.FASFL, pred.ADSL.ITTFL, pred.ADSL.SAFFL, pred.ADSL.PPROTFL,
#>   pred.ADSL.TRTxxPN that corresponds to the treatment selected in TRTP,
#>   pred.ADSL.TRTxxAN that corresponds to the treatment selected in TRTA,
#>   pred.ADSL.TRT01A, pred.ADSL.TRT01AN, pred.ADSL.TRT01P, pred.ADSL.TRT01PN,
#>   pred.ADSL.TRTSEQP, pred.ADSL.TRTSEQPN, pred.ADSL.TRTSEQA, pred.ADSL.TRTSEQAN,
#>   pred.AE.AESTDY, pred.AE.AESTDTC, pred.AE.AEENDTC, pred.AE.AEENDY,
#>   pred.AE.AESEQ, pred.AE.AETERM, pred.AE.AEMODIFY, pred.AE.AEDECOD,
#>   pred.AE.AEBODSYS, pred.AE.AEBDSYCD, pred.AE.AELLT, pred.AE.AELLTCD,
#>   pred.AE.AEPTCD, pred.AE.AEHLT, pred.AE.AEHLTCD, pred.AE.AEHLGT,
#>   pred.AE.AEHLGTCD, pred.AE.AESOC, pred.AE.AESOCCD, pred.AE.AEDUR,
#>   pred.AE.AESEV, pred.AE.AESEV or as per RAP, pred.AE.AEACN, pred.SUPPAE.QVAL
#>   where SUPPAE.QNAM="ACT1", pred.SUPPAE.QVAL where QNAM="AETRTEM",
#>   pred.AE.AEREFID, pred.AE.AEACNOTH, pred.AE.AESER, pred.AE.AESCAN,
#>   pred.AE.AESCONG, pred.AE.AESDISAB, pred.AE.AESDTH, pred.AE.AESHOSP,
#>   pred.AE.AESLIFE, pred.AE.AESOD, pred.AE.AESMIE, pred.SUPPAE.QVAL where
#>   QNAM="AESPROT", pred.AE.AEOUT, pred.AE.AEREL, pred.AE.AETOXGR, pred.Numeric
#>   version of AETOXGR, pred.SUPPAE.QVAL where SUPPAE.QNAM="GRADED",
#>   pred.SUPPAE.QVAL where SUPPAE.QNAM="AEDLT", pred.SUPPAE.QVAL where
#>   SUPPAE.QNAM="AERDG1", pred.xx.STUDYID, pred.xx.USUBJID, pred.ADSL.SUBJID,
#>   pred.ADSL.AGEU, pred.ADSL.SAFFL if needed, pred.ADSL.FASFL if needed,
#>   pred.ADSL.ITTFL if needed, pred.ADSL.ITTEFL if required, pred.ADSL.PPROTFL if
#>   needed, pred.ADSL.TRTSDT, pred.ADSL.TRTEDT, pred.ADSL.TR01SDT,
#>   pred.ADSL.TR01EDT, pred.CE.CESEQ, pred.CE.CEREFID, pred.CE.CETERM,
#>   pred.Study-specific definition, pred.CE.CNDPREV, pred.CE.CESTDTC,
#>   pred.ADSL.DTHFL, pred.ADSL.DTHFN, pred.ADSL.DTHDT, pred.Cause of Death.,
#>   pred.CM.STUDYID, pred.CM.USUBJID, pred.CM.SUBJID, pred.CM.CMSTDTC,
#>   pred.CM.CMSTRF, pred.CM.CMENDTC, pred.CM.CMENRF, pred.CM.CMSEQ,
#>   pred.CM.CMREFID, pred.CM.CMTRT, pred.CM.CMCAT, pred.CM.CMSCAT,
#>   pred.CM.CMINDC, pred.CM.CMDOSE, pred.CM.CMDOSTXT, pred.CM.CMDOSU,
#>   pred.CM.CMDOSFRQ, pred.CM.CMROUTE, pred.CM.CMPRESP, pred.CM.CMCLAS,
#>   pred.CM.CMCLASCD, pred.GSKDRUG.CMATC1 where GSKDRUG.CMDRGCOL = CM.DRGCOLCD
#>   and GSKDRUG.CMNC = 'C', pred.GSKDRUG.CMATC2 where GSKDRUG.CMDRGCOL =
#>   CM.DRGCOLCD and GSKDRUG.CMNC = 'C', pred.GSKDRUG.CMATC3 where
#>   GSKDRUG.CMDRGCOL = CM.DRGCOLCD and GSKDRUG.CMNC = 'C', pred.GSKDRUG.CMATC4
#>   where GSKDRUG.CMDRGCOL = CM.DRGCOLCD and GSKDRUG.CMNC = 'C', pred.CM.CMDUR,
#>   pred.CM.CMOCCUR, pred.CM.CMDOSFRM, pred.DA.STUDYID, pred.DA.USUBJID,
#>   pred.DA.SUBJID, pred.DA.VISITNUM, pred.DA.VISIT, pred.DA.DACAT,
#>   pred.DA.DASCAT, pred.DA.DASEQ, pred.DA.DADTC, pred.DA.DADY, pred.DA.DATEST
#>   and/or derived. Example contains "Compliance %", pred.DA.DATESTCD and/or
#>   derived, pred.ADSL.STUDYID, pred.ADSL.USUBJID, pred.FAAE.SUBJID,
#>   pred.FAAE.FATPTREF, pred.FAAE.FATPTNUM, pred.FAAE.FATPT, pred.DD.STUDYID,
#>   pred.DD.USUBJID, pred.ADSL.ACTARM, pred.ADSL.ENRLFL, pred.ADSL.AP01SDT,
#>   pred.ADSL.AP01SDTM, pred.ADSL.AP01EDT, pred.ADSL.AP01EDTM, pred.DD.DDTPTREF,
#>   pred.DD.DDTPTNUM, pred.DD.DDTPT, pred.DS.STUDYID, pred.DS.USUBJID,
#>   pred.DS.SUBJID, pred.DS.DSSTDTC, pred.DS.DSSEQ, pred.DM.STUDYID,
#>   pred.DM.USUBJID, pred.DQ.SUBJID, pred.SUPPDM.QVAL when QNAM=RACEOR or
#>   QNAM=RACEORx where x=1,2, ..., n., pred.ADSL.ARACE, pred.ADSL.ARACEN,
#>   pred.ADSL.INVID, pred.ADSL.INVNAM, pred.ADSL.COUNTRY, pred.ADSL.AGEGR1,
#>   pred.ADSL.AGEGR1N, pred.ADSL.RACEN, pred.ADSL.ETHNIC, pred.ADSL.ETHNICN,
#>   pred.ADSL.TOTFL, pred.ADSL.TOTFN, pred.ADSL.ITTFN, pred.ADSL.PPROTFN,
#>   pred.ADSL.COMPLFL, pred.ADSL.COMPLFN, pred.ADSL.RUNFFL, pred.ADSL.RUNFFN,
#>   pred.ADSL.SCRFFL, pred.ADSL.SCRFFN, pred.ADSL.ARM, pred.ADSL.ARMCD,
#>   pred.ADSL.TRTxxP, where xx is the value for APERIOD, pred.ADSL.TRTxxA, where
#>   xx is the value for APERIOD, pred.ADSL.RANDDT, pred.ADSL.RFSTDT,
#>   pred.ADSL.RFENDT, pred.ADSL.APxxSDT, where xx is the value for APERIOD,
#>   pred.ADSL.APxxEDT, where xx is the value for APERIOD
#> Warning: The following variables have code ids not found in the codelist(s):
#> ℹ ADAE.ASTDT, ADAE.ASTTM, ADAE.ASTDTM, ADAE.AENDT, ADAE.AENTM, ADAE.AENDTM,
#>   ADAE.AESTDTC, ADAE.AEENDTC, ADAE.AEDECOD, ADAE.AEBODSYS, ADAE.AEBDSYCD,
#>   ADAE.AELLT, ADAE.AELLTCD, ADAE.AEPTCD, ADAE.AEHLT, ADAE.AEHLTCD, ADAE.AEHLGT,
#>   ADAE.AEHLGTCD, ADAE.AESOC, ADAE.AESOCCD, ADAE.AEDUR, ADAE.ASEVN, ADAE.AEOUTN,
#>   ADAE.AETOXGR, ADAE.AETOXGRN, ADCE.TRTSDT, ADCE.TRTEDT, ADCE.TR01SDT,
#>   ADCE.TR01EDT, ADCE.ASTDT, ADCE.DTHDT, ADCM.TPERIOD, ADCM.TPERIODC,
#>   ADCM.ASTDT, ADCM.ASTTM, ADCM.ASTDTM, ADCM.AENDT, ADCM.AENTM, ADCM.AENDTM,
#>   ADCM.CMDUR, ADCOMP.ADT, ADCOMP.ATM, ADCOMP.ADTM, ADCOMP.PARAMCD, ADCOVID.ADT,
#>   ADCOVID.ATM, ADCOVID.ADTM, ADCOVID.AENDT, ADCOVID.ASTDT, ADDD.AP01SDT,
#>   ADDD.AP01SDTM, ADDD.AP01EDT, ADDD.AP01EDTM, ADDD.ADT, ADDS.ADT, ADDS.ATM,
#>   ADDS.ADTM, ADDS.PARCAT1, ADDS.PARCAT1N, ADDS.PARAM, ADDS.PARAMCD,
#>   ADRACE.RACEORCD, ADRACE.ARACE, ADTRT.ETHNIC
#> ✔ Metadata successfully imported
#> ℹ To use the Metacore object with metatools package, first subset a dataset
#>   using `metacore::select_dataset()`
#> ── Metacore object contains metadata for 4 datasets ────────────────────────────
#> 
#> → ADDS (Disposition Analysis)
#> 
#> → ADRACE (Race Analysis)
#> 
#> → ADTRT (Treatment Analysis)
#> 
#> → ADSL (Subject-Level Analysis Dataset)
#> 
#> 
#> 
#> To use the Metacore object with metatools package, first subset a dataset using
#> `metacore::select_dataset()`
```

And we’re good to go!
