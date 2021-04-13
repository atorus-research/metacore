# Read in doc to be used for testing
define <- xmlTreeParse("define-2021.xml", useInternalNodes = TRUE)
spec <- read_all_sheets("p21_mock.xlsx")


test_that("Test ds_spec readers", {
   # Create a reference spec to match to
   ref_ds_spec <-
      tribble(~dataset, ~structure, ~label,
              "DM"      ,"One record per subject", "Demographics",
              "EX"      ,"One record per constant dosing interval per subject", "Exposure",
              "AE"      ,"One record per adverse event per subject", "Adverse Events",
              "SUPPAE"  ,"One record per IDVAR, IDVARVAL, and QNAM value per subject", "Supplemental Qualifiers for AE",
              "SUPPDM"  ,"One record per IDVAR, IDVARVAL, and QNAM value per subject", "Supplemental Qualifiers for DM")
   # Read from define
   def_ds_spec <- xml_to_ds_spec(define)

   # Read from spec
   spec_ds_spec <- spec_type_to_ds_spec(spec)

   # Test
   expect_equal(def_ds_spec, ref_ds_spec)
   expect_equal(spec_ds_spec, arrange(ref_ds_spec, dataset))
})


test_that("Test ds_vars readers"{
   # Create a reference ds_vars
   ref_ds_vars <- tribble(
      ~dataset,  ~variable, ~key_seq, ~keep, ~core,
      "DM",  "STUDYID",           1L,  TRUE,    NA,
      "DM",   "DOMAIN",           NA,  TRUE,    NA,
      "DM",  "USUBJID",           2L,  TRUE,    NA,
      "DM",   "SUBJID",           NA,  TRUE,    NA,
      "DM",  "RFSTDTC",           NA, FALSE,    NA,
      "DM",  "RFENDTC",           NA, FALSE,    NA,
      "DM", "RFXSTDTC",           NA, FALSE,    NA,
      "DM", "RFXENDTC",           NA, FALSE,    NA,
      "DM",  "RFICDTC",           NA, FALSE,    NA,
      "DM", "RFPENDTC",           NA, FALSE,    NA,
      "DM",   "DTHDTC",           NA, FALSE,    NA,
      "DM",    "DTHFL",           NA, FALSE,    NA,
      "DM",   "SITEID",           NA,  TRUE,    NA,
      "DM",      "AGE",           NA, FALSE,    NA,
      "DM",     "AGEU",           NA, FALSE,    NA,
      "DM",      "SEX",           NA,  TRUE,    NA,
      "DM",     "RACE",           NA, FALSE,    NA,
      "DM",   "ETHNIC",           NA, FALSE,    NA,
      "DM",    "ARMCD",           NA,  TRUE,    NA,
      "DM",      "ARM",           NA,  TRUE,    NA,
      "DM", "ACTARMCD",           NA,  TRUE,    NA,
      "DM",   "ACTARM",           NA,  TRUE,    NA,
      "DM",  "COUNTRY",           NA,  TRUE,    NA,
      "DM",    "DMDTC",           NA, FALSE,    NA,
      "DM",     "DMDY",           NA, FALSE,    NA,
      "EX",  "STUDYID",           1L,  TRUE,    NA,
      "EX",   "DOMAIN",           NA,  TRUE,    NA,
      "EX",  "USUBJID",           2L,  TRUE,    NA,
      "EX",    "EXSEQ",           NA,  TRUE,    NA,
      "EX",    "EXTRT",           3L,  TRUE,    NA,
      "EX",   "EXDOSE",           NA, FALSE,    NA,
      "EX",   "EXDOSU",           NA, FALSE,    NA,
      "EX", "EXDOSFRM",           NA, FALSE,    NA,
      "EX", "EXDOSFRQ",           NA, FALSE,    NA,
      "EX",  "EXROUTE",           NA, FALSE,    NA,
      "EX", "VISITNUM",           NA, FALSE,    NA,
      "EX",    "VISIT",           NA, FALSE,    NA,
      "EX",  "VISITDY",           NA, FALSE,    NA,
      "EX",    "EPOCH",           NA, FALSE,    NA,
      "EX",  "EXSTDTC",           4L, FALSE,    NA,
      "EX",  "EXENDTC",           NA, FALSE,    NA,
      "EX",   "EXSTDY",           NA, FALSE,    NA,
      "EX",   "EXENDY",           NA, FALSE,    NA,
      "AE",  "STUDYID",           1L,  TRUE,    NA,
      "AE",   "DOMAIN",           NA,  TRUE,    NA,
      "AE",  "USUBJID",           2L,  TRUE,    NA,
      "AE",    "AESEQ",           5L,  TRUE,    NA,
      "AE",   "AESPID",           NA, FALSE,    NA,
      "AE",   "AETERM",           3L,  TRUE,    NA,
      "AE",    "AELLT",           NA, FALSE,    NA,
      "AE",  "AELLTCD",           NA, FALSE,    NA,
      "AE",  "AEDECOD",           NA, FALSE,    NA,
      "AE",   "AEPTCD",           NA, FALSE,    NA,
      "AE",    "AEHLT",           NA, FALSE,    NA,
      "AE",  "AEHLTCD",           NA, FALSE,    NA,
      "AE",   "AEHLGT",           NA, FALSE,    NA,
      "AE", "AEHLGTCD",           NA, FALSE,    NA,
      "AE", "AEBODSYS",           NA, FALSE,    NA,
      "AE", "AEBDSYCD",           NA, FALSE,    NA,
      "AE",    "AESOC",           NA, FALSE,    NA,
      "AE",  "AESOCCD",           NA, FALSE,    NA,
      "AE",    "AESEV",           NA, FALSE,    NA,
      "AE",    "AESER",           NA, FALSE,    NA,
      "AE",    "AEACN",           NA, FALSE,    NA,
      "AE",    "AEREL",           NA, FALSE,    NA,
      "AE",    "AEOUT",           NA, FALSE,    NA,
      "AE",   "AESCAN",           NA, FALSE,    NA,
      "AE",  "AESCONG",           NA, FALSE,    NA,
      "AE", "AESDISAB",           NA, FALSE,    NA,
      "AE",   "AESDTH",           NA, FALSE,    NA,
      "AE",  "AESHOSP",           NA, FALSE,    NA,
      "AE",  "AESLIFE",           NA, FALSE,    NA,
      "AE",    "AESOD",           NA, FALSE,    NA,
      "AE",    "EPOCH",           NA, FALSE,    NA,
      "AE",    "AEDTC",           NA, FALSE,    NA,
      "AE",  "AESTDTC",           4L, FALSE,    NA,
      "AE",  "AEENDTC",           NA, FALSE,    NA,
      "AE",     "AEDY",           NA, FALSE,    NA,
      "AE",   "AESTDY",           NA, FALSE,    NA,
      "AE",   "AEENDY",           NA, FALSE,    NA,
      "SUPPAE",  "STUDYID",       1L,  TRUE,    NA,
      "SUPPAE",  "RDOMAIN",       2L,  TRUE,    NA,
      "SUPPAE",  "USUBJID",       3L,  TRUE,    NA,
      "SUPPAE",    "IDVAR",       4L, FALSE,    NA,
      "SUPPAE", "IDVARVAL",       5L, FALSE,    NA,
      "SUPPAE",     "QNAM",       6L,  TRUE,    NA,
      "SUPPAE",   "QLABEL",       NA,  TRUE,    NA,
      "SUPPAE",     "QVAL",       NA,  TRUE,    NA,
      "SUPPAE",    "QORIG",       NA,  TRUE,    NA,
      "SUPPAE",    "QEVAL",       NA, FALSE,    NA,
      "SUPPDM",  "STUDYID",       1L,  TRUE,    NA,
      "SUPPDM",  "RDOMAIN",       2L,  TRUE,    NA,
      "SUPPDM",  "USUBJID",       3L,  TRUE,    NA,
      "SUPPDM",    "IDVAR",       4L, FALSE,    NA,
      "SUPPDM", "IDVARVAL",       5L, FALSE,    NA,
      "SUPPDM",     "QNAM",       6L,  TRUE,    NA,
      "SUPPDM",   "QLABEL",       NA,  TRUE,    NA,
      "SUPPDM",     "QVAL",       NA,  TRUE,    NA,
      "SUPPDM",    "QORIG",       NA,  TRUE,    NA,
      "SUPPDM",    "QEVAL",       NA, FALSE,    NA)

   # Read from define
   def_ds_vars <- xml_to_ds_vars(define)
   # Read from spec
   spec_ds_vars <- spec_type_to_ds_vars(spec)

   # Tests
   expect_equal(def_ds_vars, ref_ds_vars)
   expect_equal(arrange(spec_ds_vars, dataset, variable),
                arrange(ref_ds_vars, dataset, variable))

})


#
# test_that()
#
# ds_vars <- xml_to_ds_vars(doc)
# var_spec <- xml_to_var_spec(doc)
# value_spec <- xml_to_value_spec(doc)
# code_list <- xml_to_code_list(doc)
# derivations <- xml_to_derivations(doc)
datapasta::dpasta(def_ds_vars)


