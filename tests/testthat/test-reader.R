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


test_that("Test ds_vars readers", {
   # Create a reference ds_vars
   ref_ds_vars <- tribble(
      ~dataset,  ~variable, ~key_seq, ~order, ~keep, ~core,
      "AE",    "AEACN",       NA,    21L, FALSE,    NA_character_,
      "AE", "AEBDSYCD",       NA,    16L, FALSE,    NA_character_,
      "AE", "AEBODSYS",       NA,    15L, FALSE,    NA_character_,
      "AE",  "AEDECOD",       NA,     9L, FALSE,    NA_character_,
      "AE",    "AEDTC",       NA,    32L, FALSE,    NA_character_,
      "AE",     "AEDY",       NA,    35L, FALSE,    NA_character_,
      "AE",  "AEENDTC",       NA,    34L, FALSE,    NA_character_,
      "AE",   "AEENDY",       NA,    37L, FALSE,    NA_character_,
      "AE",   "AEHLGT",       NA,    13L, FALSE,    NA_character_,
      "AE", "AEHLGTCD",       NA,    14L, FALSE,    NA_character_,
      "AE",    "AEHLT",       NA,    11L, FALSE,    NA_character_,
      "AE",  "AEHLTCD",       NA,    12L, FALSE,    NA_character_,
      "AE",    "AELLT",       NA,     7L, FALSE,    NA_character_,
      "AE",  "AELLTCD",       NA,     8L, FALSE,    NA_character_,
      "AE",    "AEOUT",       NA,    23L, FALSE,    NA_character_,
      "AE",   "AEPTCD",       NA,    10L, FALSE,    NA_character_,
      "AE",    "AEREL",       NA,    22L, FALSE,    NA_character_,
      "AE",   "AESCAN",       NA,    24L, FALSE,    NA_character_,
      "AE",  "AESCONG",       NA,    25L, FALSE,    NA_character_,
      "AE", "AESDISAB",       NA,    26L, FALSE,    NA_character_,
      "AE",   "AESDTH",       NA,    27L, FALSE,    NA_character_,
      "AE",    "AESEQ",       5L,     4L,  TRUE,    NA_character_,
      "AE",    "AESER",       NA,    20L, FALSE,    NA_character_,
      "AE",    "AESEV",       NA,    19L, FALSE,    NA_character_,
      "AE",  "AESHOSP",       NA,    28L, FALSE,    NA_character_,
      "AE",  "AESLIFE",       NA,    29L, FALSE,    NA_character_,
      "AE",    "AESOC",       NA,    17L, FALSE,    NA_character_,
      "AE",  "AESOCCD",       NA,    18L, FALSE,    NA_character_,
      "AE",    "AESOD",       NA,    30L, FALSE,    NA_character_,
      "AE",   "AESPID",       NA,     5L, FALSE,    NA_character_,
      "AE",  "AESTDTC",       4L,    33L, FALSE,    NA_character_,
      "AE",   "AESTDY",       NA,    36L, FALSE,    NA_character_,
      "AE",   "AETERM",       3L,     6L,  TRUE,    NA_character_,
      "AE",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_,
      "AE",    "EPOCH",       NA,    31L, FALSE,    NA_character_,
      "AE",  "STUDYID",       1L,     1L,  TRUE,    NA_character_,
      "AE",  "USUBJID",       2L,     3L,  TRUE,    NA_character_,
      "DM",   "ACTARM",       NA,    22L,  TRUE,    NA_character_,
      "DM", "ACTARMCD",       NA,    21L,  TRUE,    NA_character_,
      "DM",      "AGE",       NA,    14L, FALSE,    NA_character_,
      "DM",     "AGEU",       NA,    15L, FALSE,    NA_character_,
      "DM",      "ARM",       NA,    20L,  TRUE,    NA_character_,
      "DM",    "ARMCD",       NA,    19L,  TRUE,    NA_character_,
      "DM",  "COUNTRY",       NA,    23L,  TRUE,    NA_character_,
      "DM",    "DMDTC",       NA,    24L, FALSE,    NA_character_,
      "DM",     "DMDY",       NA,    25L, FALSE,    NA_character_,
      "DM",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_,
      "DM",   "DTHDTC",       NA,    11L, FALSE,    NA_character_,
      "DM",    "DTHFL",       NA,    12L, FALSE,    NA_character_,
      "DM",   "ETHNIC",       NA,    18L, FALSE,    NA_character_,
      "DM",     "RACE",       NA,    17L, FALSE,    NA_character_,
      "DM",  "RFENDTC",       NA,     6L, FALSE,    NA_character_,
      "DM",  "RFICDTC",       NA,     9L, FALSE,    NA_character_,
      "DM", "RFPENDTC",       NA,    10L, FALSE,    NA_character_,
      "DM",  "RFSTDTC",       NA,     5L, FALSE,    NA_character_,
      "DM", "RFXENDTC",       NA,     8L, FALSE,    NA_character_,
      "DM", "RFXSTDTC",       NA,     7L, FALSE,    NA_character_,
      "DM",      "SEX",       NA,    16L,  TRUE,    NA_character_,
      "DM",   "SITEID",       NA,    13L,  TRUE,    NA_character_,
      "DM",  "STUDYID",       1L,     1L,  TRUE,    NA_character_,
      "DM",   "SUBJID",       NA,     4L,  TRUE,    NA_character_,
      "DM",  "USUBJID",       2L,     3L,  TRUE,    NA_character_,
      "EX",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_,
      "EX",    "EPOCH",       NA,    14L, FALSE,    NA_character_,
      "EX",   "EXDOSE",       NA,     6L, FALSE,    NA_character_,
      "EX", "EXDOSFRM",       NA,     8L, FALSE,    NA_character_,
      "EX", "EXDOSFRQ",       NA,     9L, FALSE,    NA_character_,
      "EX",   "EXDOSU",       NA,     7L, FALSE,    NA_character_,
      "EX",  "EXENDTC",       NA,    16L, FALSE,    NA_character_,
      "EX",   "EXENDY",       NA,    18L, FALSE,    NA_character_,
      "EX",  "EXROUTE",       NA,    10L, FALSE,    NA_character_,
      "EX",    "EXSEQ",       NA,     4L,  TRUE,    NA_character_,
      "EX",  "EXSTDTC",       4L,    15L, FALSE,    NA_character_,
      "EX",   "EXSTDY",       NA,    17L, FALSE,    NA_character_,
      "EX",    "EXTRT",       3L,     5L,  TRUE,    NA_character_,
      "EX",  "STUDYID",       1L,     1L,  TRUE,    NA_character_,
      "EX",  "USUBJID",       2L,     3L,  TRUE,    NA_character_,
      "EX",    "VISIT",       NA,    12L, FALSE,    NA_character_,
      "EX",  "VISITDY",       NA,    13L, FALSE,    NA_character_,
      "EX", "VISITNUM",       NA,    11L, FALSE,    NA_character_,
      "SUPPAE",    "IDVAR",       4L,     4L, FALSE,    NA_character_,
      "SUPPAE", "IDVARVAL",       5L,     5L, FALSE,    NA_character_,
      "SUPPAE",    "QEVAL",       NA,    10L, FALSE,    NA_character_,
      "SUPPAE",   "QLABEL",       NA,     7L,  TRUE,    NA_character_,
      "SUPPAE",     "QNAM",       6L,     6L,  TRUE,    NA_character_,
      "SUPPAE",    "QORIG",       NA,     9L,  TRUE,    NA_character_,
      "SUPPAE",     "QVAL",       NA,     8L,  TRUE,    NA_character_,
      "SUPPAE",  "RDOMAIN",       2L,     2L,  TRUE,    NA_character_,
      "SUPPAE",  "STUDYID",       1L,     1L,  TRUE,    NA_character_,
      "SUPPAE",  "USUBJID",       3L,     3L,  TRUE,    NA_character_,
      "SUPPDM",    "IDVAR",       4L,     4L, FALSE,    NA_character_,
      "SUPPDM", "IDVARVAL",       5L,     5L, FALSE,    NA_character_,
      "SUPPDM",    "QEVAL",       NA,    10L, FALSE,    NA_character_,
      "SUPPDM",   "QLABEL",       NA,     7L,  TRUE,    NA_character_,
      "SUPPDM",     "QNAM",       6L,     6L,  TRUE,    NA_character_,
      "SUPPDM",    "QORIG",       NA,     9L,  TRUE,    NA_character_,
      "SUPPDM",     "QVAL",       NA,     8L,  TRUE,    NA_character_,
      "SUPPDM",  "RDOMAIN",       2L,     2L,  TRUE,    NA_character_,
      "SUPPDM",  "STUDYID",       1L,     1L,  TRUE,    NA_character_,
      "SUPPDM",  "USUBJID",       3L,     3L,  TRUE,    NA_character_
   )

   # Read from define
   def_ds_vars <- xml_to_ds_vars(define) %>%
      arrange(dataset, variable)

   # Read from spec
   spec_ds_vars <- spec_type_to_ds_vars(spec) %>%
      arrange(dataset, variable) %>%
      select(dataset, variable, key_seq, order, keep, core)

   test <- spec_ds_vars %>%
      select(dataset, variable, spec_ord = order) %>%
      full_join(select(ref_ds_vars, dataset, variable, order),
                by = c("dataset", "variable")) %>%
      filter( order != spec_ord)

   # Tests
   expect_equal(def_ds_vars, ref_ds_vars)
   expect_equal(spec_ds_vars,
                ref_ds_vars)

})


# #
# # test_that()
#

# ds_vars <- xml_to_ds_vars(define)
# var_spec <- xml_to_var_spec(doc)
# value_spec <- xml_to_value_spec(doc)
# code_list <- xml_to_code_list(doc)
# derivations <- xml_to_derivations(doc)
# datapasta::dpasta(def_ds_vars)
# xml_to_ds_vars(define) %>%
#    arrange(dataset, variable) %>%
#    datapasta::dpasta()



