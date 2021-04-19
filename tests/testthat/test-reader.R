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
   ref_ds_vars <- tibble::tribble(
      ~dataset,  ~variable, ~key_seq, ~keep, ~core,
      "AE",    "AEACN",       NA, FALSE,    NA_character_,
      "AE", "AEBDSYCD",       NA, FALSE,    NA_character_,
      "AE", "AEBODSYS",       NA, FALSE,    NA_character_,
      "AE",  "AEDECOD",       NA, FALSE,    NA_character_,
      "AE",    "AEDTC",       NA, FALSE,    NA_character_,
      "AE",     "AEDY",       NA, FALSE,    NA_character_,
      "AE",  "AEENDTC",       NA, FALSE,    NA_character_,
      "AE",   "AEENDY",       NA, FALSE,    NA_character_,
      "AE",   "AEHLGT",       NA, FALSE,    NA_character_,
      "AE", "AEHLGTCD",       NA, FALSE,    NA_character_,
      "AE",    "AEHLT",       NA, FALSE,    NA_character_,
      "AE",  "AEHLTCD",       NA, FALSE,    NA_character_,
      "AE",    "AELLT",       NA, FALSE,    NA_character_,
      "AE",  "AELLTCD",       NA, FALSE,    NA_character_,
      "AE",    "AEOUT",       NA, FALSE,    NA_character_,
      "AE",   "AEPTCD",       NA, FALSE,    NA_character_,
      "AE",    "AEREL",       NA, FALSE,    NA_character_,
      "AE",   "AESCAN",       NA, FALSE,    NA_character_,
      "AE",  "AESCONG",       NA, FALSE,    NA_character_,
      "AE", "AESDISAB",       NA, FALSE,    NA_character_,
      "AE",   "AESDTH",       NA, FALSE,    NA_character_,
      "AE",    "AESEQ",       5L,  TRUE,    NA_character_,
      "AE",    "AESER",       NA, FALSE,    NA_character_,
      "AE",    "AESEV",       NA, FALSE,    NA_character_,
      "AE",  "AESHOSP",       NA, FALSE,    NA_character_,
      "AE",  "AESLIFE",       NA, FALSE,    NA_character_,
      "AE",    "AESOC",       NA, FALSE,    NA_character_,
      "AE",  "AESOCCD",       NA, FALSE,    NA_character_,
      "AE",    "AESOD",       NA, FALSE,    NA_character_,
      "AE",   "AESPID",       NA, FALSE,    NA_character_,
      "AE",  "AESTDTC",       4L, FALSE,    NA_character_,
      "AE",   "AESTDY",       NA, FALSE,    NA_character_,
      "AE",   "AETERM",       3L,  TRUE,    NA_character_,
      "AE",   "DOMAIN",       NA,  TRUE,    NA_character_,
      "AE",    "EPOCH",       NA, FALSE,    NA_character_,
      "AE",  "STUDYID",       1L,  TRUE,    NA_character_,
      "AE",  "USUBJID",       2L,  TRUE,    NA_character_,
      "DM",   "ACTARM",       NA,  TRUE,    NA_character_,
      "DM", "ACTARMCD",       NA,  TRUE,    NA_character_,
      "DM",      "AGE",       NA, FALSE,    NA_character_,
      "DM",     "AGEU",       NA, FALSE,    NA_character_,
      "DM",      "ARM",       NA,  TRUE,    NA_character_,
      "DM",    "ARMCD",       NA,  TRUE,    NA_character_,
      "DM",  "COUNTRY",       NA,  TRUE,    NA_character_,
      "DM",    "DMDTC",       NA, FALSE,    NA_character_,
      "DM",     "DMDY",       NA, FALSE,    NA_character_,
      "DM",   "DOMAIN",       NA,  TRUE,    NA_character_,
      "DM",   "DTHDTC",       NA, FALSE,    NA_character_,
      "DM",    "DTHFL",       NA, FALSE,    NA_character_,
      "DM",   "ETHNIC",       NA, FALSE,    NA_character_,
      "DM",     "RACE",       NA, FALSE,    NA_character_,
      "DM",  "RFENDTC",       NA, FALSE,    NA_character_,
      "DM",  "RFICDTC",       NA, FALSE,    NA_character_,
      "DM", "RFPENDTC",       NA, FALSE,    NA_character_,
      "DM",  "RFSTDTC",       NA, FALSE,    NA_character_,
      "DM", "RFXENDTC",       NA, FALSE,    NA_character_,
      "DM", "RFXSTDTC",       NA, FALSE,    NA_character_,
      "DM",      "SEX",       NA,  TRUE,    NA_character_,
      "DM",   "SITEID",       NA,  TRUE,    NA_character_,
      "DM",  "STUDYID",       1L,  TRUE,    NA_character_,
      "DM",   "SUBJID",       NA,  TRUE,    NA_character_,
      "DM",  "USUBJID",       2L,  TRUE,    NA_character_,
      "EX",   "DOMAIN",       NA,  TRUE,    NA_character_,
      "EX",    "EPOCH",       NA, FALSE,    NA_character_,
      "EX",   "EXDOSE",       NA, FALSE,    NA_character_,
      "EX", "EXDOSFRM",       NA, FALSE,    NA_character_,
      "EX", "EXDOSFRQ",       NA, FALSE,    NA_character_,
      "EX",   "EXDOSU",       NA, FALSE,    NA_character_,
      "EX",  "EXENDTC",       NA, FALSE,    NA_character_,
      "EX",   "EXENDY",       NA, FALSE,    NA_character_,
      "EX",  "EXROUTE",       NA, FALSE,    NA_character_,
      "EX",    "EXSEQ",       NA,  TRUE,    NA_character_,
      "EX",  "EXSTDTC",       4L, FALSE,    NA_character_,
      "EX",   "EXSTDY",       NA, FALSE,    NA_character_,
      "EX",    "EXTRT",       3L,  TRUE,    NA_character_,
      "EX",  "STUDYID",       1L,  TRUE,    NA_character_,
      "EX",  "USUBJID",       2L,  TRUE,    NA_character_,
      "EX",    "VISIT",       NA, FALSE,    NA_character_,
      "EX",  "VISITDY",       NA, FALSE,    NA_character_,
      "EX", "VISITNUM",       NA, FALSE,    NA_character_,
      "SUPPAE",    "IDVAR",       4L, FALSE,    NA_character_,
      "SUPPAE", "IDVARVAL",       5L, FALSE,    NA_character_,
      "SUPPAE",    "QEVAL",       NA, FALSE,    NA_character_,
      "SUPPAE",   "QLABEL",       NA,  TRUE,    NA_character_,
      "SUPPAE",     "QNAM",       6L,  TRUE,    NA_character_,
      "SUPPAE",    "QORIG",       NA,  TRUE,    NA_character_,
      "SUPPAE",     "QVAL",       NA,  TRUE,    NA_character_,
      "SUPPAE",  "RDOMAIN",       2L,  TRUE,    NA_character_,
      "SUPPAE",  "STUDYID",       1L,  TRUE,    NA_character_,
      "SUPPAE",  "USUBJID",       3L,  TRUE,    NA_character_,
      "SUPPDM",    "IDVAR",       4L, FALSE,    NA_character_,
      "SUPPDM", "IDVARVAL",       5L, FALSE,    NA_character_,
      "SUPPDM",    "QEVAL",       NA, FALSE,    NA_character_,
      "SUPPDM",   "QLABEL",       NA,  TRUE,    NA_character_,
      "SUPPDM",     "QNAM",       6L,  TRUE,    NA_character_,
      "SUPPDM",    "QORIG",       NA,  TRUE,    NA_character_,
      "SUPPDM",     "QVAL",       NA,  TRUE,    NA_character_,
      "SUPPDM",  "RDOMAIN",       2L,  TRUE,    NA_character_,
      "SUPPDM",  "STUDYID",       1L,  TRUE,    NA_character_,
      "SUPPDM",  "USUBJID",       3L,  TRUE,    NA_character_
   )

   # Read from define
   def_ds_vars <- xml_to_ds_vars(define) %>%
      arrange(dataset, variable) %>%
      mutate(core = as.character(core))
   # Read from spec
   spec_ds_vars <- spec_type_to_ds_vars(spec) %>%
      arrange(dataset, variable) #%>%
      # mutate(key_seq = if_else(key_seq >= 6, NA_integer_, key_seq))

   # Tests
   expect_equal(def_ds_vars, ref_ds_vars)
   # expect_equal(spec_ds_vars,
   #              ref_ds_vars)

})


# #
# # test_that()
# #
# # ds_vars <- xml_to_ds_vars(doc)
# # var_spec <- xml_to_var_spec(doc)
# # value_spec <- xml_to_value_spec(doc)
# # code_list <- xml_to_code_list(doc)
# # derivations <- xml_to_derivations(doc)
# datapasta::dpasta(def_ds_vars)
# xml_to_ds_vars(define) %>%
#    arrange(dataset, variable) %>%
#    datapasta::dpasta()


