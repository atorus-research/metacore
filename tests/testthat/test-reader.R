# Read in doc to be used for testing
define <- read_xml("define-2021.xml")
xml_ns_strip(define)

spec <- read_all_sheets(metacore_example("p21_mock.xlsx"))


#### Fist checking some reading in
test_that("Check spec_type", {
   expect_equal(spec_type(metacore_example("p21_mock.xlsx")), "by_type")
   expect_equal(spec_type(metacore_example("mock_spec.xlsx")), "by_type")
   # Check it errors when format is not acceptable
   expect_error(spec_type("example_spec.xlsx"))
})

#### Check reads are consistent between formats
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
   spec_ds_spec2 <- spec_type_to_ds_spec(spec, sheet = "D")

   # Test against reference
   expect_equal(def_ds_spec, ref_ds_spec)
   expect_equal(spec_ds_spec, arrange(ref_ds_spec, dataset))
   expect_equal(spec_ds_spec2, arrange(ref_ds_spec, dataset))
})


test_that("Test ds_vars readers", {
   # Create a reference ds_vars
   ref_ds_vars <- tibble::tribble(
      ~dataset,  ~variable, ~key_seq, ~order, ~keep, ~core, ~supp_flag,
      "AE",    "AEACN",       NA,    21L, FALSE,    NA_character_, NA,
      "AE", "AEBDSYCD",       NA,    16L, FALSE,    NA_character_, NA,
      "AE", "AEBODSYS",       NA,    15L, FALSE,    NA_character_, NA,
      "AE",  "AEDECOD",       NA,     9L, FALSE,    NA_character_, NA,
      "AE",    "AEDTC",       NA,    32L, FALSE,    NA_character_, NA,
      "AE",     "AEDY",       NA,    35L, FALSE,    NA_character_, NA,
      "AE",  "AEENDTC",       NA,    34L, FALSE,    NA_character_, NA,
      "AE",   "AEENDY",       NA,    37L, FALSE,    NA_character_, NA,
      "AE",   "AEHLGT",       NA,    13L, FALSE,    NA_character_, NA,
      "AE", "AEHLGTCD",       NA,    14L, FALSE,    NA_character_, NA,
      "AE",    "AEHLT",       NA,    11L, FALSE,    NA_character_, NA,
      "AE",  "AEHLTCD",       NA,    12L, FALSE,    NA_character_, NA,
      "AE",    "AELLT",       NA,     7L, FALSE,    NA_character_, NA,
      "AE",  "AELLTCD",       NA,     8L, FALSE,    NA_character_, NA,
      "AE",    "AEOUT",       NA,    23L, FALSE,    NA_character_, NA,
      "AE",   "AEPTCD",       NA,    10L, FALSE,    NA_character_, NA,
      "AE",    "AEREL",       NA,    22L, FALSE,    NA_character_, NA,
      "AE",   "AESCAN",       NA,    24L, FALSE,    NA_character_, NA,
      "AE",  "AESCONG",       NA,    25L, FALSE,    NA_character_, NA,
      "AE", "AESDISAB",       NA,    26L, FALSE,    NA_character_, NA,
      "AE",   "AESDTH",       NA,    27L, FALSE,    NA_character_, NA,
      "AE",    "AESEQ",       5L,     4L,  TRUE,    NA_character_, NA,
      "AE",    "AESER",       NA,    20L, FALSE,    NA_character_, NA,
      "AE",    "AESEV",       NA,    19L, FALSE,    NA_character_, NA,
      "AE",  "AESHOSP",       NA,    28L, FALSE,    NA_character_, NA,
      "AE",  "AESLIFE",       NA,    29L, FALSE,    NA_character_, NA,
      "AE",    "AESOC",       NA,    17L, FALSE,    NA_character_, NA,
      "AE",  "AESOCCD",       NA,    18L, FALSE,    NA_character_, NA,
      "AE",    "AESOD",       NA,    30L, FALSE,    NA_character_, NA,
      "AE",   "AESPID",       NA,     5L, FALSE,    NA_character_, NA,
      "AE",  "AESTDTC",       4L,    33L, FALSE,    NA_character_, NA,
      "AE",   "AESTDY",       NA,    36L, FALSE,    NA_character_, NA,
      "AE",   "AETERM",       3L,     6L,  TRUE,    NA_character_, NA,
      "AE",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_, NA,
      "AE",    "EPOCH",       NA,    31L, FALSE,    NA_character_, NA,
      "AE",  "STUDYID",       1L,     1L,  TRUE,    NA_character_, NA,
      "AE",  "USUBJID",       2L,     3L,  TRUE,    NA_character_, NA,
      "DM",   "ACTARM",       NA,    22L,  TRUE,    NA_character_, NA,
      "DM", "ACTARMCD",       NA,    21L,  TRUE,    NA_character_, NA,
      "DM",      "AGE",       NA,    14L, FALSE,    NA_character_, NA,
      "DM",     "AGEU",       NA,    15L, FALSE,    NA_character_, NA,
      "DM",      "ARM",       NA,    20L,  TRUE,    NA_character_, NA,
      "DM",    "ARMCD",       NA,    19L,  TRUE,    NA_character_, NA,
      "DM",  "COUNTRY",       NA,    23L,  TRUE,    NA_character_, NA,
      "DM",    "DMDTC",       NA,    24L, FALSE,    NA_character_, NA,
      "DM",     "DMDY",       NA,    25L, FALSE,    NA_character_, NA,
      "DM",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_, NA,
      "DM",   "DTHDTC",       NA,    11L, FALSE,    NA_character_, NA,
      "DM",    "DTHFL",       NA,    12L, FALSE,    NA_character_, NA,
      "DM",   "ETHNIC",       NA,    18L, FALSE,    NA_character_, NA,
      "DM",     "RACE",       NA,    17L, FALSE,    NA_character_, NA,
      "DM",  "RFENDTC",       NA,     6L, FALSE,    NA_character_, NA,
      "DM",  "RFICDTC",       NA,     9L, FALSE,    NA_character_, NA,
      "DM", "RFPENDTC",       NA,    10L, FALSE,    NA_character_, NA,
      "DM",  "RFSTDTC",       NA,     5L, FALSE,    NA_character_, NA,
      "DM", "RFXENDTC",       NA,     8L, FALSE,    NA_character_, NA,
      "DM", "RFXSTDTC",       NA,     7L, FALSE,    NA_character_, NA,
      "DM",      "SEX",       NA,    16L,  TRUE,    NA_character_, NA,
      "DM",   "SITEID",       NA,    13L,  TRUE,    NA_character_, NA,
      "DM",  "STUDYID",       1L,     1L,  TRUE,    NA_character_, NA,
      "DM",   "SUBJID",       NA,     4L,  TRUE,    NA_character_, NA,
      "DM",  "USUBJID",       2L,     3L,  TRUE,    NA_character_, NA,
      "EX",   "DOMAIN",       NA,     2L,  TRUE,    NA_character_, NA,
      "EX",    "EPOCH",       NA,    14L, FALSE,    NA_character_, NA,
      "EX",   "EXDOSE",       NA,     6L, FALSE,    NA_character_, NA,
      "EX", "EXDOSFRM",       NA,     8L, FALSE,    NA_character_, NA,
      "EX", "EXDOSFRQ",       NA,     9L, FALSE,    NA_character_, NA,
      "EX",   "EXDOSU",       NA,     7L, FALSE,    NA_character_, NA,
      "EX",  "EXENDTC",       NA,    16L, FALSE,    NA_character_, NA,
      "EX",   "EXENDY",       NA,    18L, FALSE,    NA_character_, NA,
      "EX",  "EXROUTE",       NA,    10L, FALSE,    NA_character_, NA,
      "EX",    "EXSEQ",       NA,     4L,  TRUE,    NA_character_, NA,
      "EX",  "EXSTDTC",       4L,    15L, FALSE,    NA_character_, NA,
      "EX",   "EXSTDY",       NA,    17L, FALSE,    NA_character_, NA,
      "EX",    "EXTRT",       3L,     5L,  TRUE,    NA_character_, NA,
      "EX",  "STUDYID",       1L,     1L,  TRUE,    NA_character_, NA,
      "EX",  "USUBJID",       2L,     3L,  TRUE,    NA_character_, NA,
      "EX",    "VISIT",       NA,    12L, FALSE,    NA_character_, NA,
      "EX",  "VISITDY",       NA,    13L, FALSE,    NA_character_, NA,
      "EX", "VISITNUM",       NA,    11L, FALSE,    NA_character_, NA,
      "SUPPAE",    "IDVAR",       4L,     4L, FALSE,    NA_character_, NA,
      "SUPPAE", "IDVARVAL",       5L,     5L, FALSE,    NA_character_, NA,
      "SUPPAE",    "QEVAL",       NA,    10L, FALSE,    NA_character_, NA,
      "SUPPAE",   "QLABEL",       NA,     7L,  TRUE,    NA_character_, NA,
      "SUPPAE",     "QNAM",       6L,     6L,  TRUE,    NA_character_, NA,
      "SUPPAE",    "QORIG",       NA,     9L,  TRUE,    NA_character_, NA,
      "SUPPAE",     "QVAL",       NA,     8L,  TRUE,    NA_character_, NA,
      "SUPPAE",  "RDOMAIN",       2L,     2L,  TRUE,    NA_character_, NA,
      "SUPPAE",  "STUDYID",       1L,     1L,  TRUE,    NA_character_, NA,
      "SUPPAE",  "USUBJID",       3L,     3L,  TRUE,    NA_character_, NA,
      "SUPPDM",    "IDVAR",       4L,     4L, FALSE,    NA_character_, NA,
      "SUPPDM", "IDVARVAL",       5L,     5L, FALSE,    NA_character_, NA,
      "SUPPDM",    "QEVAL",       NA,    10L, FALSE,    NA_character_, NA,
      "SUPPDM",   "QLABEL",       NA,     7L,  TRUE,    NA_character_, NA,
      "SUPPDM",     "QNAM",       6L,     6L,  TRUE,    NA_character_, NA,
      "SUPPDM",    "QORIG",       NA,     9L,  TRUE,    NA_character_, NA,
      "SUPPDM",     "QVAL",       NA,     8L,  TRUE,    NA_character_, NA,
      "SUPPDM",  "RDOMAIN",       2L,     2L,  TRUE,    NA_character_, NA,
      "SUPPDM",  "STUDYID",       1L,     1L,  TRUE,    NA_character_, NA,
      "SUPPDM",  "USUBJID",       3L,     3L,  TRUE,    NA_character_, NA,
   )

   # Read from define
   def_ds_vars <- xml_to_ds_vars(define) %>%
      arrange(dataset, variable)

   # Read from spec
   spec_ds_vars <- spec_type_to_ds_vars(spec) %>%
      arrange(dataset, variable) %>%
      select(dataset, variable, key_seq, order, keep, core, supp_flag)


   # Tests
   expect_equal(def_ds_vars, ref_ds_vars)
   expect_equal(spec_ds_vars, ref_ds_vars)

})


test_that("Test var_spec readers", {
   ref_var_spec <-
      tibble::tribble(
      ~variable,      ~type, ~length,                                    ~label, ~format, ~common,
      "ACTARM",     "text",     20L,               "Description of Actual Arm",      NA,      NA,
      "ACTARMCD",     "text",      8L,                         "Actual Arm Code",      NA,      NA,
      "AEACN",     "text",     30L,       "Action Taken with Study Treatment",      NA,      NA,
      "AEBDSYCD",  "integer",      8L,         "Body System or Organ Class Code",      NA,      NA,
      "AEBODSYS",     "text",     67L,              "Body System or Organ Class",      NA,      NA,
      "AEDECOD",     "text",    200L,                 "Dictionary-Derived Term",      NA,      NA,
      "AEDTC",     "date",     10L,                 "Date/Time of Collection",      NA,      NA,
      "AEDY",  "integer",      8L,      "Study Day of Visit/Collection/Exam",      NA,      NA,
      "AEENDTC",     "date",     10L,          "End Date/Time of Adverse Event",      NA,      NA,
      "AEENDY",  "integer",      8L,       "Study Day of End of Adverse Event",      NA,      NA,
      "AEHLGT",     "text",    100L,                   "High Level Group Term",      NA,      NA,
      "AEHLGTCD",  "integer",      8L,              "High Level Group Term Code",      NA,      NA,
      "AEHLT",     "text",    100L,                         "High Level Term",      NA,      NA,
      "AEHLTCD",  "integer",      8L,                    "High Level Term Code",      NA,      NA,
      "AELLT",     "text",    100L,                       "Lowest Level Term",      NA,      NA,
      "AELLTCD",  "integer",      8L,                  "Lowest Level Term Code",      NA,      NA,
      "AEOUT",     "text",    200L,                "Outcome of Adverse Event",      NA,      NA,
      "AEPTCD",  "integer",      8L,                     "Preferred Term Code",      NA,      NA,
      "AEREL",     "text",      8L,                               "Causality",      NA,      NA,
      "AESCAN",     "text",      1L,                         "Involves Cancer",      NA,      NA,
      "AESCONG",     "text",      1L,      "Congenital Anomaly or Birth Defect",      NA,      NA,
      "AESDISAB",     "text",      1L, "Persist or Signif Disability/Incapacity",      NA,      NA,
      "AESDTH",     "text",      1L,                        "Results in Death",      NA,      NA,
      "AESEQ",  "integer",      8L,                         "Sequence Number",      NA,      NA,
      "AESER",     "text",      1L,                           "Serious Event",      NA,      NA,
      "AESEV",     "text",      8L,                      "Severity/Intensity",      NA,      NA,
      "AESHOSP",     "text",      1L,    "Requires or Prolongs Hospitalization",      NA,      NA,
      "AESLIFE",     "text",      1L,                     "Is Life Threatening",      NA,      NA,
      "AESOC",     "text",    100L,              "Primary System Organ Class",      NA,      NA,
      "AESOCCD",  "integer",      8L,         "Primary System Organ Class Code",      NA,      NA,
      "AESOD",     "text",      1L,                  "Occurred with Overdose",      NA,      NA,
      "AESPID",     "text",      3L,              "Sponsor-Defined Identifier",      NA,      NA,
      "AESTDTC",     "date",     10L,        "Start Date/Time of Adverse Event",      NA,      NA,
      "AESTDY",  "integer",      8L,     "Study Day of Start of Adverse Event",      NA,      NA,
      "AETERM",     "text",    200L,     "Reported Term for the Adverse Event",      NA,      NA,
      "AGE",  "integer",      8L,                                     "Age",      NA,      NA,
      "AGEU",     "text",      6L,                               "Age Units",      NA,      NA,
      "ARM",     "text",     20L,              "Description of Planned Arm",      NA,      NA,
      "ARMCD",     "text",      8L,                        "Planned Arm Code",      NA,      NA,
      "COUNTRY",     "text",      3L,                                 "Country",      NA,      NA,
      "DMDTC",     "date",     10L,                 "Date/Time of Collection",      NA,      NA,
      "DMDY",  "integer",      8L,                 "Study Day of Collection",      NA,      NA,
      "DOMAIN",     "text",      2L,                     "Domain Abbreviation",      NA,      NA,
      "DTHDTC", "datetime",     20L,                      "Date/Time of Death",      NA,      NA,
      "DTHFL",     "text",      1L,                      "Subject Death Flag",      NA,      NA,
      "EPOCH",     "text",      9L,                                   "Epoch",      NA,      NA,
      "ETHNIC",     "text",     25L,                               "Ethnicity",      NA,      NA,
      "EXDOSE",  "integer",      8L,                                    "Dose",      NA,      NA,
      "EXDOSFRM",     "text",      5L,                               "Dose Form",      NA,      NA,
      "EXDOSFRQ",     "text",      2L,           "Dosing Frequency per Interval",      NA,      NA,
      "EXDOSU",     "text",      2L,                              "Dose Units",      NA,      NA,
      "EXENDTC",     "date",     10L,              "End Date/Time of Treatment",      NA,      NA,
      "EXENDY",  "integer",      8L,           "Study Day of End of Treatment",      NA,      NA,
      "EXROUTE",     "text",     11L,                 "Route of Administration",      NA,      NA,
      "EXSEQ",  "integer",      8L,                         "Sequence Number",      NA,      NA,
      "EXSTDTC",     "date",     10L,            "Start Date/Time of Treatment",      NA,      NA,
      "EXSTDY",  "integer",      8L,         "Study Day of Start of Treatment",      NA,      NA,
      "EXTRT",     "text",     10L,                      "Name of  Treatment",      NA,      NA,
      "IDVAR",     "text",      8L,                    "Identifying Variable",      NA,      NA,
      "IDVARVAL",     "text",    200L,              "Identifying Variable Value",      NA,      NA,
      "QEVAL",     "text",    200L,                               "Evaluator",      NA,      NA,
      "QLABEL",     "text",     40L,                "Qualifier Variable Label",      NA,      NA,
      "QNAM",     "text",      8L,                 "Qualifier Variable Name",      NA,      NA,
      "QORIG",     "text",    200L,                                  "Origin",      NA,      NA,
      "QVAL",     "text",    200L,                              "Data Value",      NA,      NA,
      "RACE",     "text",     78L,                                    "Race",      NA,      NA,
      "RDOMAIN",     "text",      2L,             "Related Domain Abbreviation",      NA,      NA,
      "RFENDTC",     "date",     10L,         "Subject Reference End Date/Time",      NA,      NA,
      "RFICDTC", "datetime",     20L,           "Date/Time of Informed Consent",      NA,      NA,
      "RFPENDTC", "datetime",     20L,       "Date/Time of End of Participation",      NA,      NA,
      "RFSTDTC",     "date",     10L,       "Subject Reference Start Date/Time",      NA,      NA,
      "RFXENDTC", "datetime",     20L,       "Date/Time of Last Study Treatment",      NA,      NA,
      "RFXSTDTC", "datetime",     20L,      "Date/Time of First Study Treatment",      NA,      NA,
      "SEX",     "text",      1L,                                     "Sex",      NA,      NA,
      "SITEID",     "text",      3L,                   "Study Site Identifier",      NA,      NA,
      "STUDYID",     "text",     12L,                        "Study Identifier",      NA,      NA,
      "SUBJID",     "text",      4L,        "Subject Identifier for the Study",      NA,      NA,
      "USUBJID",     "text",     11L,               "Unique Subject Identifier",      NA,      NA,
      "VISIT",     "text",     19L,                              "Visit Name",      NA,      NA,
      "VISITDY",  "integer",      8L,              "Planned Study Day of Visit",      NA,      NA,
      "VISITNUM",    "float",      8L,                            "Visit Number",   "8.1",      NA
   )

   # Read from define
   def_var_spec <- xml_to_var_spec(define) %>%
      arrange(variable)

   # Read from spec
   spec_var_spec <- spec_type_to_var_spec(spec) %>%
      arrange(variable) %>%
      select(variable, type, length, label, format)
   # remove common as it is derived when reading in specs but left alone from defines

   # Tests
   expect_equal(def_var_spec, ref_var_spec)
   expect_equal(spec_var_spec,
                ref_var_spec %>%
                   select(-common))

})


test_that("values_spec reader tests", {
   ref_value_spec <- tibble::tribble(
      ~dataset,  ~variable,      ~type,    ~origin,         ~code_id,   ~sig_dig,   ~where,            ~derivation_id,
      "AE",    "AEACN",     "text",  "Derived",               NA,          NA,          NA,             "MT.AE.AEACN",
      "AE", "AEBDSYCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE", "AEBODSYS",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE",  "AEDECOD",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE",    "AEDTC",     "date",  "Derived",               NA,          NA,          NA,             "MT.AE.AEDTC",
      "AE",     "AEDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "AE",  "AEENDTC",     "date",      "CRF",               NA,          NA,          NA,                        NA,
      "AE",   "AEENDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "AE",   "AEHLGT",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE", "AEHLGTCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "AEHLT",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE",  "AEHLTCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "AELLT",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE",  "AELLTCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "AEOUT",     "text",      "CRF",         "CL.OUT",          NA,          NA,                        NA,
      "AE",   "AEPTCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "AEREL",     "text",      "CRF",      "CL.AECAUS",          NA,          NA,                        NA,
      "AE",   "AESCAN",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",  "AESCONG",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE", "AESDISAB",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",   "AESDTH",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",    "AESEQ",  "integer",  "Derived",               NA,          NA,          NA,             "MT.AE.AESEQ",
      "AE",    "AESER",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",    "AESEV",     "text",      "CRF",         "CL.SEV",          NA,          NA,                        NA,
      "AE",  "AESHOSP",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",  "AESLIFE",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",    "AESOC",     "text", "Assigned",      "CL.AEDICT",          NA,          NA,                        NA,
      "AE",  "AESOCCD",  "integer", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "AESOD",     "text",      "CRF",          "CL.YN",          NA,          NA,                        NA,
      "AE",   "AESPID",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "AE",  "AESTDTC",     "date",      "CRF",               NA,          NA,          NA,                        NA,
      "AE",   "AESTDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "AE",   "AETERM",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "AE",   "DOMAIN",     "text", "Assigned",               NA,          NA,          NA,                        NA,
      "AE",    "EPOCH",     "text",  "Derived",       "CL.EPOCH",          NA,          NA,             "MT.AE.EPOCH",
      "AE",  "STUDYID",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "AE",  "USUBJID",     "text",  "Derived",               NA,          NA,          NA,           "MT.AE.USUBJID",
      "DM",   "ACTARM",     "text",  "Derived",         "CL.ARM",          NA,          NA,            "MT.DM.ACTARM",
      "DM", "ACTARMCD",     "text",  "Derived",       "CL.ARMCD",          NA,          NA,          "MT.DM.ACTARMCD",
      "DM",      "AGE",  "integer",  "Derived",               NA,          NA,          NA,               "MT.DM.AGE",
      "DM",     "AGEU",     "text", "Assigned",        "CL.AGEU",          NA,          NA,             "COM.DM.AGEU",
      "DM",      "ARM",     "text", "Assigned",         "CL.ARM",          NA,          NA,              "COM.DM.ARM",
      "DM",    "ARMCD",     "text", "Assigned",       "CL.ARMCD",          NA,          NA,            "COM.DM.ARMCD",
      "DM",  "COUNTRY",     "text",  "Derived",     "CL.COUNTRY",          NA,          NA,           "MT.DM.COUNTRY",
      "DM",    "DMDTC",     "date",      "CRF",               NA,          NA,          NA,                        NA,
      "DM",     "DMDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "DM",   "DOMAIN",     "text", "Assigned",               NA,          NA,          NA,                        NA,
      "DM",   "DTHDTC", "datetime",  "Derived",               NA,          NA,          NA,            "MT.DM.DTHDTC",
      "DM",    "DTHFL",     "text",  "Derived",     "CL.Y_BLANK",          NA,          NA,             "MT.DM.DTHFL",
      "DM",   "ETHNIC",     "text",  "Derived",      "CL.ETHNIC",          NA,          NA,            "MT.DM.ETHNIC",
      "DM",     "RACE",     "text",      "CRF",        "CL.RACE",          NA,          NA,                        NA,
      "DM",  "RFENDTC",     "date",  "Derived",               NA,          NA,          NA,           "MT.DM.RFENDTC",
      "DM",  "RFICDTC", "datetime",  "Derived",               NA,          NA,          NA,           "MT.DM.RFICDTC",
      "DM", "RFPENDTC", "datetime",  "Derived",               NA,          NA,          NA,          "MT.DM.RFPENDTC",
      "DM",  "RFSTDTC",     "date",  "Derived",               NA,          NA,          NA,           "MT.DM.RFSTDTC",
      "DM", "RFXENDTC", "datetime",  "Derived",               NA,          NA,          NA,          "MT.DM.RFXENDTC",
      "DM", "RFXSTDTC", "datetime",  "Derived",               NA,          NA,          NA,          "MT.DM.RFXSTDTC",
      "DM",      "SEX",     "text",      "CRF",         "CL.SEX",          NA,          NA,                        NA,
      "DM",   "SITEID",     "text", "Assigned",               NA,          NA,          NA,                        NA,
      "DM",  "STUDYID",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "DM",   "SUBJID",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "DM",  "USUBJID",     "text",  "Derived",               NA,          NA,          NA,           "MT.DM.USUBJID",
      "EX",   "DOMAIN",     "text", "Assigned",               NA,          NA,          NA,                        NA,
      "EX",    "EPOCH",     "text",  "Derived",       "CL.EPOCH",          NA,          NA,             "MT.EX.EPOCH",
      "EX",   "EXDOSE",  "integer",      "eDT",               NA,          NA,          NA,                        NA,
      "EX", "EXDOSFRM",     "text",      "eDT",    "CL.EXDOSFRM",          NA,          NA,                        NA,
      "EX", "EXDOSFRQ",     "text",      "eDT",      "CL.EXFREQ",          NA,          NA,                        NA,
      "EX",   "EXDOSU",     "text",      "eDT",     "CL.EXDOSEU",          NA,          NA,                        NA,
      "EX",  "EXENDTC",     "date",      "CRF",               NA,          NA,          NA,                        NA,
      "EX",   "EXENDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "EX",  "EXROUTE",     "text",      "eDT",     "CL.EXROUTE",          NA,          NA,                        NA,
      "EX",    "EXSEQ",  "integer",  "Derived",               NA,          NA,          NA,             "MT.EX.EXSEQ",
      "EX",  "EXSTDTC",     "date",      "CRF",               NA,          NA,          NA,                        NA,
      "EX",   "EXSTDY",  "integer",  "Derived",               NA,          NA,          NA, "MT.COMPMETHOD.STUDY_DAY",
      "EX",    "EXTRT",     "text",      "eDT",       "CL.EXTRT",          NA,          NA,                        NA,
      "EX",  "STUDYID",     "text",      "CRF",               NA,          NA,          NA,                        NA,
      "EX",  "USUBJID",     "text",  "Derived",               NA,          NA,          NA,           "MT.EX.USUBJID",
      "EX",    "VISIT",     "text",      "CRF",       "CL.VISIT",          NA,          NA,                        NA,
      "EX",  "VISITDY",  "integer",  "Derived",               NA,          NA,          NA,           "MT.EX.VISITDY",
      "EX", "VISITNUM",    "float",      "CRF",    "CL.VISITNUM",          1L,          NA,                        NA,
      "SUPPAE",    "IDVAR",     "text", "Assigned",               NA,      NA,         NA,         "COM.SUPPAE.IDVAR",
      "SUPPAE", "IDVARVAL",     "text",  "Derived",               NA,      NA,         NA,      "MT.SUPPAE.IDVARVAL",
      "SUPPAE",    "QEVAL",     "text", "Assigned",       "CL.QEVAL",      NA,         NA,                        NA,
      "SUPPAE",   "QLABEL",     "text", "Assigned",               NA,      NA,         NA,                        NA,
      "SUPPAE",     "QNAM",     "text", "Assigned", "CL.SUPPAE.QNAM",      NA,         NA,                        NA,
      "SUPPAE",    "QORIG",     "text", "Assigned",               NA,      NA,         NA,                        NA,
      "SUPPAE",     "QVAL",     "text",  "Derived",          "CL.YN",      NA, "QNAM == 'TRTEMFL'",  "MT.SUPPAE.QNAM.TRTEMFL",
      "SUPPAE",  "RDOMAIN",     "text", "Assigned",               NA,      NA,         NA,            "COM.SUPPAE.RDOMAIN",
      "SUPPAE",  "STUDYID",     "text",      "CRF",               NA,      NA,         NA,                        NA,
      "SUPPAE",  "USUBJID",     "text",  "Derived",               NA,      NA,         NA,       "MT.SUPPAE.USUBJID",
      "SUPPDM",    "IDVAR",     "text", "Assigned",               NA,      NA,         NA,        "COM.SUPPDM.IDVAR",
      "SUPPDM", "IDVARVAL",     "text", "Assigned",               NA,      NA,         NA,      "COM.SUPPDM.IDVARVAL",
      "SUPPDM",    "QEVAL",     "text", "Assigned",       "CL.QEVAL",      NA,         NA,                        NA,
      "SUPPDM",   "QLABEL",     "text", "Assigned",               NA,      NA,         NA,                        NA,
      "SUPPDM",     "QNAM",     "text", "Assigned", "CL.SUPPDM.QNAM",      NA,         NA,                        NA,
      "SUPPDM",    "QORIG",     "text", "Assigned",               NA,      NA,         NA,                        NA,
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,  "QNAM == 'COMPLT16'", "MT.SUPPDM.QNAM.COMPLT16",
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,  "QNAM == 'COMPLT24'", "MT.SUPPDM.QNAM.COMPLT24",
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,   "QNAM == 'COMPLT8'",  "MT.SUPPDM.QNAM.COMPLT8",
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,  "QNAM == 'EFFICACY'", "MT.SUPPDM.QNAM.EFFICACY",
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,    "QNAM == 'SAFETY'",   "MT.SUPPDM.QNAM.SAFETY",
      "SUPPDM",     "QVAL",     "text",  "Derived",     "CL.Y_BLANK",      NA,       "QNAM == 'ITT'",      "MT.SUPPDM.QNAM.ITT",
      "SUPPDM",  "RDOMAIN",     "text", "Assigned",               NA,      NA,         NA,                 "COM.SUPPDM.RDOMAIN",
      "SUPPDM",  "STUDYID",     "text",      "CRF",               NA,      NA,         NA,                        NA,
      "SUPPDM",  "USUBJID",     "text",  "Derived",               NA,      NA,         NA,       "MT.SUPPDM.USUBJID"
   )

   # Read from define
   def_value_spec <- xml_to_value_spec(define) %>%
      arrange(dataset, variable) %>%
      select(dataset, variable, type, origin, code_id, sig_dig, where, derivation_id )

   # Read from spec
   comments <- spec$Comments$ID
   spec_value_spec <- spec_type_to_value_spec(spec) %>%
      arrange(dataset, variable) %>%
      select(dataset, variable, type, origin, code_id, sig_dig, where, derivation_id) %>%
      #Fix naming as it is slightly different, but matches within metacore
      mutate(code_id = if_else(!is.na(code_id), paste0("CL.", code_id), code_id, NA_character_),
             derivation_id = case_when(
                origin == "Derived" ~ paste0("MT.", derivation_id),
                origin == "Assigned" & derivation_id %in% comments ~ paste0("COM.", derivation_id),
                TRUE ~ NA_character_),
             where = str_replace(where, "EQ", "=="),
             where = str_split(where, "\\s(?=(\\w*$))") %>%
                map(~paste0(.[1], " '", .[2])),
             where = if_else(where == "NA 'NA", NA_character_, paste0(where, "'")))


   # Tests
   expect_equal(def_value_spec, ref_value_spec)
   expect_equal(spec_value_spec,  ref_value_spec)

   })


test_that("derivation reader tests", {
   # Create reference derivation tibble
   ref_derivation <- tibble::tribble(
      ~derivation_id,      ~derivation,
      "COM.DM.AGEU",       "AGEU='YEARS'",
      "COM.DM.ARM",        "According to randomization list",
      "COM.DM.ARMCD",      "According to randomization list",
      "COM.SUPPAE.IDVAR",  "IDVAR='AESEQ'",
      "COM.SUPPAE.RDOMAIN", "RDOMAIN='AE'",
      "COM.SUPPDM.IDVAR", "IDVAR=' '",
      "COM.SUPPDM.IDVARVAL", "IDVARVAL= ' '",
      "COM.SUPPDM.RDOMAIN", "RDOMAIN='AE'",
      "MT.AE.AEACN" ,            "AEACN=Null (data on action taken concerning study treatment was not collected)"                                                       ,
      "MT.AE.AEDTC" ,            "Date of final visit (SV)"                                                                                                             ,
      "MT.AE.AESEQ" ,            "Sequential number identifying records within each USUBJID"                                                                            ,
      "MT.AE.EPOCH" ,            "Use AESTDTC to determine from the SDTM SE what EPOCH this record falls under.\nIf SE.SESTDTC <= AESTDTC < SE.SEENDTC, then use that EPOCH.",
      "MT.AE.USUBJID"           ,"Concatenation of STUDYID, DM.SITEID and DM.SUBJID"                                                                                    ,
      "MT.COMPMETHOD.STUDY_DAY" ,"(date portion of --DTC) minus (date portion of RFSTDTC) , add 1 if -- DTC >= RFSTDC"                                                  ,
      "MT.DM.ACTARM"            ,"Derived from EX"                                                                                                                      ,
      "MT.DM.ACTARMCD"          ,"Derived from EX"                                                                                                                      ,
      "MT.DM.AGE"               ,"Subject's Age at start of study drug (RFSTDTC)."                                                                                      ,
      "MT.DM.COUNTRY"           ,"Derived from site information"                                                                                                        ,
      "MT.DM.DTHDTC"            ,"If DS record exists with DSDECOD='DEATH' then DTHDTC=AEENDTC."                                                                        ,
      "MT.DM.DTHFL"             ,"If DS record exists with DSDECOD='DEATH' then DEATHFL=Y."                                                                             ,
      "MT.DM.ETHNIC"            ,"Derived from Origin entered on CRF: ETHINC='HISPANIC OR LATINO' if Origin='Hispanic'. Otherwise ETHNIC='NOT HISPANIC OR LATINO'"      ,
      "MT.DM.RFENDTC"           ,"Date/time of last study drug treatment derived from EX"                                                                               ,
      "MT.DM.RFICDTC"           ,"Date of informed consent was not entered in database (see annotated CRF)"                                                             ,
      "MT.DM.RFPENDTC"          ,"DSSTDTC of last disposition event"                                                                                                    ,
      "MT.DM.RFSTDTC"           ,"Date/time of first study drug treatment derived from EX"                                                                              ,
      "MT.DM.RFXENDTC"          ,"RFXENDTC=RFENDTC"                                                                                                                     ,
      "MT.DM.RFXSTDTC"          ,"RFXSTDTC=RFSTDTC"                                                                                                                     ,
      "MT.DM.USUBJID"           ,"Concatenation of STUDYID, DM.SITEID and DM.SUBJID"                                                                                    ,
      "MT.EX.EPOCH"             ,"Use EXDTC to determine from the SDTM SE what EPOCH this record falls under.\nIf SE.SESTDTC <= EXDTC < SE.SEENDTC, then use that EPOCH.",
      "MT.EX.EXSEQ"             ,"Sequential number identifying records within each USUBJID"                                                                            ,
      "MT.EX.USUBJID"           ,"Concatenation of STUDYID, DM.SITEID and DM.SUBJID"                                                                                    ,
      "MT.EX.VISITDY"           ,"TV.VISITDY"                                                                                                                           ,
      "MT.SUPPAE.IDVARVAL"      ,"Value of AESEQ of corresponding parent record"                                                                                        ,
      "MT.SUPPAE.QNAM.TRTEMFL"  ,"see SAP"                                                                                                                              ,
      "MT.SUPPAE.QVAL"          ,"see value level metadata"                                                                                                             ,
      "MT.SUPPAE.USUBJID"       ,"Concatenation of STUDYID, DM.SITEID and DM.SUBJID"                                                                                    ,
      "MT.SUPPDM.QNAM.COMPLT16" ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QNAM.COMPLT24" ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QNAM.COMPLT8"  ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QNAM.EFFICACY" ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QNAM.ITT"      ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QNAM.SAFETY"   ,"see SAP"                                                                                                                              ,
      "MT.SUPPDM.QVAL"          ,"see value level metadata"                                                                                                             ,
      "MT.SUPPDM.USUBJID"       ,"Concatenation of STUDYID, DM.SITEID and DM.SUBJID" ,
   )

   # Read from define
   def_derivation <- xml_to_derivations(define) %>%
      arrange(derivation_id) %>%
      mutate(derivation = str_replace_all(derivation, '\\"', "\\'"))

   # Read from spec

   ref_deriv <- spec$Methods %>%
      select(derivation_id = ID,
             derivation = Description) %>%
      mutate(derivation_id = paste0("MT.", derivation_id))
   ref_deriv <- spec$Variables %>%
      filter(Origin %in% c("Assigned")) %>%
      mutate(derivation_id = paste0("MT.", Dataset, ".", Variable),
             derivation = Comment) %>%
      select(starts_with("derivation")) %>%
      bind_rows(ref_deriv, .) %>%
      arrange(derivation_id) %>%
      distinct()

   spec_derivation <- spec_type_to_derivations(spec) %>%
      arrange(derivation_id) %>%
      mutate(derivation_id = paste0("MT.", derivation_id))

   # Tests
   expect_equal(def_derivation, ref_derivation)
   expect_equal(spec_derivation, ref_deriv)
})

test_that("codelist reader tests", {
   # Create reference derivation tibble
   ref_codelist <- tibble::tribble(
      ~code_id,                        ~name,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ~codes,              ~type,
      "CL.AECAUS",                     "AECAUS",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     tibble(code = c("NONE", "POSSIBLE", "PROBABLE", "REMOTE"), decode = c("NONE", "POSSIBLE", "PROBABLE", "REMOTE")),      "code_decode",
      "CL.AEDICT",   "ADVERSE EVENT DICTIONARY",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       tibble(dictionary = "MEDDRA", version = "8.0"), "external_library",
      "CL.AGEU",                       "AGEU",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             tibble(code = "YEARS", decode = "YEARS"),      "code_decode",
      "CL.ARM",                        "ARM",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 tibble(code = c("Screen Failure", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"), decode = c("Screen Failure", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),      "code_decode",
      "CL.ARMCD",                      "ARMCD",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      tibble(code = c("Scrnfail", "Pbo", "Xan_Lo", "Xan_Hi"), decode = c("Screen Failure", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),      "code_decode",
      "CL.COUNTRY",                    "COUNTRY",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 tibble(code = "USA", decode = "USA"),      "code_decode",
      "CL.DRUGDICT",            "DRUG DICTIONARY",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   tibble(dictionary = "WHODRUG", version = "200604"), "external_library",
      "CL.EPOCH",                      "EPOCH",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           tibble(code = c("SCREENING", "TREATMENT", "FOLLOW-UP"), decode = c("Screening", "Treatment", "Follow-Up")),      "code_decode",
      "CL.ETHNIC",                     "ETHNIC",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         tibble(code = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"), decode = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO")),      "code_decode",
      "CL.EXDOSEU",                    "EXDOSEU",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   tibble(code = "mg", decode = "mg"),      "code_decode",
      "CL.EXDOSFRM",                   "EXDOSFRM",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             tibble(code = "PATCH", decode = "PATCH"),      "code_decode",
      "CL.EXFREQ",                     "EXFREQ",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   tibble(code = "QD", decode = "QD"),      "code_decode",
      "CL.EXROUTE",                    "EXROUTE",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 tibble(code = "TRANSDERMAL", decode = "TRANSDERMAL"),      "code_decode",
      "CL.EXTRT",                      "EXTRT",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       tibble(code = c("PLACEBO", "XANOMELINE"), decode = c("PLACEBO", "XANOMELINE")),      "code_decode",
      "CL.MHDICT", "MEDICAL HISTORY DICTIONARY",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       tibble(dictionary = "MEDDRA", version = "8.0"), "external_library",
      "CL.OUT",                        "OUT",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               tibble(code = c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED", "FATAL"), decode = c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED", "FATAL")),      "code_decode",
      "CL.QEVAL",                      "QEVAL",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           tibble(code = "CLINICAL STUDY SPONSOR", decode = "CLINICAL STUDY SPONSOR"),      "code_decode",
      "CL.RACE",                       "RACE",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   tibble(code = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"), decode = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN")),      "code_decode",
      "CL.SEV",                        "SEV",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             tibble(code = c("MILD", "MODERATE", "SEVERE"), decode = c("MILD", "MODERATE", "SEVERE")),      "code_decode",
      "CL.SEX",                        "SEX",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             tibble(code = c("F", "M", "U"), decode = c("Female", "Male", "Unknown")),      "code_decode",
      "CL.SUPPAE.QNAM",                "SUPPAE.QNAM",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          tibble(code = "AETRTEM", decode = "TREAMENT EMERGENT FLAG"),      "code_decode",
      "CL.SUPPDM.QNAM",                "SUPPDM.QNAM",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      tibble(code = c("COMPLT16", "COMPLT24", "COMPLT8", "EFFICACY", "ITT", "SAFETY"), decode = c("Completers of Week 16 Population Flag", "Completers of Week 24 Population Flag", "Completers of Week 8 Population Flag", "Efficacy Population Flag", "Intent to Treat Population Flag", "Safety Population Flag")),      "code_decode",
      "CL.VISIT",                      "VISIT", tibble(code = c("SCREENING 1", "UNSCHEDULED 1.1", "UNSCHEDULED 1.2", "UNSCHEDULED 1.3", "SCREENING 2", "BASELINE", "UNSCHEDULED 3.1", "AMBUL ECG PLACEMENT", "WEEK 2", "UNSCHEDULED 4.1", "UNSCHEDULED 4.2", "WEEK 4", "UNSCHEDULED 5.1", "AMBUL ECG REMOVAL", "UNSCHEDULED 6.1", "WEEK 6", "UNSCHEDULED 7.1", "WEEK 8", "WEEK 10 (T)", "UNSCHEDULED 8.2", "WEEK 12", "WEEK 14 (T)", "UNSCHEDULED 9.2", "UNSCHEDULED 9.3", "WEEK 16", "WEEK 18 (T)", "UNSCHEDULED 10.2", "WEEK 20", "WEEK 22 (T)", "UNSCHEDULED 11.2",
                                                              "WEEK 24", "UNSCHEDULED 12.1", "WEEK 26", "UNSCHEDULED 13.1", "AE FOLLOW-UP", "RETRIEVAL", "Rash followup"), decode = c("SCREENING 1", "UNSCHEDULED 1.1", "UNSCHEDULED 1.2", "UNSCHEDULED 1.3", "SCREENING 2", "BASELINE", "UNSCHEDULED 3.1", "AMBUL ECG PLACEMENT", "WEEK 2", "UNSCHEDULED 4.1", "UNSCHEDULED 4.2", "WEEK 4", "UNSCHEDULED 5.1", "AMBUL ECG REMOVAL", "UNSCHEDULED 6.1", "WEEK 6", "UNSCHEDULED 7.1", "WEEK 8", "WEEK 10 (T)", "UNSCHEDULED 8.2", "WEEK 12", "WEEK 14 (T)", "UNSCHEDULED 9.2", "UNSCHEDULED 9.3",
                                                                                                                                                                                      "WEEK 16", "WEEK 18 (T)", "UNSCHEDULED 10.2", "WEEK 20", "WEEK 22 (T)", "UNSCHEDULED 11.2", "WEEK 24", "UNSCHEDULED 12.1", "WEEK 26", "UNSCHEDULED 13.1", "AE FOLLOW-UP", "RETRIEVAL", "Rash followup")),      "code_decode",
      "CL.VISITNUM",                   "VISITNUM",                                                                                                                                                                                                                                                                                                                                                                  tibble(code = c("1", "1.1", "1.2", "1.3", "2", "3", "3.1", "3.5", "4", "4.1", "4.2", "5", "5.1", "6", "6.1", "7", "7.1", "8", "8.1", "8.2", "9", "9.1", "9.2", "9.3", "10", "10.1", "10.2", "11", "11.1", "11.2", "12", "12.1", "13", "13.1", "101", "201", "501"), decode = c("SCREENING 1", "UNSCHEDULED 1.1", "UNSCHEDULED 1.2", "UNSCHEDULED 1.3", "SCREENING 2", "BASELINE", "UNSCHEDULED 3.1", "AMBUL ECG PLACEMENT", "WEEK 2", "UNSCHEDULED 4.1", "UNSCHEDULED 4.2", "WEEK 4", "UNSCHEDULED 5.1", "AMBUL ECG REMOVAL",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "UNSCHEDULED 6.1", "WEEK 6", "UNSCHEDULED 7.1", "WEEK 8", "WEEK 10 (T)", "UNSCHEDULED 8.2", "WEEK 12", "WEEK 14 (T)", "UNSCHEDULED 9.2", "UNSCHEDULED 9.3", "WEEK 16", "WEEK 18 (T)", "UNSCHEDULED 10.2", "WEEK 20", "WEEK 22 (T)", "UNSCHEDULED 11.2", "WEEK 24", "UNSCHEDULED 12.1", "WEEK 26", "UNSCHEDULED 13.1", "AE FOLLOW-UP", "RETRIEVAL", "Rash followup")),      "code_decode",
      #"CL.Y_BLANK",                    "Y_BLANK",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   tibble(code = "Y", decode = "Yes"),      "code_decode",
      "CL.YN",                         "YN",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  tibble(code = c("N", "Y"), decode = c("No", "Yes")),      "code_decode"
   )

   # Read from define
   def_codelist <- xml_to_codelist(define) %>%
      filter(code_id != "CL.Y_BLANK") %>%
      arrange(code_id) %>%
      select(code_id, name, codes, type)

   # Read from spec
   spec_codelist <- spec_type_to_codelist(spec, simplify = FALSE) %>%
      mutate(code_id = paste0("CL.", code_id)) %>%
      filter(code_id != "CL.Y_BLANK") %>%
      arrange(code_id) %>%
      select(code_id, name, codes, type)

   # Tests
   expect_equal(def_codelist, ref_codelist)
   expect_equal(spec_codelist, ref_codelist)
})

test_that("Specification Reader's errors and warnings", {
   # Check the name-checks work for each
   expect_error(spec_type_to_ds_spec(spec, cols = c("foo")))
   expect_error(spec_type_to_ds_spec(spec, cols = c("foo" = "foo")))
   expect_error(spec_type_to_ds_vars(spec, cols = c("foo")))
   expect_error(spec_type_to_ds_vars(spec, cols = c("foo" = "foo")))
   expect_error(spec_type_to_var_spec(spec, cols = c("foo")))
   expect_error(spec_type_to_var_spec(spec, cols = c("foo" = "foo")))
   expect_error(spec_type_to_value_spec(spec, cols = c("foo")))
   expect_error(spec_type_to_value_spec(spec, cols = c("foo" = "foo")))
   expect_error(spec_type_to_derivations(spec, cols = c("foo")))
   expect_error(spec_type_to_derivations(spec, cols = c("foo" = "foo")))
   expect_error(spec_type_to_codelist(spec, cols = c("foo")))
   expect_error(spec_type_to_codelist(spec, cols = c("foo" = "foo")))

})
