mc_define <- spec_to_metacore(metacore_example("p21_mock.xlsx"), where_sep_sheet = FALSE, verbose = "silent", define_fields = TRUE)
mc_original <- spec_to_metacore(metacore_example("p21_mock.xlsx"), where_sep_sheet = FALSE, verbose = "silent", define_fields = FALSE)

make_ae_spec <- function() {
  list(
    ds_spec = tibble::tibble(
      dataset = "AE", structure = "OneRowPerEvent", label = "Adverse Events"
    ),
    ds_vars = tibble::tibble(
      dataset = "AE", variable = "AETERM",
      key_seq = NA_integer_, order = 1L,
      mandatory = TRUE, core = "Expected", supp_flag = FALSE
    ),
    var_spec = tibble::tibble(
      variable = "AETERM", length = 200L, label = "AE Term",
      type = "character", common = NA, format = NA_character_
    ),
    value_spec = tibble::tibble(
      dataset = "AE", variable = "AETERM", type = "character",
      origin = "Collected", sig_dig = NA_integer_,
      code_id = NA_character_, where = NA_character_,
      derivation_id = NA_character_
    ),
    derivations = tibble::tibble(derivation_id = character(), derivation = character()),
    codelist = tibble::tibble(
      code_id = character(), name = character(), type = character(), codes = list()
    ),
    supp = tibble::tibble(
      dataset = character(), variable = character(),
      idvar = character(), qeval = character()
    )
  )
}

make_define_spec <- function() {
  list(
    ds_spec = tibble::tibble(
      dataset   = c("AE", "DM"),
      structure = c("OneRowPerEvent", "OneRowPerSubject"),
      label     = c("Adverse Events", "Demographics")
    ),
    ds_vars = tibble::tibble(
      dataset   = c("AE", "AE", "DM"),
      variable  = c("AETERM", "USUBJID", "USUBJID"),
      key_seq   = c(NA_integer_, 1L, 1L),
      order     = c(1L, 2L, 1L),
      mandatory = TRUE,
      core      = "Expected",
      supp_flag = FALSE
    ),
    var_spec = tibble::tibble(
      variable = c("AETERM", "USUBJID"),
      length   = c(200L, 40L),
      label    = c("AE Term", "Subject ID"),
      type     = "character",
      common   = NA,
      format   = NA_character_
    ),
    value_spec = tibble::tibble(
      dataset       = c("AE", "AE", "DM"),
      variable      = c("AETERM", "USUBJID", "USUBJID"),
      type          = "character",
      origin        = "Collected",
      sig_dig       = NA_integer_,
      code_id       = NA_character_,
      where         = NA_character_,
      derivation_id = NA_character_,
      comment_id    = c("COM.AE", NA, NA)
    ),
    derivations = tibble::tibble(
      derivation_id = character(), derivation   = character(),
      method_name   = character(), method_type  = character(),
      document_id   = character(), pages        = character()
    ),
    codelist = tibble::tibble(
      code_id = character(), name = character(), type = character(), codes = list()
    ),
    supp = tibble::tibble(
      dataset = character(), variable = character(),
      idvar = character(), qeval = character()
    ),
    study_level = tibble::tibble(
      study_name = "PILOT01", study_description = "Pilot Study",
      protocol_name = "PILOT01", standard_name = "ADaM",
      standard_version = "1.1", define_version = "2.0", language = "en"
    ),
    documents = tibble::tibble(
      document_id = "ADRG", title = "Reviewer Guide", href = "adrg.pdf"
    ),
    comments = tibble::tibble(
      comment_id = c("COM.AE", "COM.DM"),
      comment    = c("AE note", "DM note")
    )
  )
}
