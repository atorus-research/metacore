# Helpers -----------------------------------------------------------------------

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

make_two_dataset_spec <- function() {
  list(
    ds_spec = tibble::tibble(
      dataset   = c("AE", "DM"),
      structure = c("OneRowPerEvent", "OneRowPerSubject"),
      label     = c("Adverse Events", "Demographics")
    ),
    ds_vars = tibble::tibble(
      dataset   = c("AE", "AE", "DM", "DM"),
      variable  = c("AETERM", "USUBJID", "USUBJID", "AGE"),
      key_seq   = c(NA_integer_, 1L, 1L, NA_integer_),
      order     = c(1L, 2L, 1L, 2L),
      mandatory = c(TRUE, TRUE, TRUE, FALSE),
      core      = c("Expected", "Expected", "Expected", "Expected"),
      supp_flag = FALSE
    ),
    var_spec = tibble::tibble(
      variable = c("AETERM", "USUBJID", "AGE"),
      length   = c(200L, 40L, 3L),
      label    = c("AE Term", "Unique Subject ID", "Age"),
      type     = c("character", "character", "integer"),
      common   = NA,
      format   = NA_character_
    ),
    value_spec = tibble::tibble(
      dataset       = c("AE", "AE", "DM", "DM"),
      variable      = c("AETERM", "USUBJID", "USUBJID", "AGE"),
      type          = c("character", "character", "character", "integer"),
      origin        = c("Collected", "Assigned", "Assigned", "Collected"),
      sig_dig       = NA_integer_,
      code_id       = NA_character_,
      where         = NA_character_,
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


# Class and inheritance ---------------------------------------------------------

test_that("metacore(define_fields = TRUE) returns MetacoreDefine with correct class hierarchy", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(class(mc), c("MetacoreDefine", "Metacore", "R6"))
  expect_true(inherits(mc, "MetacoreDefine"))
  expect_true(inherits(mc, "Metacore"))
  expect_true(inherits(mc, "R6"))
})

test_that("metacore(define_fields = FALSE) returns base Metacore, not MetacoreDefine", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = FALSE))))

  expect_equal(class(mc), c("Metacore", "R6"))
  expect_false(inherits(mc, "MetacoreDefine"))
})


# Active bindings ---------------------------------------------------------------

test_that("MetacoreDefine exposes study_level, documents, comments active bindings", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_s3_class(mc$study_level, "tbl_df")
  expect_s3_class(mc$documents, "tbl_df")
  expect_s3_class(mc$comments, "tbl_df")
})

test_that("study_level, documents, comments are read-only", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_error(mc$study_level <- tibble::tibble(), "read only")
  expect_error(mc$documents <- tibble::tibble(), "read only")
  expect_error(mc$comments <- tibble::tibble(), "read only")
})

test_that("all 7 base tables still accessible from MetacoreDefine", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_s3_class(mc$ds_spec, "tbl_df")
  expect_s3_class(mc$ds_vars, "tbl_df")
  expect_s3_class(mc$var_spec, "tbl_df")
  expect_s3_class(mc$value_spec, "tbl_df")
  expect_s3_class(mc$derivations, "tbl_df")
  expect_s3_class(mc$codelist, "tbl_df")
  expect_s3_class(mc$supp, "tbl_df")
})


# NULL inputs produce empty schema tibbles -------------------------------------

test_that("NULL study_level produces empty tibble with correct columns", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(names(mc$study_level), names(define_column_schema()$.study_level))
  expect_equal(nrow(mc$study_level), 0L)
})

test_that("NULL documents produces empty tibble with correct columns", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(names(mc$documents), names(define_column_schema()$.documents))
  expect_equal(nrow(mc$documents), 0L)
})

test_that("NULL comments produces empty tibble with correct columns", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(names(mc$comments), names(define_column_schema()$.comments))
  expect_equal(nrow(mc$comments), 0L)
})


# Provided data is stored correctly --------------------------------------------

test_that("provided study_level data is stored with correct values", {
  sl <- tibble::tibble(
    study_name        = "CDISC01",
    study_description = "CDISC Pilot Study",
    protocol_name     = "CDISC01",
    standard_name     = "ADaM",
    standard_version  = "1.1",
    define_version    = "2.0",
    language          = "en"
  )
  mc <- suppressWarnings(
    do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE, study_level = sl)))
  )

  expect_equal(nrow(mc$study_level), 1L)
  expect_equal(mc$study_level$study_name |> toString(), "CDISC01")
  expect_equal(mc$study_level$standard_name |> toString(), "ADaM")
  expect_equal(mc$study_level$define_version |> toString(), "2.0")
})

test_that("provided documents data is stored correctly", {
  docs <- tibble::tibble(
    document_id = "ADRG",
    title       = "Analysis Data Reviewer's Guide",
    href        = "adrg.pdf"
  )
  mc <- suppressWarnings(
    do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE, documents = docs)))
  )

  expect_equal(nrow(mc$documents), 1L)
  expect_equal(mc$documents$document_id |> toString(), "ADRG")
  expect_equal(mc$documents$href |> toString(), "adrg.pdf")
})

test_that("provided comments data is stored correctly", {
  spec <- make_ae_spec()
  spec$value_spec$comment_id <- "COM01"
  cmts <- tibble::tibble(comment_id = "COM01", comment = "A variable note")

  mc <- suppressWarnings(
    metacore(
      spec$ds_spec, spec$ds_vars, spec$var_spec, spec$value_spec,
      spec$derivations, spec$codelist, spec$supp,
      comments = cmts, define_fields = TRUE
    )
  )

  expect_equal(nrow(mc$comments), 1L)
  expect_equal(mc$comments$comment_id |> toString(), "COM01")
  expect_equal(mc$comments$comment |> toString(), "A variable note")
})


# Column labels ----------------------------------------------------------------

test_that("study_level columns have correct labels", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(attr(mc$study_level$study_name, "label"), "Study Name")
  expect_equal(attr(mc$study_level$study_description, "label"), "Study Description")
  expect_equal(attr(mc$study_level$protocol_name, "label"), "Protocol Name")
  expect_equal(attr(mc$study_level$standard_name, "label"), "Standard")
  expect_equal(attr(mc$study_level$standard_version, "label"), "Standard Version")
  expect_equal(attr(mc$study_level$define_version, "label"), "Define Version")
  expect_equal(attr(mc$study_level$language, "label"), "Language")
})

test_that("documents columns have correct labels", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(attr(mc$documents$document_id, "label"), "Document ID")
  expect_equal(attr(mc$documents$title, "label"), "Title")
  expect_equal(attr(mc$documents$href, "label"), "Href")
})

test_that("comments columns have correct labels", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_equal(attr(mc$comments$comment_id, "label"), "Comment ID")
  expect_equal(attr(mc$comments$comment, "label"), "Comment Text")
})


# Extended columns in base tables ----------------------------------------------

test_that("define_fields = TRUE adds class, repeating, reference, purpose to ds_spec", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_true(all(c("class", "repeating", "reference", "purpose") %in% names(mc$ds_spec)))
})

test_that("define_fields = TRUE adds role to ds_vars", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_true("role" %in% names(mc$ds_vars))
})

test_that("define_fields = TRUE adds where_label and comment_id to value_spec", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_true(all(c("where_label", "comment_id") %in% names(mc$value_spec)))
})

test_that("define_fields = TRUE adds method_name, method_type, document_id, pages to derivations", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = TRUE))))

  expect_true(all(c("method_name", "method_type", "document_id", "pages") %in% names(mc$derivations)))
})

test_that("define_fields = FALSE does NOT add extended columns to ds_spec or ds_vars", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = FALSE))))

  expect_false("class" %in% names(mc$ds_spec))
  expect_false("role" %in% names(mc$ds_vars))
  expect_false("where_label" %in% names(mc$value_spec))
  expect_false("method_name" %in% names(mc$derivations))
})

test_that("define_fields = FALSE ds_spec columns match base schema exactly", {
  mc <- suppressWarnings(do.call(metacore, c(make_ae_spec(), list(define_fields = FALSE))))

  expect_equal(names(mc$ds_spec), names(base_column_schema()$.ds_spec))
})


# Validation -------------------------------------------------------------------

test_that("all-empty MetacoreDefine tables produce a warning", {
  schema <- define_column_schema()

  expect_warning(
    metacore(
      ds_spec = schema$.ds_spec,
      ds_vars = schema$.ds_vars,
      var_spec = schema$.var_spec,
      value_spec = schema$.value_spec,
      derivations = schema$.derivations,
      codelist = schema$.codelist,
      supp = schema$.supp,
      define_fields = TRUE
    ),
    "all datasets are empty"
  )
})

test_that("comment_check warns when comment_id in value_spec has no matching comment", {
  spec <- make_ae_spec()
  spec$value_spec$comment_id <- "MISSING"
  # Non-empty comments table (required for comment_check to run), but ID doesn't match
  cmts <- tibble::tibble(comment_id = "OTHER", comment = "A different comment")

  expect_warning(
    metacore(
      spec$ds_spec, spec$ds_vars, spec$var_spec, spec$value_spec,
      spec$derivations, spec$codelist, spec$supp,
      comments = cmts, define_fields = TRUE
    ),
    "referenced but not found"
  )
})

test_that("comment_check warns when a comment row is never referenced in value_spec", {
  spec <- make_ae_spec()
  cmts <- tibble::tibble(comment_id = "UNUSED", comment = "No one references this")

  expect_warning(
    metacore(
      spec$ds_spec, spec$ds_vars, spec$var_spec, spec$value_spec,
      spec$derivations, spec$codelist, spec$supp,
      comments = cmts, define_fields = TRUE
    ),
    "never used"
  )
})

test_that("comment_check is skipped when comments table is empty", {
  spec <- make_ae_spec()

  # Empty comments table → comment_check not invoked; no comment-related warning
  warnings_raised <- character()
  withCallingHandlers(
    suppressMessages(
      metacore(
        spec$ds_spec, spec$ds_vars, spec$var_spec, spec$value_spec,
        spec$derivations, spec$codelist, spec$supp,
        comments = tibble::tibble(comment_id = character(), comment = character()),
        define_fields = TRUE, verbose = "silent"
      )
    ),
    warning = function(w) {
      warnings_raised <<- c(warnings_raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("referenced but not found|never used", warnings_raised)))
})


# Integration: select_dataset --------------------------------------------------


# select_dataset_define --------------------------------------------------------

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

test_that("select_dataset on MetacoreDefine returns DatasetMeta with define class vector", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))

  expect_equal(class(ae), c("DatasetMeta", "MetacoreDefine", "Metacore", "R6"))
  expect_true(is_DatasetMeta(ae))
})

test_that("select_dataset on MetacoreDefine filters base tables to the requested dataset", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))

  expect_equal(unique(ae$ds_spec$dataset), "AE")
  expect_false("DM" %in% ae$ds_vars$dataset)
  expect_false("DM" %in% ae$value_spec$dataset)
})

test_that("select_dataset on MetacoreDefine preserves study_level unchanged", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))

  expect_equal(nrow(ae$study_level), 1L)
  expect_equal(ae$study_level$study_name |> toString(), "PILOT01")
})

test_that("select_dataset on MetacoreDefine retains only comments referenced by filtered value_spec", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))
  dm <- suppressWarnings(select_dataset(mc, "DM", verbose = "silent"))

  expect_equal(ae$comments$comment_id |> toString(), "COM.AE")
  expect_equal(nrow(dm$comments), 0L)
})

test_that("select_dataset on MetacoreDefine retains only documents referenced by filtered derivations", {
  spec <- make_define_spec()
  spec$derivations <- tibble::tibble(
    derivation_id = "D.AE",
    derivation    = "Derived from CRF",
    method_name   = "M1",
    method_type   = "Computation",
    document_id   = "ADRG",
    pages         = "1"
  )
  spec$value_spec$derivation_id <- c("D.AE", NA, NA)
  spec$documents <- tibble::tibble(
    document_id = c("ADRG", "OTHER"),
    title       = c("Reviewer Guide", "Other Doc"),
    href        = c("adrg.pdf", "other.pdf")
  )
  mc <- suppressWarnings(
    do.call(metacore, c(spec, list(define_fields = TRUE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))
  dm <- suppressWarnings(select_dataset(mc, "DM", verbose = "silent"))

  expect_equal(ae$documents$document_id |> toString(), "ADRG")
  expect_equal(nrow(dm$documents), 0L)
})

test_that("select_dataset on MetacoreDefine errors for an unknown dataset", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  expect_error(select_dataset(mc, "XX", verbose = "silent"), "XX")
})

test_that("select_dataset on MetacoreDefine with simplify = TRUE returns a flat tibble", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_define_spec(), list(define_fields = TRUE, verbose = "silent")))
  )
  ae_simple <- suppressWarnings(
    select_dataset(mc, "AE", simplify = TRUE, verbose = "silent")
  )

  expect_s3_class(ae_simple, "tbl_df")
  expect_true("variable" %in% names(ae_simple))
  expect_true("dataset" %in% names(ae_simple))
})

test_that("select_dataset on base Metacore returns DatasetMeta with base class vector", {
  mc <- suppressWarnings(
    do.call(metacore, c(make_ae_spec(), list(define_fields = FALSE, verbose = "silent")))
  )
  ae <- suppressWarnings(select_dataset(mc, "AE", verbose = "silent"))

  expect_equal(class(ae), c("DatasetMeta", "Metacore", "R6"))
  expect_true(is_DatasetMeta(ae))
  expect_false(inherits(ae, "MetacoreDefine"))
})
