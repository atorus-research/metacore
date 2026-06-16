#' Extended Metacore initializer (Define.xml schema: base tables + 3 extra tables)
#' @family Metacore
#' @noRd
MetaCoreDefine_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp, study_level = NULL,
                                      documents = NULL, comments = NULL, quiet = deprecated(), verbose = "message") {
  if (lifecycle::is_present(quiet)) {
    deprecate_soft(
      when = "0.3.0",
      what = "MetaCoreDefine_initialize(quiet)",
      with = "MetaCoreDefine_initialize(verbose)"
    )
  } else {
    quiet <- FALSE
  }

  full_schema <- define_column_schema()

  .metacore_init_base_tables(
    private, full_schema, ds_spec, ds_vars, var_spec,
    value_spec, derivations, codelist, supp
  )

  private$.study_level <- fill_cols(study_level, full_schema$.study_level) |>
    add_labs(
      study_name = "Study Name", study_description = "Study Description",
      protocol_name = "Protocol Name", standard_name = "Standard",
      standard_version = "Standard Version", define_version = "Define Version",
      language = "Language"
    )

  private$.documents <- fill_cols(documents, full_schema$.documents) |>
    add_labs(document_id = "Document ID", title = "Title", href = "Href")

  private$.comments <- fill_cols(comments, full_schema$.comments) |>
    add_labs(comment_id = "Comment ID", comment = "Comment Text")

  self$validate()

  if (inherits_only(self, c("MetacoreDefine", "Metacore", "R6"))) {
    private$.greet(quiet)
  }
}

#' Extended Metacore validation — checks all tables including Define.xml tables
#' @family Metacore
#' @noRd
MetaCoreDefine_validate <- function() {
  if (var_name_check(private, define_fields = TRUE)) {
    if (nrow(private$.ds_spec) == 0 &
      nrow(private$.ds_vars) == 0 &
      nrow(private$.var_spec) == 0 &
      nrow(private$.value_spec) == 0 &
      nrow(private$.derivations) == 0 &
      nrow(private$.codelist) == 0 &
      nrow(private$.supp) == 0 &
      nrow(private$.study_level) == 0 &
      nrow(private$.documents) == 0 &
      nrow(private$.comments) == 0) {
      cli_warn("Other checks were not performed, because all datasets are empty",
        call. = FALSE
      )
    } else {
      check_columns(
        private$.ds_spec, private$.ds_vars, private$.var_spec,
        private$.value_spec, private$.derivations, private$.codelist,
        comments = private$.comments
      )
      ds_vars_check(private$.ds_vars, private$.var_spec)
      value_check(private$.ds_vars, private$.value_spec)
      derivation_check(private$.value_spec, private$.derivations)
      codelist_check(private$.value_spec, private$.codelist)
      if (nrow(private$.comments) > 0) {
        comment_check(private$.value_spec, private$.comments)
      }
      if (nrow(private$.supp) > 0) {
        check_columns(supp = private$.supp)
        supp_check(private$.ds_vars, private$.supp)
      }
    }
  } else {
    cli_warn("Other checks were not performed, because column names were incorrect",
      call. = FALSE
    )
  }
}

#' Extended Metacore filter — subsets all tables to a single dataset,
#' then trims documents and comments to only those still referenced.
#' @family Metacore
#' @noRd
MetaCoreDefine_filter <- function(value) {
  super$metacore_filter(value)

  # Trim documents to those referenced by the now-filtered derivations
  if ("document_id" %in% names(private$.derivations)) {
    used_docs <- private$.derivations |>
      filter(!is.na(.data$document_id)) |>
      distinct(.data$document_id)
    private$.documents <- private$.documents |>
      right_join(used_docs, by = "document_id", multiple = "all")
  }

  # Trim comments to those referenced by the now-filtered value_spec
  if ("comment_id" %in% names(private$.value_spec)) {
    used_comments <- private$.value_spec |>
      filter(!is.na(.data$comment_id)) |>
      distinct(.data$comment_id)
    private$.comments <- private$.comments |>
      right_join(used_comments, by = "comment_id", multiple = "all")
  }
}


#' Set study-level metadata on a MetacoreDefine object
#'
#' Populates the `study_level` table, which cannot be derived from a Pinnacle 21
#' spec sheet and must therefore be supplied by the caller after construction.
#' Each argument corresponds to one column in `define_column_schema()$.study_level`.
#' Omitted arguments are stored as `NA`.
#'
#' @param study_name       Study name (single string).
#' @param study_description Study description (single string).
#' @param protocol_name    Protocol name (single string).
#' @param standard_name    Standard name, e.g. `"CDISC/NCI"` (single string).
#' @param standard_version Standard version, e.g. `"2.0"` (single string).
#' @param define_version   Define-XML version, e.g. `"2.1"` (single string).
#' @param language         Language code, e.g. `"en"` (single string).
#'
#' @return The object, invisibly, to allow method chaining.
#' @family Metacore
#' @noRd
MetaCoreDefine_set_study_level <- function(
    study_name = NA_character_,
    study_description = NA_character_,
    protocol_name = NA_character_,
    standard_name = NA_character_,
    standard_version = NA_character_,
    define_version = NA_character_,
    language = NA_character_) {
  args <- list(
    study_name = study_name, study_description = study_description,
    protocol_name = protocol_name, standard_name = standard_name,
    standard_version = standard_version, define_version = define_version,
    language = language
  )

  bad <- purrr::keep(args, function(x) !(length(x) == 1L && (is.na(x) || is.character(x))))
  if (length(bad) > 0) {
    cli_abort(c(
      "x" = "Each argument to {.fn set_study_level} must be a single character string or {.val NA}.",
      "i" = "Problem argument{?s}: {.arg {names(bad)}}"
    ))
  }

  schema <- define_column_schema()$.study_level

  private$.study_level <- tibble::tibble(!!!args) |>
    fill_cols(schema) |>
    add_labs(
      study_name = "Study Name", study_description = "Study Description",
      protocol_name = "Protocol Name", standard_name = "Standard",
      standard_version = "Standard Version", define_version = "Define Version",
      language = "Language"
    )

  invisible(self)
}


#' Extended Metacore R6 Class (Define.xml schema: adds study_level, documents, comments)
#' @family Metacore
#' @noRd
MetaCoreDefine <- R6::R6Class(
  "MetacoreDefine",
  inherit = MetaCore,
  public = list(
    initialize      = MetaCoreDefine_initialize,
    validate        = MetaCoreDefine_validate,
    metacore_filter = MetaCoreDefine_filter,
    set_study_level = MetaCoreDefine_set_study_level
  ),
  private = list(
    .study_level = tibble(),
    .documents   = tibble(),
    .comments    = tibble()
  ),
  active = list(
    study_level = readonly("study_level"),
    documents   = readonly("documents"),
    comments    = readonly("comments")
  )
)
