#' Column schema for the original (pre-Define.xml) metacore schema
#'
#' Returns the 7-table schema that existed before Define.xml-specific fields
#' were added. Used when `define_fields = FALSE` to preserve backwards
#' compatibility for callers who do not need the Define.xml extensions.
#'
#' @return named list of zero-row prototype tibbles, one per table
#' @noRd
base_column_schema <- function() {
   list(
      .ds_spec = tibble(
         dataset = character(), structure = character(), label = character()
      ),
      .ds_vars = tibble(
         dataset = character(), variable = character(), key_seq = integer(),
         order = integer(), mandatory = logical(), core = character(),
         supp_flag = logical()
      ),
      .var_spec = tibble(
         variable = character(), length = integer(), label = character(),
         type = character(), common = logical(), format = character()
      ),
      .value_spec = tibble(
         dataset = character(), variable = character(), type = character(),
         origin = character(), sig_dig = integer(), code_id = character(),
         where = character(), derivation_id = character()
      ),
      .derivations = tibble(
         derivation_id = character(), derivation = character()
      ),
      .codelist = tibble(
         code_id = character(), name = character(), type = character(), codes = list()
      ),
      .supp = tibble(
         dataset = character(), variable = character(), idvar = character(),
         qeval = character()
      )
   )
}


#' Column schema for every metacore table
#'
#' Single source of truth for the columns (and their types) of each table held
#' in a metacore object. Each element is a zero-row tibble used both to validate
#' column names (via `col_vars()``) and to back-fill missing columns when a
#' metacore object is initialised (via `fill_cols()`). New, optional columns
#' added here are automatically tolerated on existing objects.
#'
#' @return named list of zero-row schema tibbles, one per table
#' @noRd
define_column_schema <- function() {
   schema <- base_column_schema()

   # Extend shared tables with define-specific columns
   schema$.ds_spec <- tibble(
      !!!schema$.ds_spec,
      class = character(), repeating = logical(), reference = logical(),
      purpose = character()
   )
   schema$.ds_vars <- tibble(!!!schema$.ds_vars, role = character())
   schema$.value_spec <- tibble(
      !!!schema$.value_spec[1:7],
      where_label = character(), derivation_id = character(),
      comment_id = character()
   )
   schema$.derivations <- tibble(
      !!!schema$.derivations,
      method_name = character(), method_type = character(),
      document_id = character(), pages = character()
   )

   # Define-only tables
   schema$.study_level <- tibble(
      study_name = character(), study_description = character(),
      protocol_name = character(), standard_name = character(),
      standard_version = character(), define_version = character(),
      language = character()
   )
   schema$.documents <- tibble(
      document_id = character(), title = character(), href = character()
   )
   schema$.comments <- tibble(
      comment_id = character(), comment = character()
   )

   schema
}


#' Column Names by dataset
#'
#' @param schema Optional named list of prototype tibbles from
#'   `define_column_schema()` or `base_column_schema()`. Defaults to the full
#'   extended schema.
#' @return list of column names by dataset
#' @noRd
col_vars <- function(schema = NULL) {
   if (is.null(schema)) schema <- define_column_schema()
   # study_level, documents, and comments are study-wide tables that are not
   # name-validated against the per-dataset tables, so they are excluded here
   schema$.study_level <- NULL
   schema$.documents <- NULL
   schema$.comments <- NULL
   lapply(schema, names)
}


#' Back-fill missing columns against a schema
#'
#' Adds any columns present in `schema` but absent from `.data`, using the
#' schema's type and filling with `NA`. Existing columns are left untouched.
#' A `NULL` input returns the empty schema This makes newly added schema
#' columns optional for callers building metacore objects.
#'
#' @param .data a data frame (or `NULL`)
#' @param schema a zero-row schema tibble from either `base_column_schema()` or
#'   `define_column_schema()`
#' @return `.data` with all schema columns present
#' @noRd
fill_cols <- function(.data, schema) {
   if (is.null(.data)) {
      return(schema)
   }
   missing <- setdiff(names(schema), names(.data))
   for (col in missing) {
      .data[[col]] <- schema[[col]][seq_len(nrow(.data))]
   }
   .data
}
