#' Column schema for the original (pre-Define.xml) metacore schema
#'
#' Returns the 7-table schema that existed before Define.xml-specific fields
#' were added. Used when `define_fields = FALSE` to preserve backwards
#' compatibility for callers who do not need the Define.xml extensions.
#'
#' @return named list of zero-row schema tibbles, one per table
#' @export
base_column_schema <- function() {
  list(
    .ds_spec = tibble(
      dataset = character(), structure = character(), label = character()
    ),
    .ds_vars = tibble(
      dataset = character(), variable = character(), order = integer(),
      mandatory = logical(), key_seq = integer(), core = character(),
      supp_flag = logical()
    ),
    .var_spec = tibble(
      variable = character(), length = integer(), label = character(),
      type = character(), format = character(), common = logical()
    ),
    .value_spec = tibble(
      dataset = character(), variable = character(), origin = character(), type = character(),
      code_id = character(), sig_dig = integer(), where = character(), derivation_id = character()
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
#' @export
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


#' Default column regex mappings for the base (pre-Define.xml) schema
#'
#' Returns named regex vectors used by the `spec_type_to_*` family to locate
#' and rename columns when reading Excel specifications. Mirrors the structure
#' of `base_column_schema()` — one entry per table, keys matching column names.
#'
#' @return named list of named character vectors, one per table
#' @export
base_col_regex <- function() {
  list(
    .ds_spec = c(
      "dataset"   = "[N|n]ame|[D|d]ataset|[D|d]omain",
      "structure" = "[S|s]tructure",
      "label"     = "[L|l]abel|[D|d]escription"
    ),
    .ds_vars = c(
      "dataset"   = "[D|d]ataset|[D|d]omain",
      "variable"  = "[V|v]ariable [[N|n]ame]?|[V|v]ariables?",
      "order"     = "[V|v]ariable [O|o]rder|[O|o]rder",
      "mandatory" = "[K|k]eep|[M|m]andatory"
    ),
    .var_spec = c(
      "variable" = "[N|n]ame|[V|v]ariables?",
      "length"   = "[L|l]ength",
      "label"    = "[L|l]abel",
      "type"     = "[T|t]ype",
      "dataset"  = "[D|d]ataset|[D|d]omain",
      "format"   = "[F|f]ormat"
    ),
    .value_spec = c(
      "dataset"       = "[D|d]ataset|[D|d]omain",
      "variable"      = "[N|n]ame|[V|v]ariables?",
      "origin"        = "[O|o]rigin",
      "type"          = "[T|t]ype",
      "code_id"       = "[C|c]odelist|Controlled Term",
      "sig_dig"       = "[S|s]ignificant",
      "where"         = "[W|w]here",
      "derivation_id" = "[M|m]ethod",
      "predecessor"   = "[P|p]redecessor"
    ),
    .derivations = c(
      "derivation_id" = "ID",
      "derivation"    = "[D|d]efinition|[D|d]escription"
    )
  )
}


#' Default column regex mappings including Define.xml-specific fields
#'
#' Extends `base_col_regex()` with the additional columns required for
#' Define.xml generation. Mirrors the structure of `define_column_schema()`.
#'
#' @return named list of named character vectors, one per table
#' @export
define_col_regex <- function() {
  regex <- base_col_regex()
  regex$.ds_spec <- c(
    regex$.ds_spec,
    "class"     = "[C|c]lass",
    "repeating" = "[R|r]epeating",
    "reference" = "[R|r]eference [D|d]ata",
    "purpose"   = "[P|p]urpose"
  )
  regex$.ds_vars <- c(regex$.ds_vars, "role" = "[R|r]ole")
  regex$.value_spec <- c(
    regex$.value_spec,
    "where_label" = "[L|l]abel|[D|d]escription"
    # comment_id is populated via a separate join in spec_type_to_value_spec,
    # not through create_tbl, so it is intentionally excluded here.
  )
  regex$.derivations <- c(
    regex$.derivations,
    "method_name" = "[N|n]ame",
    "method_type" = "[T|t]ype",
    "document_id" = "[D|d]ocument",
    "pages"       = "[P|p]ages"
  )
  # Define-only tables (no base equivalent)
  regex$.documents <- c(
    "document_id" = "ID",
    "title"       = "[T|t]itle",
    "href"        = "[H|h]ref"
  )
  regex$.comments <- c(
    "comment_id" = "ID",
    "comment"    = "[D|d]escription"
  )
  regex
}


#' Columns present only in the define schema for a given table
#'
#' Returns the column names that exist in `define_column_schema()` but not in
#' `base_column_schema()` for the named table. Used by the `spec_type_to_*`
#' family to derive which columns are optional (i.e. define-specific) without
#' maintaining a separate hardcoded list alongside the schema.
#'
#' @param table_name unqualified table name, e.g. `"ds_spec"` (no leading dot)
#' @return character vector of define-only column names, or `character(0)` if
#'   the table is identical in both schemas (or absent from both)
#' @noRd
define_only_cols <- function(table_name) {
  tbl <- paste0(".", table_name)
  define <- define_column_schema()
  base <- base_column_schema()
  if (!tbl %in% names(define)) {
    return(character())
  }
  if (!tbl %in% names(base)) {
    return(names(define[[tbl]]))
  }
  setdiff(names(define[[tbl]]), names(base[[tbl]]))
}


#' Column Names by dataset
#'
#' @param schema Optional named list of schema tibbles from
#'   `define_column_schema()` or `base_column_schema()`. Defaults to the full
#'   extended schema.
#' @return list of column names by dataset
#' @noRd
col_vars <- function(schema = NULL) {
  if (is.null(schema)) schema <- define_column_schema()
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


#' Reorder columns to match column_schema order
#'
#' @param .data dataset to reorder
#' @param table_name name of the table (e.g., "ds_spec", "ds_vars")
#'
#' @return dataset with columns in schema order
#' @noRd
reorder_by_schema <- function(.data, table_name) {
  schema <- define_column_schema()
  table_key <- paste0(".", table_name)

  if (!table_key %in% names(schema)) {
    return(.data)
  }

  expected_cols <- names(schema[[table_key]])
  current_cols <- names(.data)

  # Keep only columns that exist in current data, in schema order
  cols_to_keep <- intersect(expected_cols, current_cols)

  .data |> select(all_of(cols_to_keep))
}
