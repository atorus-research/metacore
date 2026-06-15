#' Specification document to metacore object
#'
#' This function takes the location of an excel specification document and reads
#' it in as a meta core object. At the moment it only supports specification in
#' the format of pinnacle 21 specifications. But, the section level spec builder can
#' be used as building blocks for bespoke specification documents.
#'
#' @param path string of file location
#' @param quiet `r lifecycle::badge("superseded")` Option to quietly load in, this
#'   will suppress warnings, but not errors. Expects either `TRUE` or `FALSE`.
#'   Default behaviour is `FALSE`. From v0.3.0 this argument is deprecated in favour of
#'   `verbose`.
#' @param where_sep_sheet Option to tell if the where is in a separate sheet,
#'   like in older p21 specs or in a single sheet like newer p21 specs.
#' @param define_fields `r lifecycle::badge("experimental")`logical; set to `TRUE` to
#'   include the extended columns required for Define.xml generation (`class`, `repeating`,
#'   `reference`, `purpose` in `ds_spec`; `role` in `ds_vars`; `where_label` and
#'   `comment_id` in `value_spec`; `method_name`, `method_type`, `document_id`,
#'   `pages` in `derivations`) as well as the `study_level`, `documents`, and
#'   `comments` tables. Defaults to `FALSE` to preserve the original schema.
#' @param verbose A character string specifying the desired verbosity level.
#'   Must be one of:
#'   \describe{
#'     \item{"message"}{(default) Messages and warnings are handled normally.}
#'     \item{"warn"}{Messages are suppressed, but warnings are allowed.}
#'     \item{"collapse"}{Warnings are collapsed into a single message indicating the
#'     number of suppressed warnings.}
#'     \item{"silent"}{Both messages and warnings are suppressed.}
#'   }
#'
#' @return given a spec document it returns a metacore object
#'
#' @examples
#' # Run `spec_to_metacore` with `verbose = "collapse"`
#' spec_path <- metacore_example("p21_mock.xlsx")
#' metacore <- spec_to_metacore(
#'   path = spec_path,
#'   verbose = "collapse"
#' )
#'
#' # Run `spec_to_metacore` with `verbose = "warn"`
#' metacore <- spec_to_metacore(
#'   path = spec_path,
#'   verbose = "warn"
#' )
#'
#' @export
spec_to_metacore <- function(path, quiet = deprecated(), where_sep_sheet = TRUE,
                             define_fields = FALSE, verbose = "message") {
  if (lifecycle::is_present(quiet)) {
    deprecate_soft(when = "0.3.0", what = "spec_to_metacore(quiet)", with = "spec_to_metacore(verbose)")
  } else {
    quiet <- FALSE
  }

  with_verbosity(
    {
      doc <- read_all_sheets(path)

      if (spec_type(path) != "by_type") {
        cli_abort(
          "This specification format is not currently supported. You will need to write your own reader"
        )
      }

      # Core tables — always built regardless of define_fields
      ds_spec <- spec_type_to_ds_spec(doc, define_fields = define_fields)
      ds_vars <- spec_type_to_ds_vars(doc, define_fields = define_fields)
      var_spec <- spec_type_to_var_spec(doc)
      value_spec <- spec_type_to_value_spec(doc, where_sep_sheet = where_sep_sheet, define_fields = define_fields)
      derivations <- spec_type_to_derivations(doc, define_fields = define_fields)
      codelist <- spec_type_to_codelist(doc)

      # Define.xml-only tables — skipped when define_fields = FALSE
      documents <- if (define_fields) spec_type_to_documents(doc) else NULL
      comments <- if (define_fields) spec_type_to_comments(doc) else NULL
      supp <- base_column_schema()$.supp

      # Supplemental variables — skipped when define_fields = FALSE
      if (define_fields) {
        supp <- spec_type_to_supp(
          doc,
          where_sep_sheet = where_sep_sheet,
          var_spec = var_spec,
          value_spec = value_spec,
          codelist = codelist,
          comments = comments
        )

        ds_vars <- add_supp_to_table(supp, ds_vars, define_column_schema()$.ds_vars)
        var_spec <- add_supp_to_table(supp, var_spec, define_column_schema()$.var_spec)
        value_spec <- add_supp_to_table(supp, value_spec, define_column_schema()$.value_spec)
        supp <- reorder_by_schema(supp, "supp")
      }

      mc <- metacore(
        ds_spec, ds_vars, var_spec, value_spec, derivations, codelist,
        supp = supp,
        documents = documents,
        comments = comments,
        define_fields = define_fields,
        quiet = quiet,
        verbose = verbose
      )

      if (quiet) invisible(mc) else mc
    },
    quiet,
    verbose
  )
}


#' Check the type of spec document
#'
#' @param path file location as a string
#'
#' @return returns string indicating the type of spec document
#' @export
#'
spec_type <- function(path) {
  sheets <- excel_sheets(path)
  if (!any(sheets |> str_detect("[D|d]omains?|[D|d]atasets?"))) {
    cli_abort("File does not contain a Domain/Datasets tab, which is needed. Please either modify the spec document or write a reader (see documentation for more information)")
  } else if (any(sheets |> str_detect("ADSL|DM"))) {
    type <- "by_ds"
  } else if (any(sheets |> str_detect("[V|v]ariables?"))) {
    type <- "by_type"
  } else {
    cli_abort("File in an unknown format. Please either modify the spec document or write a reader (see documentation for more information)")
  }
  type
}


#' Read in all Sheets
#'
#' Given a path to a file, this function reads in all sheets of an excel file
#'
#' @param path string of the file path
#' @export
#'
#' @return a list of datasets
read_all_sheets <- function(path) {
  sheets <- excel_sheets(path)
  all_dat <- sheets |>
    map(~ read_excel(path, sheet = ., col_types = "text", progress = FALSE))
  names(all_dat) <- sheets
  all_dat
}


#' Spec to ds_spec
#'
#' Builds the `ds_spec` table from a named list of Excel sheets produced by
#' [read_all_sheets()]. `ds_spec` holds **one row per dataset** and captures
#' dataset-level metadata.
#'
#' @details
#' ## How it works
#'
#' The function searches every sheet in `doc` for one that contains all
#' required columns (matched by regular expression), renames those columns to
#' the standard metacore names, and returns a single tidy tibble. If no sheet
#' contains all required columns an informative error is raised; if more than
#' one sheet matches a warning is issued listing the candidates.
#'
#' ## Output columns
#'
#' | Column | Description |
#' |:-------|:------------|
#' | `dataset` | Short dataset name, e.g. `"ADSL"` |
#' | `structure` | Dataset structure, e.g. `"ONE RECORD PER SUBJECT"` |
#' | `label` | Human-readable dataset label |
#'
#' ## Default column matching
#'
#' When `cols = NULL` (the default), patterns from [base_col_regex()] are used.
#' These are designed to match standard Pinnacle 21 headings:
#'
#' - **`dataset`** — heading matches `"Name"`, `"Dataset"`, or `"Domain"`
#' - **`structure`** — heading matches `"Structure"`
#' - **`label`** — heading matches `"Label"` or `"Description"`
#'
#' If your specification uses different headings, supply a named character
#' vector to `cols`. Names must match the output column names listed in the
#' table above. Values are treated as regular expressions:
#'
#' ```r
#' spec_type_to_ds_spec(doc, cols = c(
#'   "dataset"   = "Study Domain",
#'   "structure" = "Record Structure",
#'   "label"     = "Long Label"
#' ))
#' ```
#'
#' You only need to supply entries for columns whose headings differ from the
#' defaults — omitted columns will still use the built-in pattern.
#'
#' ## Additional Define.xml columns (`define_fields = TRUE`)
#'
#' Set `define_fields = TRUE` to append four extra columns required for
#' Define.xml 2.0 generation. Patterns are drawn from [define_col_regex()]:
#'
#' | Column | Description | Default heading match |
#' |:-------|:------------|:----------------------|
#' | `class` | Dataset class (e.g. `"BASIC DATA STRUCTURE"`) | `"Class"` |
#' | `repeating` | Is the dataset repeating? (`TRUE`/`FALSE`) | `"Repeating"` |
#' | `reference` | Is it reference data? (`TRUE`/`FALSE`) | `"Reference Data"` |
#' | `purpose` | Purpose (e.g. `"Analysis"`, `"Tabulation"`) | `"Purpose"` |
#'
#' @param doc Named list of data frames produced by [read_all_sheets()].
#' @param cols Named character vector of regular expressions mapping output
#'   column names to column headings in your specification. When `NULL`
#'   (default), patterns from [base_col_regex()] are used (or [define_col_regex()]
#'   when `define_fields = TRUE`). Valid names are `dataset`, `structure`,
#'   `label`; plus `class`, `repeating`, `reference`, `purpose` when
#'   `define_fields = TRUE`.
#' @param sheet Regular expression used to restrict which sheets are searched.
#'   `NULL` (default) searches all sheets.
#' @param define_fields `r lifecycle::badge("experimental")` Logical; when
#'   `TRUE`, additional Define.xml columns (`class`, `repeating`, `reference`,
#'   `purpose`) are appended to the result. Defaults to `FALSE`.
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_ds_spec <- function(
    doc,
    cols = base_col_regex()$.ds_spec,
    sheet = NULL,
    define_fields = FALSE) {
  cols <- (if (define_fields) define_col_regex()$.ds_spec else cols)

  # Validate against the full schema so typos are caught regardless of mode
  valid_names <- names(define_column_schema()$.ds_spec)
  if (!all(names(cols) %in% valid_names) | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var ds_spec}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {valid_names}}"
    ))
  }

  # Drop define-specific col mappings from cols when define_fields = FALSE
  active_schema <- if (define_fields) define_column_schema() else base_column_schema()
  cols <- cols[names(cols) %in% names(active_schema$.ds_spec)]

  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
  }

  # purpose is optional: if missing from all sheets, warn and let fill_cols add NA
  if (define_fields && "purpose" %in% names(cols)) {
    purpose_regex <- cols[["purpose"]]
    purpose_found <- doc |>
      purrr::map_lgl(~ any(str_detect(names(.), purpose_regex))) |>
      any()
    if (!purpose_found) {
      cli_warn(c(
        "The {.val purpose} column was not found in the spec sheet.",
        "i" = "The {.field purpose} column will be filled with {.val NA}."
      ))
      cols <- cols[names(cols) != "purpose"]
    }
  }

  out <- create_tbl(doc, cols, context = "spec_type_to_ds_spec", schema = active_schema$.ds_spec) |>
    distinct()

  if (define_fields) {
    out <- mutate(
      out,
      repeating = yn_to_tf(.data$repeating),
      reference = yn_to_tf(.data$reference)
    )
  }

  reorder_by_schema(out, "ds_spec")
}

#' Spec to ds_vars
#'
#' Builds the `ds_vars` table from a named list of Excel sheets produced by
#' [read_all_sheets()]. `ds_vars` holds **one row per dataset–variable
#' combination** and acts as the bridge between dataset-level and
#' variable-level metadata.
#'
#' @details
#' ## How it works
#'
#' The function searches every sheet in `doc` for one that contains all
#' required columns, renames them to the standard metacore names, and returns a
#' single tidy tibble. Key sequence information is handled separately because
#' it is commonly stored on a different sheet (e.g. the Domains sheet) from the
#' variable list.
#'
#' ## Output columns
#'
#' | Column | Description |
#' |:-------|:------------|
#' | `dataset` | Dataset name, e.g. `"ADSL"` |
#' | `variable` | Variable name, e.g. `"USUBJID"` |
#' | `order` | Column order within the dataset (integer) |
#' | `mandatory` | `TRUE` if NULL values are not permitted (from Define-XML v2.1) |
#' | `key_seq` | Integer sort key; `NA` if the variable is not a key variable |
#' | `core` | ADaM core status: `"Required"`, `"Expected"`, or `"Permissible"` |
#' | `supp_flag` | `TRUE` if the variable is a supplemental qualifier |
#'
#' ## Default column matching
#'
#' When `cols = NULL` (the default), patterns from [base_col_regex()] are used.
#' These are designed to match standard Pinnacle 21 headings:
#'
#' - **`dataset`** — heading matches `"Dataset"` or `"Domain"`
#' - **`variable`** — heading matches `"Variable Name"` or `"Variables"`
#' - **`order`** — heading matches `"Variable Order"` or `"Order"`
#' - **`mandatory`** — heading matches `"Keep"` or `"Mandatory"`
#'
#' If your specification uses different headings, supply a named character
#' vector to `cols`:
#'
#' ```r
#' spec_type_to_ds_vars(doc, cols = c(
#'   "dataset"   = "Domain",
#'   "variable"  = "Variable Name",
#'   "order"     = "Variable Order",
#'   "mandatory" = "Required"
#' ))
#' ```
#'
#' ## Key sequence
#'
#' The key sequence (`key_seq`) is read **from a separate sheet by default**
#' (`key_seq_sep_sheet = TRUE`), using the column names in `key_seq_cols`. If
#' your specification stores the key sequence on the same sheet as the variable
#' list, set `key_seq_sep_sheet = FALSE` and add a `"key_seq"` entry to `cols`:
#'
#' ```r
#' spec_type_to_ds_vars(doc,
#'   key_seq_sep_sheet = FALSE,
#'   cols = c(
#'     "dataset"   = "Domain",
#'     "variable"  = "Variable Name",
#'     "order"     = "Variable Order",
#'     "mandatory" = "Mandatory",
#'     "key_seq"   = "Key Sequence"
#'   )
#' )
#' ```
#'
#' ## Additional Define.xml columns (`define_fields = TRUE`)
#'
#' Set `define_fields = TRUE` to append one extra column required for
#' Define.xml 2.0 generation. Patterns are drawn from [define_col_regex()]:
#'
#' | Column | Description | Default heading match |
#' |:-------|:------------|:----------------------|
#' | `role` | Variable role (e.g. `"Identifier"`, `"Topic"`) | `"Role"` |
#'
#' @param doc Named list of data frames produced by [read_all_sheets()].
#' @param cols Named character vector of regular expressions mapping output
#'   column names to column headings in your specification. When `NULL`
#'   (default), patterns from [base_col_regex()] are used (or [define_col_regex()]
#'   when `define_fields = TRUE`). Valid names are `dataset`, `variable`,
#'   `order`, `mandatory`; plus `key_seq` when `key_seq_sep_sheet = FALSE`;
#'   plus `role` when `define_fields = TRUE`.
#' @param sheet Regular expression used to restrict which sheets are searched.
#'   Defaults to matching sheets named `"Variables"` or `"Datasets"`.
#' @param key_seq_sep_sheet Logical; `TRUE` (default) reads the key sequence
#'   from a separate sheet using `key_seq_cols`. Set to `FALSE` and include
#'   `"key_seq"` in `cols` when key sequence is on the same sheet as variables.
#' @param key_seq_cols Named character vector identifying the dataset and key
#'   sequence columns when `key_seq_sep_sheet = TRUE`. Defaults to
#'   `c("dataset" = "Dataset", "key_seq" = "Key Variables")`.
#' @param define_fields `r lifecycle::badge("experimental")` Logical; when
#'   `TRUE`, the additional Define.xml column (`role`) is appended to the
#'   result. Defaults to `FALSE`.
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_ds_vars <- function(
    doc,
    cols = base_col_regex()$.ds_vars,
    key_seq_sep_sheet = TRUE,
    key_seq_cols = c(
      "dataset" = "Dataset",
      "key_seq" = "Key Variables"
    ),
    sheet = "[V|v]ar|Datasets",
    define_fields = FALSE) {
  cols <- (if (define_fields) define_col_regex()$.ds_vars else cols)

  # Validate against the full schema so typos are caught regardless of mode
  valid_names <- names(define_column_schema()$.ds_vars)
  name_check <- all(names(cols) %in% valid_names)

  name_check_extra <- ifelse(
    key_seq_sep_sheet,
    key_seq_sep_sheet,
    all(names(key_seq_cols) %in% c("dataset", "key_seq"))
  )

  if (any(!name_check, !name_check_extra, is.null(names(cols)))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var ds_vars}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {valid_names}}"
    ))
  }

  # Drop define-specific col mappings from cols when define_fields = FALSE
  active_schema <- if (define_fields) define_column_schema() else base_column_schema()

  # Sub-setting sheets
  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
  }

  # Get base doc
  out <- create_tbl(doc, cols, context = "spec_type_to_ds_vars", schema = active_schema$.ds_vars)

  # Getting the key seq values
  if (key_seq_sep_sheet) {
    key_seq_df <- create_tbl(doc, key_seq_cols, context = "spec_type_to_ds_vars") |>
      mutate(
        key_seq = str_split(key_seq, ",\\s"),
        key_seq = map(key_seq, function(x) {
          tibble(variable = x) |>
            mutate(key_seq = row_number())
        })
      ) |>
      unnest(key_seq)

    out <- left_join(
      select(out, -key_seq),
      key_seq_df,
      by = c("dataset", "variable")
    )
  }

  out |>
    reorder_by_schema("ds_vars") |>
    distinct() |>
    mutate(
      key_seq = as.integer(.data$key_seq),
      mandatory = yn_to_tf(.data$mandatory),
      order = as.numeric(.data$order)
    )
}


#' Spec to var_spec
#'
#' Builds the `var_spec` table from a named list of Excel sheets produced by
#' [read_all_sheets()]. `var_spec` holds **one row per unique variable name**
#' and stores metadata that is shared across all datasets (label, type, length,
#' format).
#'
#' @details
#' ## How it works
#'
#' The function searches every sheet in `doc` for one that contains all
#' required columns, renames them to the standard metacore names, deduplicates
#' by variable, and returns a single tidy tibble. Because CDISC standards
#' require that a variable's label and length be consistent across datasets,
#' normalising to one row per variable both enforces this rule and reduces
#' object size.
#'
#' ## Output columns
#'
#' | Column | Description |
#' |:-------|:------------|
#' | `variable` | Variable name, e.g. `"USUBJID"` |
#' | `length` | Maximum character length or numeric width (integer) |
#' | `label` | Variable label, e.g. `"Unique Subject Identifier"` |
#' | `type` | Variable type, e.g. `"Char"` or `"Num"` |
#' | `format` | SAS format string, e.g. `"DATE9."` (`NA` if none) |
#' | `common` | `TRUE` if the variable appears in every dataset (auto-computed) |
#'
#' ## Default column matching
#'
#' When `cols = NULL` (the default), patterns from [base_col_regex()] are used.
#' These are designed to match standard Pinnacle 21 headings:
#'
#' - **`variable`** — heading matches `"Name"` or `"Variables"`
#' - **`length`** — heading matches `"Length"`
#' - **`label`** — heading matches `"Label"`
#' - **`type`** — heading matches `"Type"`
#' - **`format`** — heading matches `"Format"`
#'
#' If your specification uses different headings, supply a named character
#' vector to `cols`:
#'
#' ```r
#' spec_type_to_var_spec(doc, cols = c(
#'   "variable" = "Variable Name",
#'   "length"   = "Max Length",
#'   "label"    = "Variable Label",
#'   "type"     = "Data Type",
#'   "format"   = "SAS Format"
#' ))
#' ```
#'
#' ## Handling variables with different metadata across datasets
#'
#' Some variables (e.g. `VISIT`) legitimately have different lengths or labels
#' in different datasets. To handle this, include a `"dataset"` entry in
#' `cols` pointing to the dataset/domain column. The function will then:
#'
#' 1. Qualify any variable with inconsistent metadata as `DOMAIN.VARIABLE`
#'    (e.g. `"ADAE.VISIT"` vs `"ADSL.VISIT"`).
#' 2. Automatically compute the `common` flag — `TRUE` for variables that
#'    appear in every dataset.
#'
#' ```r
#' spec_type_to_var_spec(doc, cols = c(
#'   "variable" = "Variable Name",
#'   "length"   = "Length",
#'   "label"    = "Label",
#'   "type"     = "Type",
#'   "format"   = "Format",
#'   "dataset"  = "Domain"   # enables per-domain deduplication
#' ))
#' ```
#'
#' @param doc Named list of data frames produced by [read_all_sheets()].
#' @param cols Named character vector of regular expressions mapping output
#'   column names to column headings in your specification. When `NULL`
#'   (default), patterns from [base_col_regex()] are used automatically. Valid
#'   names are `variable`, `length`, `label`, `type`, `format`, and the
#'   optional processing column `dataset` (see Details).
#' @param sheet Regular expression used to restrict which sheets are searched.
#'   Defaults to matching sheets named `"Variables"` or similar.
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_var_spec <- function(
    doc,
    cols = base_col_regex()$.var_spec,
    sheet = "[V|v]ar") {
  # "dataset" is a processing-only column (not in schema) used to detect
  # per-domain duplicate variables before it is dropped from the output.
  var_spec_names <- c(names(define_column_schema()$.var_spec), "dataset")

  name_check <- all(names(cols) %in% var_spec_names)
  if (!name_check | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var var_spec}",
      "i" = "The column vector {.arg cols} must be named with {.val {var_spec_names}}",
      "i" = "Additionally, dataset is only used to clarify if information differs by domain."
    ))
  }

  # Filter sheets if specified
  if (!is.null(sheet)) {
    doc <- doc[str_subset(names(doc), sheet)]
  }

  out <- create_tbl(doc, cols, context = "spec_type_to_var_spec", schema = define_column_schema()$.var_spec)

  # Check for duplicate variables without dataset column
  if (!"dataset" %in% names(out)) {
    dups <- out |>
      distinct() |>
      dplyr::count(variable) |>
      filter(n > 1) |>
      pull(variable)

    if (length(dups) > 0) {
      cli_abort(c(
        "x" = "The following variables are repeated with different metadata for different datasets: {dups}",
        "i" = "Please add {.val dataset} = \"<name of dataset column>\" to your named cols vector to correct this.",
        "i" = "E.g. \"dataset\" = \"[D|d]ataset\""
      ))
    }
  } else {
    # Add common flag if not already in cols
    if (!"common" %in% names(cols)) {
      common_vars <- out |>
        distinct(dataset, variable) |>
        tidyr::pivot_wider(names_from = dataset, values_from = dataset, values_fill = NA) |>
        filter(if_all(-variable, ~ !is.na(.))) |>
        mutate(common = TRUE) |>
        select(variable, common)

      out <- out |>
        left_join(common_vars, by = "variable")
      # replace_na(list(common = FALSE))
    }

    # Remove duplicates and qualify variables with dataset if different metadata
    out <- out |>
      group_by(variable) |>
      mutate(
        unique = n_distinct(length, label, type),
        variable = if_else(unique == 1, variable, paste0(dataset, ".", variable)),
        length = as.integer(length)
      ) |>
      distinct(variable, length, label, type, .keep_all = TRUE) |>
      select(-dataset, -unique)
  }

  out |>
    distinct() |>
    ungroup() |>
    reorder_by_schema("var_spec")
}

#' Spec to value_spec
#'
#' Builds the `value_spec` table from a named list of Excel sheets produced by
#' [read_all_sheets()]. `value_spec` holds **one row per dataset–variable
#' combination** and captures the value-level metadata needed for programming
#' and traceability.
#'
#' @details
#' ## How it works
#'
#' The function searches every sheet in `doc` for one that contains all
#' required columns, renames them, and returns a single tidy tibble. Because
#' many specifications only include value-level metadata for *derived* variables
#' and leave collected variables off the VLM sheet, the function automatically
#' backfills all variables from the variables sheet (`var_sheet`) so that every
#' variable has at least one row in `value_spec`.
#'
#' ## Output columns
#'
#' | Column | Description |
#' |:-------|:------------|
#' | `dataset` | Dataset name, e.g. `"ADAE"` |
#' | `variable` | Variable name, e.g. `"AVAL"` |
#' | `origin` | Data origin: `"Derived"`, `"Assigned"`, `"Collected"`, etc. |
#' | `type` | Variable type, e.g. `"Char"` or `"Num"` |
#' | `code_id` | Cross-reference to a codelist entry in `codelist` |
#' | `sig_dig` | Number of significant digits (numeric variables) |
#' | `where` | WHERE clause identifying a parameter row in VLM |
#' | `derivation_id` | Cross-reference to a derivation in `derivations` |
#'
#' ## Default column matching
#'
#' When `cols = NULL` (the default), patterns from [base_col_regex()] are used.
#' These are designed to match standard Pinnacle 21 headings:
#'
#' - **`dataset`** — heading matches `"Dataset"` or `"Domain"`
#' - **`variable`** — heading matches `"Name"` or `"Variables"`
#' - **`origin`** — heading matches `"Origin"`
#' - **`type`** — heading matches `"Type"`
#' - **`code_id`** — heading matches `"Codelist"` or `"Controlled Term"`
#' - **`sig_dig`** — heading matches `"Significant"`
#' - **`where`** — heading matches `"Where"`
#' - **`derivation_id`** — heading matches `"Method"`
#' - **`predecessor`** — heading matches `"Predecessor"` *(processing only —
#'   converted to `derivation_id` entries and dropped from the output)*
#'
#' If your specification uses different headings, supply a named character
#' vector to `cols`:
#'
#' ```r
#' spec_type_to_value_spec(doc, cols = c(
#'   "dataset"       = "Domain",
#'   "variable"      = "Variable Name",
#'   "origin"        = "Source",
#'   "derivation_id" = "Computational Method"
#' ))
#' ```
#'
#' ## WHERE clause location
#'
#' Pinnacle 21 stores WHERE clauses on a **separate** sheet by default.
#' If your specification puts the WHERE clause on the same sheet as the
#' variables, set `where_sep_sheet = FALSE`:
#'
#' ```r
#' spec_type_to_value_spec(doc, where_sep_sheet = FALSE)
#' ```
#'
#' ## Variable sheet backfill
#'
#' `var_sheet` names the sheet used to backfill missing variables. Set to
#' `NULL` if your value tab already has a row for every variable:
#'
#' ```r
#' spec_type_to_value_spec(doc, var_sheet = NULL)
#' ```
#'
#' ## Additional Define.xml columns (`define_fields = TRUE`)
#'
#' Set `define_fields = TRUE` to append two extra columns required for
#' Define.xml 2.0 generation. Patterns are drawn from [define_col_regex()]:
#'
#' | Column | Description | Default heading match |
#' |:-------|:------------|:----------------------|
#' | `where_label` | Human-readable label for the WHERE clause | `"Label"` or `"Description"` |
#' | `comment_id` | Cross-reference to a comment in `comments` | *(joined from the variables sheet, not column-matched)* |
#'
#' @param doc Named list of data frames produced by [read_all_sheets()].
#' @param cols Named character vector of regular expressions mapping output
#'   column names to column headings in your specification. When `NULL`
#'   (default), patterns from [base_col_regex()] are used (or [define_col_regex()]
#'   when `define_fields = TRUE`). Valid names are `dataset`, `variable`,
#'   `origin`, `type`, `code_id`, `sig_dig`, `where`, `derivation_id`, and the
#'   processing column `predecessor`; plus `where_label` when
#'   `define_fields = TRUE`.
#' @param sheet Regular expression used to restrict which sheets are searched.
#'   `NULL` (default) searches all sheets.
#' @param where_sep_sheet Logical; `TRUE` (default) reads WHERE clauses from a
#'   dedicated sheet via `where_cols`. Set to `FALSE` when WHERE information is
#'   on the same sheet as the variables.
#' @param where_cols Named character vector identifying the id and where columns
#'   when `where_sep_sheet = TRUE`. Defaults to
#'   `c("id" = "ID", "where" = c("Variable", "Comparator", "Value"))`.
#' @param var_sheet Regular expression matching the variables sheet used to
#'   backfill any variables not already in the value tab. Set to `NULL` to
#'   skip backfilling. Defaults to matching sheets named `"Variables"`.
#' @param define_fields `r lifecycle::badge("experimental")` Logical; when
#'   `TRUE`, additional Define.xml columns (`where_label`, `comment_id`) are
#'   appended to the result. Defaults to `FALSE`.
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_value_spec <- function(
    doc,
    cols = base_col_regex()$.value_spec,
    sheet = NULL,
    where_sep_sheet = TRUE,
    where_cols = c(
      "id" = "ID",
      "where" = c("Variable", "Comparator", "Value")
    ),
    var_sheet = "[V|v]ar",
    define_fields = FALSE) {
  cols <- (if (define_fields) define_col_regex()$.value_spec else cols)

  # "predecessor" is a processing-only column (not in schema) transformed into
  # derivation_id and then dropped. "comment_id" is a define-extra populated via
  # a separate join rather than through create_tbl, so excluded from optional.
  valid_names <- c(names(define_column_schema()$.value_spec), "predecessor")

  if (!all(names(cols) %in% valid_names) | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var value_spec}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {valid_names}}",
      "i" = "If {.val derivation_id} is not avaliable it can be excluded and dataset.variable will be used.",
      "i" = "If the where information is on a seperate sheet, put the column with cross ref as where."
    ))
  }
  active_schema <- if (define_fields) define_column_schema() else base_column_schema()
  cols <- cols[names(cols) %in% c(names(active_schema$.value_spec), "predecessor")]

  # Select a subset of sheets if specified
  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
  }

  out <- create_tbl(doc, cols, context = "spec_type_to_value_spec", schema = active_schema$.value_spec)

  # Does a var sheet exist?
  if (!is.null(var_sheet)) {
    var_sheet <- names(doc) |>
      keep(~ str_detect(., var_sheet))
  }

  # If so, add any variables not in the value sheet
  if (length(var_sheet) > 0) {
    var_out <- doc[var_sheet] |>
      map_dfr(~ .x %>%
        select_rename_w_dups(cols) |>
        mutate(where = NA_character_)) |>
      anti_join(out, by = c("dataset", "variable"))

    out <- bind_rows(out, var_out)
  }

  if (where_sep_sheet && any(!is.na(out$where))) {
    where_df <- create_tbl(doc, where_cols, context = "spec_type_to_value_spec") |>
      tidyr::unite("where_new", starts_with("where"), sep = " ", na.rm = TRUE, remove = FALSE) |>
      select(id, where_new)

    out <- out |>
      left_join(where_df, by = c("where" = "id")) |>
      select(-where, where = where_new)
  } else if (where_sep_sheet) {
    cli_warn(c(
      "x" = "where column needed to cross-reference where information from separate sheet"
    ))
  }

  if (!"derivation_id" %in% names(cols)) {
    out <- out |>
      mutate(
        derivation_id = if_else(
          str_to_lower(.data$origin) == "assigned",
          paste0(dataset, ".", variable),
          paste0("pred.", dataset, ".", variable)
        )
      )
  }

  out <- out |>
    distinct() |>
    mutate(
      sig_dig = as.integer(.data$sig_dig),
      derivation_id = case_when(
        !is.na(.data$derivation_id) ~ .data$derivation_id,
        str_to_lower(.data$origin) == "predecessor" ~ paste0("pred.", as.character(.data$predecessor)),
        str_to_lower(.data$origin) == "assigned" ~ paste0(.data$dataset, ".", .data$variable)
      )
    ) |>
    select(-.data$predecessor)

  if (define_fields && "where" %in% names(out)) {
    out <- mutate(out, where_label = if_else(is.na(where), NA_character_, where_label))
  }

  # Extract comment_id from Variables sheet if available for define fields
  if (length(var_sheet) > 0 && define_fields == TRUE) {
    comment_mapping <- doc[var_sheet] |>
      map_dfr(~ .x %>%
        select(
          dataset = matches("[D|d]ataset|[D|d]omain"),
          variable = matches("[N|n]ame|[V|v]ariables?"),
          comment_id = matches("[C|c]omment")
        ) %>%
        filter(!is.na(comment_id), comment_id != "")) |>
      distinct()

    if (nrow(comment_mapping) > 0) {
      out <- out |>
        select(-comment_id) |>
        left_join(comment_mapping, by = c("dataset", "variable"))
    }
  }

  out |>
    reorder_by_schema("value_spec")
}

#' Spec to codelist
#'
#' Creates the value_spec from a list of datasets (optionally filtered by the
#' sheet input). The named vector `*_cols` is used to determine which is the
#' correct sheet and renames the columns.
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param codelist_cols Named vector of column names that make up the codelist.
#'   The column names can be regular expressions for more flexibility. But, the
#'   names must follow the given pattern
#' @param permitted_val_cols Named vector of column names that make up the
#'   permitted value The column names can be regular expressions for more
#'   flexibility. This is optional, can be left as null if there isn't a
#'   permitted value sheet
#' @param dict_cols Named vector of column names that make up the dictionary
#'   value The column names can be regular expressions for more flexibility.
#'   This is optional, can be left as null if there isn't a permitted value
#'   sheet
#' @param sheets Optional, regular expressions of the sheets
#' @param simplify Boolean value, if true will convert code/decode pairs that
#'   are all equal to a permitted value list. True by default
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_codelist <- function(
    doc,
    codelist_cols = c(
      "code_id" = "ID",
      "name" = "[N|n]ame",
      "code" = "^[C|c]ode|^[T|t]erm",
      "decode" = "[D|d]ecode"
    ),
    permitted_val_cols = NULL,
    dict_cols = c(
      "code_id" = "ID",
      "name" = "[N|n]ame",
      "dictionary" = "[D|d]ictionary",
      "version" = "[V|v]ersion"
    ),
    sheets = NULL,
    simplify = FALSE) {
  if (is.null(codelist_cols)) {
    cli_abort("Codelist column names must be provided as {.arg codelist_cols}")
  }

  codelist_names <- c("code_id", "name", "code", "decode")
  dict_names <- c("code_id", "name", "dictionary", "version")
  permitted_val_names <- c("code_id", "name", "code")

  # Validate names of codelist columns (minimum requirement)
  name_check <- all(names(codelist_cols) %in% codelist_names)

  if (!name_check | is.null(names(codelist_cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.arg codelist_cols}",
      "i" = "{.arg codelist_cols} must be named with {.val {codelist_names}}"
    ))
  }

  # If present, validate names of permitted_val columns
  if (!is.null(permitted_val_cols)) {
    name_check <- all(names(permitted_val_cols) %in% permitted_val_names)
    if (!name_check) {
      cli_abort(c(
        "x" = "Incorrect column names supplied for {.arg permitted_val_cols}",
        "i" = "{.arg permitted_val_cols} must be named with {.val {permitted_val_names}}"
      ))
    }
  }

  # If present, validate names of dictionary columns
  if (!is.null(dict_cols)) {
    name_check <- all(names(dict_cols) %in% dict_names)
    if (!name_check) {
      cli_abort(c(
        "x" = "Incorrect column names supplied for {.arg dict_cols}",
        "i" = "{.arg dict_cols} must be named with {.val {dict_names}}",
        "i" = "If a dictionary sheet isn't available set {.arg dict_cols} to NULL"
      ))
    }
  }

  # Select a subset of sheets if specified
  if (!is.null(sheets)) {
    sheet_ls <- str_subset(names(doc), sheets)
    doc <- doc[sheet_ls]
  }

  # Create base codelist table
  cd_out <- create_tbl(doc, codelist_cols, context = "spec_type_to_codelist") |>
    group_by(code_id) |>
    mutate(type = if_else(simplify & all(code == decode), "permitted_val", "code_decode")) |>
    nest(codes = c(code, decode)) |>
    mutate(codes = dplyr::case_match(type,
      "permitted_val" ~ lapply(codes, \(df) pull(df, code)),
      .default = codes
    ))

  # Add permitted values if available
  if (!is.null(permitted_val_cols)) {
    pv_out <- create_tbl(doc, permitted_val_cols, context = "spec_type_to_codelist") |>
      mutate(type = "permitted_val") |>
      group_by(code_id) |>
      nest(codes = c(code))

    cd_out <- bind_rows(cd_out, pv_out)
  }

  # Add dictionary if available
  if (!is.null(dict_cols)) {
    dict_out <- create_tbl(doc, dict_cols, context = "spec_type_to_codelist") |>
      mutate(type = "external_library") |>
      group_by(code_id) |>
      nest(codes = c(dictionary, version))

    cd_out <- bind_rows(cd_out, dict_out)
  }

  cd_out |>
    distinct() |>
    filter(!is.na(code_id)) |>
    ungroup() |>
    reorder_by_schema("codelist")
}

#' Spec to derivation
#'
#' Builds the `derivations` table from a named list of Excel sheets produced by
#' [read_all_sheets()]. `derivations` holds **one row per unique derivation**
#' and maps a derivation ID to its full derivation text.
#'
#' @details
#' ## How it works
#'
#' Derivations are assembled from **two sources** and then combined:
#'
#' 1. **Methods/derivations sheet** (`cols`, `sheet`): A dedicated sheet
#'    containing an ID column and a derivation-text column. Rows from this
#'    sheet are included directly.
#'
#' 2. **Variables sheet** (`var_cols`): Origin, predecessor, and comment columns
#'    are read from the variables sheet and used to auto-generate derivation
#'    entries for variables not already covered by the methods sheet:
#'    - `"Derived"` origin → derivation text comes from the derivation column.
#'    - `"Assigned"` origin → derivation text comes from the comment column;
#'      the ID is formed as `DATASET.VARIABLE`.
#'    - `"Predecessor"` origin → derivation text is the predecessor variable
#'      name; the ID is formed as `pred.PREDECESSOR`.
#'
#' ## Output columns
#'
#' | Column | Description |
#' |:-------|:------------|
#' | `derivation_id` | Unique derivation identifier, e.g. `"M.AVAL"` |
#' | `derivation` | Full derivation text or computational method |
#'
#' ## Default column matching — derivation sheet
#'
#' When `cols = NULL` (the default), patterns from [base_col_regex()] are used
#' to find columns on the methods/derivations sheet:
#'
#' - **`derivation_id`** — heading matches `"ID"`
#' - **`derivation`** — heading matches `"Definition"` or `"Description"`
#'
#' ## Default column matching — variables sheet (`var_cols`)
#'
#' `var_cols` locates the origin, predecessor, and comment columns. Defaults:
#'
#' - **`dataset`** — heading matches `"Dataset"` or `"Domain"`
#' - **`variable`** — heading matches `"Name"` or `"Variables"`
#' - **`origin`** — heading matches `"Origin"`
#' - **`predecessor`** — heading matches `"Predecessor"`
#' - **`comment`** — heading matches `"Comment"`
#'
#' To customise, pass a named character vector to `cols` and/or `var_cols`:
#'
#' ```r
#' spec_type_to_derivations(doc,
#'   cols = c(
#'     "derivation_id" = "Method ID",
#'     "derivation"    = "Computational Algorithm"
#'   ),
#'   var_cols = c(
#'     "dataset"     = "Domain",
#'     "variable"    = "Variable Name",
#'     "origin"      = "Source",
#'     "predecessor" = "Predecessor Variable",
#'     "comment"     = "Notes"
#'   )
#' )
#' ```
#'
#' ## Additional Define.xml columns (`define_fields = TRUE`)
#'
#' Set `define_fields = TRUE` to append four extra columns required for
#' Define.xml 2.0 generation. Patterns are drawn from [define_col_regex()]:
#'
#' | Column | Description | Default heading match |
#' |:-------|:------------|:----------------------|
#' | `method_name` | Short name for the method | `"Name"` |
#' | `method_type` | Method type (e.g. `"Computation"`) | `"Type"` |
#' | `document_id` | Cross-reference to an entry in `documents` | `"Document"` |
#' | `pages` | Page reference(s) within the document | `"Pages"` |
#'
#' @param doc Named list of data frames produced by [read_all_sheets()].
#' @param cols Named character vector of regular expressions mapping output
#'   column names to headings on the derivation/methods sheet. When `NULL`
#'   (default), patterns from [base_col_regex()] are used (or [define_col_regex()]
#'   when `define_fields = TRUE`). Valid names are `derivation_id`, `derivation`;
#'   plus `method_name`, `method_type`, `document_id`, `pages` when
#'   `define_fields = TRUE`.
#' @param var_cols Named character vector of regular expressions mapping the
#'   processing columns (`dataset`, `variable`, `origin`, `predecessor`,
#'   `comment`) to headings on the variables sheet. Defaults to built-in
#'   patterns covering standard Pinnacle 21 headings.
#' @param sheet Regular expression used to restrict which sheets are searched
#'   for the derivation/methods sheet. Defaults to matching sheets named
#'   `"Method"` or `"Derivations"`.
#' @param define_fields `r lifecycle::badge("experimental")` Logical; when
#'   `TRUE`, additional Define.xml columns (`method_name`, `method_type`,
#'   `document_id`, `pages`) are appended to the result. Defaults to `FALSE`.
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
#' @importFrom purrr quietly
spec_type_to_derivations <- function(
    doc,
    cols = base_col_regex()$.derivations,
    sheet = "Method|Derivations?",
    var_cols = c(
      "dataset" = "[D|d]ataset|[D|d]omain",
      "variable" = "[N|n]ame|[V|v]ariables?",
      "origin" = "[O|o]rigin",
      "predecessor" = "[P|p]redecessor",
      "comment" = "[C|c]omment"
    ),
    define_fields = FALSE) {
  cols <- (if (define_fields) define_col_regex()$.derivations else cols)

  # Validate against the full schema so typos are caught regardless of mode
  valid_names <- names(define_column_schema()$.derivations)
  var_names <- c("dataset", "variable", "origin", "predecessor", "comment")

  name_check <- all(names(cols) %in% valid_names)
  if (!name_check | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.arg cols}",
      "i" = "{.arg cols} must be named with {.val {valid_names}}"
    ))
  }
  active_schema <- if (define_fields) define_column_schema() else base_column_schema()
  cols <- cols[names(cols) %in% names(active_schema$.derivations)]

  # Validate the names of the variable columns (used to join)
  name_check <- all(names(var_cols) %in% var_names)
  if (!name_check | is.null(names(var_cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.arg var_cols}",
      "i" = "{.arg var_cols} must be named with {.val {var_names}}"
    ))
  }

  # Get the predecessor table
  ls_derivations <- quietly(create_tbl)(doc, var_cols)$result

  if (!is.data.frame(ls_derivations) && is.list(ls_derivations)) {
    ls_derivations <- ls_derivations |>
      purrr::list_rbind()

    # Add comments if available
    comment_sheets <- names(doc) |>
      str_subset("(?i)comment")

    if (length(comment_sheets) > 0) {
      comments <- doc[[comment_sheets[1]]] |>
        select(matches("(?i)^(id|description)$"))

      ls_derivations <- ls_derivations |>
        mutate(
          origin_lower = str_to_lower(origin),
          comment = if_else(
            origin_lower == "assigned",
            comments$Description[match(comment, comments$ID)],
            comment
          )
        ) |>
        select(-origin_lower)
    }
  }

  other_derivations <- ls_derivations |>
    mutate(
      origin_lower = str_to_lower(origin),
      derivation_id = dplyr::case_match(origin_lower,
        "predecessor" ~ paste0("pred.", predecessor),
        "assigned" ~ paste0(dataset, ".", variable),
        .default = NA_character_
      ),
      derivation = dplyr::case_match(origin_lower,
        "predecessor" ~ as.character(predecessor),
        "assigned" ~ comment,
        .default = NA_character_
      ),
      .keep = "unused"
    ) |>
    filter(!is.na(derivation_id)) |>
    select(derivation, derivation_id)

  # Filter sheets if specified
  if (!is.null(sheet)) {
    doc <- doc[str_subset(names(doc), sheet)]
  }

  out <- create_tbl(doc, cols, context = "spec_type_to_derivations", schema = active_schema$.derivations)

  out |>
    bind_rows(other_derivations) |>
    distinct() |>
    filter(!is.na(derivation_id)) |>
    reorder_by_schema("derivations")
}

#' Spec to documents
#'
#' `r lifecycle::badge("experimental")`
#' Creates the documents table from a Documents sheet in the specification.
#' The Documents sheet should contain a document identifier (`document_id`),
#' a human-readable title, and an href (file path or URL) pointing to the
#' external document. Documents are linked to derivations via `document_id`
#' in the derivations table.
#'
#' This table is only populated when `define_fields = TRUE` is passed to
#' [spec_to_metacore()]. It is a Define.xml-specific table with no equivalent
#' in the base schema.
#'
#' @param doc Named list of datasets. @seealso [read_all_sheets()] for exact format
#' @param cols Named vector of column regexes. Defaults are drawn from
#'   [define_col_regex()]. Names must be a subset of `c("document_id", "title", "href")`.
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object, or `NULL` if no
#'   matching sheet is found
#' @export
#'
#' @family spec builders
spec_type_to_documents <- function(
    doc,
    cols = define_col_regex()$.documents,
    sheet = "[D|d]ocuments?") {
  documents_names <- names(define_column_schema()$.documents)

  name_check <- all(names(cols) %in% documents_names)
  if (!name_check | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var documents}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {documents_names}}"
    ))
  }

  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
    if (length(doc) == 0) {
      return(NULL)
    }
  }

  create_tbl(doc, cols, context = "spec_type_to_documents", schema = define_column_schema()$.documents) |>
    distinct() |>
    reorder_by_schema("ds_documents")
}

#' Spec to comments
#'
#' `r lifecycle::badge("experimental")`
#' Creates the comments table from a Comments sheet in the specification.
#' The Comments sheet should contain comment_id (ID column) and comment text
#' (Description column). Comments are linked to variables via the comment_id
#' in the value_spec table.
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact format
#' @param cols Named vector of column regexes. Defaults are drawn from
#'   [define_col_regex()]. Names must be a subset of `c("comment_id", "comment")`.
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object (comments table)
#' @export
#'
#' @family spec builders
spec_type_to_comments <- function(
    doc,
    cols = define_col_regex()$.comments,
    sheet = "[C|c]omments?") {
  comments_names <- names(define_column_schema()$.comments)

  name_check <- all(names(cols) %in% comments_names)
  if (!name_check | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var comments}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {comments_names}}"
    ))
  }

  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
    # If no matching sheets found, return empty comments table (comments are optional)
    if (length(doc) == 0) {
      return(tibble(comment_id = character(), comment = character()))
    }
  }

  create_tbl(doc, cols, context = "spec_type_to_comments", schema = define_column_schema()$.comments) |>
    distinct() |>
    filter(!is.na(comment_id)) |>
    reorder_by_schema("comments")
}

#' Spec to supp
#'
#' `r lifecycle::badge("experimental")`
#' Creates the supp table from value_spec, codelist, and comments by identifying
#' supplemental datasets (SUPP*) and extracting their metadata. For each SUPP domain,
#' extracts variable names from the QNAM codelist, identifying variables from the
#' IDVAR comment, and evaluator from the QEVAL codelist.
#'
#' Note for future: length is populated only when VLM exists
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern.
#'   For modern Pinnacle 21 specification spec types, the mapping will typically be
#'   the same as the mapping for the `value_spec` table.
#' @param sheet Regular expression for the sheet name
#' @param where_sep_sheet Boolean value to control if the where information in a
#'   separate dataset. If the where information is on a separate sheet, set to
#'   true and provide the column information with the `where_cols` inputs.
#' @param where_cols Named list with an id and where field. All columns in the
#'   where field will be collapsed together
#' @param var_spec var_spec table from the metacore object
#' @param value_spec value_spec table from the metacore object
#' @param codelist codelist table from metacore object
#' @param comments comments table from metacore object (optional). Only required when
#'   generating a define.xml enabled metacore object.
#'
#' @return a dataset formatted for the metacore object (supp table)
#' @export
#'
#' @family spec builders
spec_type_to_supp <- function(
    doc,
    cols = c(
      "dataset" = "[D|d]ataset|[D|d]omain",
      "variable" = "[V|v]ariable",
      "where" = "[W|w]here [C|clause]",
      "type" = "[T|t]ype",
      "length" = "[L|l]ength",
      "origin" = "[O|o]rigin"
    ),
    sheet = NULL,
    where_sep_sheet = TRUE,
    where_cols = c(
      "id" = "ID",
      "variable" = "[V|v]ariable",
      "comparator" = "[C|c]omparator",
      "value" = "[V|v]alue"
    ),
    var_spec = NULL,
    value_spec = NULL,
    codelist = NULL,
    comments = NULL) {
  names <- c("dataset", "variable", "where", "type", "length", "origin")

  name_check <- all(names(cols) %in% names)
  if (!name_check | is.null(names(cols))) {
    cli_abort(c(
      "x" = "Incorrect column names supplied for {.var spec}",
      "i" = "The column vector {.arg cols} must be named with a subset of {.val {names}}"
    ))
  }

  if (!is.null(sheet)) {
    sheet_ls <- str_subset(names(doc), sheet)
    doc <- doc[sheet_ls]
    if (length(doc) == 0) {
      return(tibble(comment_id = character(), comment = character()))
    }
  }

  # SUPP datasets share a Variables sheet with parent domains, so isolate them
  # and strip the prefix to recover the parent domain name used as the join key
  # throughout metacore (e.g. SUPPAE -> AE)
  out <- create_tbl(doc, cols, context = "spec_type_to_supp") |>
    filter(str_detect(dataset, "^SUPP")) |>
    mutate(dataset = gsub("^SUPP", "", dataset)) |>
    distinct()

  if (where_sep_sheet && "where" %in% names(out)) {
    # Older P21 specs store WHERE clauses on a dedicated sheet keyed by ID;
    # the `value` column holds the QNAM variable name the clause applies to
    where_df <- create_tbl(doc, where_cols, context = "spec_type_to_supp") |>
      select(id, variable = value)

    out <- out |>
      left_join(where_df, by = c("where" = "id")) |>
      select(dataset, variable, type, length, origin)
  } else if (where_sep_sheet) {
    cli_warn(c(
      "x" = "where column needed to cross-reference where information from separate sheet"
    ))
  }

  if (is.null(value_spec) || nrow(value_spec) == 0) {
    return(tibble(dataset = character(), variable = character(), idvar = character(), qeval = character()))
  }

  # SUPP variable metadata (names, idvar, qeval) is encoded as value-level rows
  # inside the SUPP domain's own value_spec, not as direct columns — we unpack
  # QNAM, IDVAR, and QEVAL rows separately below
  supp_vars <- value_spec |>
    filter(str_detect(dataset, "^SUPP")) |>
    mutate(dataset = gsub("^SUPP", "", dataset)) |>
    arrange(dataset, variable)

  if (nrow(supp_vars) == 0) {
    return(tibble(dataset = character(), variable = character(), idvar = character(), qeval = character()))
  }

  # Fallback so the idvar join below doesn't fail when comments weren't loaded
  if (is.null(comments)) {
    comments <- tibble(comment_id = character(), comment = character())
  }

  # QNAM's codelist holds the supplemental variable names (code) and labels
  # (decode) — this is the canonical list of supp variables for the domain
  supp_qnam <- supp_vars |>
    filter(variable == "QNAM") |>
    select(dataset, code_id) |>
    left_join(codelist |> select(code_id, codes), by = "code_id") |>
    mutate(codes = map(codes, \(x) if (is.data.frame(x)) x else tibble(code = unlist(x), decode = unlist(x)))) |>
    unnest(codes) |>
    select(dataset, variable = code, label = decode)

  # QEVAL is a single permitted value per domain indicating who evaluates the
  # supplemental qualifier (e.g. "INVESTIGATOR"); one value shared across all
  # variables in the domain
  qeval_lookup <- supp_vars |>
    filter(variable == "QEVAL") |>
    select(dataset, code_id) |>
    left_join(codelist |> select(code_id, codes), by = "code_id") |>
    mutate(codes = map(codes, \(x) if (is.data.frame(x)) x else tibble(code = unlist(x), decode = unlist(x)))) |>
    unnest(codes) |>
    select(dataset, qeval = code)

  # IDVAR is stored as a comment in the format IDVAR="<varname>" rather than a
  # direct column, because the spec has no dedicated idvar field
  idvar_lookup <- supp_vars |>
    filter(variable == "IDVAR") |>
    select(dataset, comment_id) |>
    left_join(comments |> select(comment_id, comment), by = "comment_id") |>
    mutate(idvar = str_replace(comment, '^IDVAR="(.*)"$', "\\1")) |>
    select(dataset, idvar)

  # supp_qnam drives the row set (one row per supp variable); out contributes
  # the type/length/origin read from the Variables sheet
  supp_qnam |>
    left_join(out, by = c("dataset", "variable")) |>
    left_join(idvar_lookup, by = "dataset") |>
    left_join(qeval_lookup, by = "dataset") |>
    mutate(length = as.integer(length)) |>
    distinct() |>
    filter(!is.na(variable))
}

#' Add supplemental variable rows to a core metacore table
#'
#' Takes the parsed `supp` table (from [spec_type_to_supp()]) and appends any
#' supplemental variables not already present in `target`. The parent domain is
#' derived by stripping the leading `"SUPP"` prefix from the dataset name
#' (e.g. `"SUPPAE"` becomes `"AE"`). Only columns present in `target_schema`
#' are carried across; missing schema columns are back-filled with typed `NA`s.
#' Where `target_schema` includes a `supp_flag` column, newly added rows have
#' it set to `TRUE`.
#'
#' @param supp The supp table returned by [spec_type_to_supp()]
#' @param target The core table to append rows to (e.g. `ds_vars`, `var_spec`,
#'   or `value_spec`)
#' @param target_schema A zero-row schema tibble for `target`, used to
#'   determine which columns to keep and their types. Typically one element of
#'   [define_column_schema()] or [base_column_schema()]
#'
#' @return `target` with supplemental variable rows appended, columns ordered
#'   to match `target_schema`
#' @noRd
add_supp_to_table <- function(supp, target, target_schema) {
  if (is.null(supp) || nrow(supp) == 0) {
    return(target)
  }

  # Derive parent domain from SUPP dataset name (SUPPAE -> AE)
  new_rows <- supp |>
    mutate(dataset = str_remove(dataset, "^SUPP")) |>
    select(any_of(names(target_schema)))

  # Join key: dataset+variable for tables that carry both, variable-only for var_spec
  join_key <- intersect(c("dataset", "variable"), names(target_schema))

  to_add <- new_rows |>
    anti_join(target, by = join_key) |>
    distinct()

  if (nrow(to_add) == 0) {
    return(target)
  }

  # Back-fill schema columns absent from to_add with typed NAs
  missing_cols <- setdiff(names(target_schema), names(to_add))
  for (col in missing_cols) {
    to_add[[col]] <- target_schema[[col]][seq_len(nrow(to_add))]
  }

  if ("supp_flag" %in% names(target_schema)) {
    to_add <- mutate(to_add, supp_flag = TRUE)
  }

  bind_rows(target, to_add) |>
    select(all_of(names(target_schema)))
}

### Helper Functions

#' Create table
#'
#' This function creates a table from excel sheets. This is mainly used
#' internally for building spec readers, but is exported so others who need to
#' build spec readers can use it.
#' @param doc list of sheets from a excel doc
#' @param cols vector of regex to get a datasets base on which columns it has.
#'   If the vector is named it will also rename the columns
#' @param context Provides the calling context for better error messaging to the user
#' @param schema Optional zero-row schema tibble (e.g. from [`base_column_schema()`] or
#'   `define_column_schema()`). When provided, any schema columns absent from the
#'   matched sheet are back-filled with typed `NA`s via `fill_cols()`.
#'
#' @return dataset (or list of datasets if not specific enough)
#' @export
create_tbl <- function(doc, cols, context = NULL, schema = NULL) {
  # Find sheets where every column can be matched
  matches <- doc |>
    keep(function(x) {
      cols |>
        map_lgl(~ any(str_detect(names(x), .))) |>
        all()
    })

  # If no matches throw error with closest matches
  if (length(matches) == 0) {
    mismatch_per_sheet <- doc |>
      map(function(x) {
        cols |>
          map_lgl(~ any(str_detect(names(x), .))) |>
          discard(~.)
      })
    mis_lens <- mismatch_per_sheet |> map_int(length)
    closest_sheets <- mis_lens |>
      keep(~ . == min(mis_lens)) |>
      names()
    sheets_to_error <- mismatch_per_sheet[names(mismatch_per_sheet) %in% closest_sheets]

    has_where_col <- sheets_to_error |>
      map_lgl(~ any(str_detect(names(.x), regex("^where", ignore_case = TRUE)))) |>
      any()

    sheet_details <- sheets_to_error |>
      purrr::imap_chr(~ paste0("Sheet '", .y, "' is missing: ", paste(names(.x), collapse = ", ")))

    cli_abort(
      c(
        "x" = "Unable to identify a sheet with all columns.",
        "i" = "Closest matches identified:",
        "*" = sheet_details,
        if (has_where_col) c("!" = "Tip: A 'where' column was detected. Check if {.arg where_sep_sheet} is set correctly.")
      )
    )
  }

  result <- if (length(matches) == 1) {
    build_from_sheet(matches[[1]], cols, context, names(matches))
  } else {
    sheets_mats <- names(matches)
    cli_warn(
      c(
        "Column names are not specific enough to identify a single sheet.",
        "The following {length(sheets_mats)} match the criteria set:"
      ),
      ansi_collapse(sheets_mats)
    )
    imap(matches, ~ build_from_sheet(.x, cols, context, .y))
  }

  if (!is.null(schema)) {
    if (is.data.frame(result)) {
      result <- fill_cols(result, schema)
    } else {
      result <- lapply(result, fill_cols, schema = schema)
    }
  }

  result
}


#' Build a table from a single matched sheet
#'
#' @param sheet_data a single data frame (one element of the `doc` list)
#' @param cols named regex vector of columns to select and rename
#' @param context calling context string for error messages
#' @param sheet_name sheet name used in duplicate-column error messages
#'
#' @return renamed data frame
#' @noRd
build_from_sheet <- function(sheet_data, cols, context, sheet_name = NULL) {
  sheet_names <- names(sheet_data)

  # Duplicate-match check and tightening of regex to exact anchors if needed
  nm_test <- cols |>
    map(~ str_detect(sheet_names, .)) |>
    map(~ sheet_names[.]) |>
    keep(~ length(.) > 1)

  if (length(nm_test) > 0) {
    test_exact <- cols[names(nm_test)] |>
      (\(x) paste0("^", x, "$"))() |>
      map_int(~ sum(str_detect(sheet_names, .))) |>
      keep(~ . != 1)
    if (length(test_exact) == 0) {
      cols[names(nm_test)] <- cols[names(nm_test)] |> (\(x) paste0("^", x, "$"))()
    } else {
      errors <- NULL
      for (i in seq_along(nm_test)) {
        errors <- c(errors, str_glue(
          "{names(nm_test[i])} matches {length(nm_test[[i]])} columns: {paste(nm_test[[i]], collapse = ', ')}"
        ))
      }
      context_hint <- if (!is.null(context)) {
        str_glue("Please check your regular expression for `{context}`")
      } else {
        "Please check your regular expressions."
      }
      cli_abort(
        c(
          "Unable to rename the following columns in {sheet_name}",
          set_names(errors, rep("x", length(errors))),
          "i" = context_hint
        )
      )
    }
  }

  select_rename_w_dups(sheet_data, cols)
}


#' Yes No to True False
#'
#' @param x takes in a vector to convert
#'
#' @return returns a logical vector or normal vector with warning
#' @noRd
#'
yn_to_tf <- function(x) {
  if (all(is.na(x) | str_detect(x, regex("^y$|^n$|^yes$|^no$", ignore_case = T)))) {
    case_when(
      str_detect(x, regex("^y$|^yes$", ignore_case = T)) ~ TRUE,
      str_detect(x, regex("^n$|^no$", ignore_case = T)) ~ FALSE,
      is.na(x) ~ NA
    )
  } else if (is.logical(x)) {
    x
  } else {
    cli_warn("Keep column needs to be True or False, please correct before converting to a Metacore object")
    x
  }
}


#' Select in a dataset with renames
#'
#' This works like select, but if there are duplicates it won't cause issues
#'
#' @param .data dataset to select columns and rename
#' @param cols named vector
#'
#' @return dataset
#' @noRd
select_rename_w_dups <- function(.data, cols) {
  pull_safe <- safely(~ select(.x, matches(.y, ignore.case = FALSE)))

  cols |>
    map_dfr(function(col) {
      out <- pull_safe(.data, col) |>
        purrr::pluck("result")

      if (ncol(out) == 1) {
        pull(out, 1)
      } else {
        NULL
      }
    })
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
