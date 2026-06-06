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
   # Check if user has supplied `quiet` instead of `verbose`
   if (lifecycle::is_present(quiet)) {
      deprecate_soft(when = "0.3.0", what = "spec_to_metacore(quiet)", with = "spec_to_metacore(verbose)")
   } else {
      quiet <- FALSE
   } # Else deal with deprecated argument for compatibility

   with_verbosity(
      {
         doc <- read_all_sheets(path)

         if (spec_type(path) == "by_type") {
            ds_spec <- spec_type_to_ds_spec(doc)
            ds_vars <- spec_type_to_ds_vars(doc)
            var_spec <- spec_type_to_var_spec(doc)
            value_spec <- spec_type_to_value_spec(doc, where_sep_sheet = where_sep_sheet)
            derivations <- spec_type_to_derivations(doc)
            codelist <- spec_type_to_codelist(doc)
            documents <- spec_type_to_documents(doc)
            comments <- spec_type_to_comments(doc)
            supp <- create_supp_table(
               doc,
               where_sep_sheet = where_sep_sheet,
               var_spec = var_spec,
               value_spec = value_spec,
               codelist = codelist,
               comments = comments
            )

            # Add supplemental variables to ds_vars, var_spec, value_spec
            ds_vars <- add_supp_to_table(supp, ds_vars, define_column_schema()$.ds_vars)
            var_spec <- add_supp_to_table(supp, var_spec, define_column_schema()$.var_spec)
            value_spec <- add_supp_to_table(supp, value_spec, define_column_schema()$.value_spec)

            # Strip unneeded vars from supp
            supp <- reorder_by_schema(supp, "supp")

            mc <- metacore(
               ds_spec,
               ds_vars,
               var_spec,
               value_spec,
               derivations,
               codelist,
               supp = supp,
               documents = documents,
               comments = comments,
               define_fields = define_fields,
               quiet = quiet,
               verbose = verbose
            )
         } else {
            cli_abort(
               "This specification format is not currently supported. You will need to write your own reader"
            )
         }

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
#' Creates the ds_spec from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_ds_spec <- function(
      doc,
      cols = c(
         "dataset" = "[N|n]ame|[D|d]ataset|[D|d]omain",
         "structure" = "[S|s]tructure",
         "label" = "[L|l]abel|[D|d]escription",
         "class" = "[C|c]lass",
         "repeating" = "[R|r]epeating",
         "reference" = "[R|r]eference [D|d]ata",
         "purpose" = "[P|p]urpose"
      ),
      sheet = NULL
) {
   ds_spec_names <- c("dataset", "structure", "label", "class", "repeating", "reference", "purpose")
   ds_spec_optional <- c("class", "repeating", "reference", "purpose")

   name_check <- all(names(cols) %in% ds_spec_names)
   if (!name_check | is.null(names(cols))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.var ds_spec}",
         "i" = "The column vector {.arg cols} must be named with a subset of {.val {ds_spec_names}}",
         "i" = "The columns {.val {ds_spec_optional}} are optional"
      ))
   }
   if (!is.null(sheet)) {
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }

   create_tbl(doc, cols, ds_spec_optional, context = "spec_type_to_ds_spec") |>
      distinct() |>
      mutate(
         repeating = yn_to_tf(.data$repeating),
         reference = yn_to_tf(.data$reference),
      ) |>
      reorder_by_schema("ds_spec")
}

#' Spec to ds_vars
#'
#' Creates the ds_vars from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param sheet Regular expression for the sheet names
#' @param key_seq_sep_sheet A boolean to indicate if the key sequence is on a
#'   separate sheet. If set to false add the key_seq column name to the `cols`
#'   vector.
#' @param key_seq_cols names vector to get the key_sequence for each dataset
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_ds_vars <- function(
      doc,
      cols = c(
         "dataset" = "[D|d]ataset|[D|d]omain",
         "variable" = "[V|v]ariable [[N|n]ame]?|[V|v]ariables?",
         "order" = "[V|v]ariable [O|o]rder|[O|o]rder",
         "core" = "[C|c]ore|CDISC [C|c]ore",
         "mandatory" = "[K|k]eep|[M|m]andatory",
         "role" = "[R|r]ole"
      ),
      key_seq_sep_sheet = TRUE,
      key_seq_cols = c(
         "dataset" = "Dataset",
         "key_seq" = "Key Variables"
      ),
      sheet = "[V|v]ar|Datasets"
) {

   ds_vars_names <- c("dataset", "variable", "order", "mandatory", "key_seq", "core", "supp_flag", "role")
   ds_vars_optional <- c("core", "role")

   name_check <- all(names(cols) %in% ds_vars_names)

   name_check_extra <- ifelse(
      key_seq_sep_sheet,
      key_seq_sep_sheet,
      all(names(key_seq_cols) %in% c("dataset", "key_seq"))
   )

   # Testing for names of vectors
   if (any(!name_check, !name_check_extra, is.null(names(cols)))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.var ds_vars}",
         "i" = "The column vector {.arg cols} must be named with a subset of {.val {ds_vars_names}}",
         "i" = "The columns {.val {ds_vars_optional}} are optional"
      ))
   }

   # Sub-setting sheets
   if (!is.null(sheet)) {
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }

   # Get base doc
   out <- create_tbl(doc, cols, ds_vars_optional, context = as.character(sys.call(0)[[1]]))

   # Getting the key seq values
   if (key_seq_sep_sheet) {
      key_seq_df <- create_tbl(doc, key_seq_cols, ds_vars_optional, context = as.character(sys.call(0)[[1]])) |>
         mutate(
            key_seq = str_split(key_seq, ",\\s"),
            key_seq = map(key_seq, function(x) {
               tibble(variable = x) |>
                  mutate(key_seq = row_number())
            })
         ) |>
         unnest(key_seq)

      out <- left_join(out, key_seq_df, by = c("dataset", "variable"))
   }

   out |>
      distinct() |>
      mutate(
         key_seq = as.integer(.data$key_seq),
         mandatory = yn_to_tf(.data$mandatory),
         core = as.character(.data$core),
         order = as.numeric(.data$order)
      ) |>
      reorder_by_schema("ds_vars")
}


#' Spec to var_spec
#'
#' Creates the var_spec from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns. (Note: the keep column will be converted logical)
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_var_spec <- function(
      doc,
      cols = c(
         "variable" = "[N|n]ame|[V|v]ariables?",
         "length" = "[L|l]ength",
         "label" = "[L|l]abel",
         "type" = "[T|t]ype",
         "dataset" = "[D|d]ataset|[D|d]omain",
         "format" = "[F|f]ormat"
      ),
      sheet = "[V|v]ar") {

   var_spec_names <- c("variable", "length", "label", "type", "dataset", "common", "format")
   var_spec_optional <- c("common")

   # Check the names
   name_check <- all(names(cols) %in% var_spec_names)
   if (!name_check | is.null(names(cols))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.var var_spec}",
         "i" = "The column vector {.arg cols} must be named with {.val {var_spec_names}}",
         "i" = "The columns {.val {var_spec_optional}} are optional",
         "i" = "Additionally, dataset is only used to clarify if information differs by domain."
      ))
   }

   # Filter sheets if specified
   if (!is.null(sheet)) {
      doc <- doc[str_subset(names(doc), sheet)]
   }

   out <- create_tbl(doc, cols, var_spec_optional, context = "spec_type_to_var_spec")

   # Check for duplicate variables without dataset column
   if (!"dataset" %in% names(out)) {
      dups <- out |>
         distinct() |>
         count(variable) |>
         filter(n > 1) |>
         pull(variable)

      if (length(dups) > 0) {
         cli_abort(c(
            col_red("The following variables are repeated with different metadata for different datasets:"),
            "i" = ansi_collapse(dups),
            "i" = "Please add 'dataset' = [Name of dataset column] to your named cols vector to correct this."
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
#' Creates the value_spec from a list of datasets (optionally filtered by the
#' sheet input). The named vector `cols` is used to determine which is the
#' correct sheet and renames the columns
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param sheet Regular expression for the sheet name
#' @param where_sep_sheet Boolean value to control if the where information in a
#'   separate dataset. If the where information is on a separate sheet, set to
#'   true and provide the column information with the `where_cols` inputs.
#' @param where_cols Named list with an id and where field. All columns in the
#'   where field will be collapsed together
#' @param var_sheet Name of sheet with the Variable information on it. Metacore
#'   expects each variable will have a row in the value_spec. Because many
#'   specification only have information in the value tab this is added. If the
#'   information already exists in the value tab of your specification set to
#'   NULL
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
spec_type_to_value_spec <- function(
      doc,
      cols = c(
         "dataset" = "[D|d]ataset|[D|d]omain",
         "variable" = "[N|n]ame|[V|v]ariables?",
         "origin" = "[O|o]rigin",
         "type" = "[T|t]ype",
         "code_id" = "[C|c]odelist|Controlled Term",
         "sig_dig" = "[S|s]ignificant",
         "where" = "[W|w]here",
         "where_label" = "[L|l]abel|[D|d]escription",
         "derivation_id" = "[M|m]ethod",
         "predecessor" = "[P|p]redecessor"
      ),
      sheet = NULL,
      where_sep_sheet = TRUE,
      where_cols = c(
         "id" = "ID",
         "where" = c("Variable", "Comparator", "Value")
      ),
      var_sheet = "[V|v]ar"
) {

   value_spec_names <- c("dataset", "variable", "origin", "type", "code_id", "sig_dig", "where", "where_label", "derivation_id", "predecessor")
   value_spec_optional <- c("predecessor", "where_label")

   name_check <- all(names(cols) %in% value_spec_names)

   if (!name_check | is.null(names(cols))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.var value_spec}",
         "i" = "The column vector {.arg cols} must be named with a subset of {.val {value_spec_names}}",
         "i" = "The columns {.val {value_spec_optional}} are optional",
         "i" = "If {.val derivation_id} is not avaliable it can be excluded and dataset.variable will be used.",
         "i" = "If the where information is on a seperate sheet, put the column with cross ref as where."
      ))
   }

   # Select a subset of sheets if specified
   if (!is.null(sheet)) {
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }

   out <- create_tbl(doc, cols, value_spec_optional, context = spec_type_to_value_spec)

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
                    mutate(where = NA_character_)
         ) |>
         anti_join(out, by = c("dataset", "variable"))

      out <- bind_rows(out, var_out)
   }

   if (where_sep_sheet && "where" %in% names(out)) {
      where_df <- create_tbl(doc, where_cols, context = spec_type_to_value_spec) |>
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
         where_label = if_else(is.na(where), NA_character_, where_label),
         derivation_id = case_when(
            !is.na(.data$derivation_id) ~ .data$derivation_id,
            str_to_lower(.data$origin) == "predecessor" ~ paste0("pred.", as.character(.data$predecessor)),
            str_to_lower(.data$origin) == "assigned" ~ paste0(.data$dataset, ".", .data$variable)
         )
      ) |>
      select(-.data$predecessor)

   # Extract comment_id from Variables sheet if available
   var_sheets <- names(doc) |> keep(~ str_detect(., "[V|v]ar"))
   if (length(var_sheets) > 0) {
      comment_mapping <- doc[var_sheets] |>
         map_dfr(~ .x %>%
                    select(
                       dataset = matches("[D|d]ataset|[D|d]omain"),
                       variable = matches("[N|n]ame|[V|v]ariables?"),
                       comment_id = matches("[C|c]omment")
                    ) %>%
                    filter(!is.na(comment_id), comment_id != "")
         ) |>
         distinct()

      if (nrow(comment_mapping) > 0) {
         out <- out |>
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
      simplify = FALSE
) {

   if (is.null(codelist_cols)) {
      cli_abort("Codelist column names must be provided as {.arg codelist_cols}")
   }

   codelist_names <- c("code_id", "name", "code", "decode")
   dict_names <-  c("code_id", "name", "dictionary", "version")
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
#' Creates the derivation table from a list of datasets (optionally filtered by
#' the sheet input). The named vector `cols` is used to determine which is the
#' correct sheet and renames the columns. The derivation will be used for
#' "derived" origins, the comments for "assigned" origins, and predecessor for
#' "predecessor" origins.
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param var_cols Named vector of the name(s) of the origin, predecessor and
#'   comment columns. These do not have to be on the specified sheet.
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builders
#' @importFrom purrr quietly
spec_type_to_derivations <- function(
      doc,
      cols = c(
         "derivation_id" = "ID",
         "derivation" = "[D|d]efinition|[D|d]escription",
         "method_name" = "[N|n]ame",
         "method_type" = "[T|t]ype",
         "document_id" = "[D|d]ocument",
         "pages" = "[P|p]ages"
      ),
      sheet = "Method|Derivations?",
      var_cols = c(
         "dataset" = "[D|d]ataset|[D|d]omain",
         "variable" = "[N|n]ame|[V|v]ariables?",
         "origin" = "[O|o]rigin",
         "predecessor" = "[P|p]redecessor",
         "comment" = "[C|c]omment"
      )
) {

   derivations_names <- c("derivation_id", "derivation", "method_name", "method_type", "document_id", "pages")
   derivations_optional <- c("method_name", "method_type", "document_id", "pages")
   var_names <- c("dataset", "variable", "origin", "predecessor", "comment")

   # Validate names of the derivations columns
   name_check <- all(names(cols) %in% derivations_names)
   if (!name_check | is.null(names(cols))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.arg cols}",
         "i" = "{.arg cols} must be named with {.val {derivations_names}}"
      ))
   }

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

   if (is.list(ls_derivations)) {
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

   out <- create_tbl(doc, cols, derivations_optional, context = "spec_type_to_derivations")

   out |>
      bind_rows(other_derivations) |>
      distinct() |>
      filter(!is.na(derivation_id)) |>
      reorder_by_schema("derivations")
}

spec_type_to_documents <- function(
      doc,
      cols = c(
         "document_id" = "ID",
         "title" = "[T|t]itle",
         "href" = "[H|h]ref"
      ),
      sheet = "[D|d]ocuments?"
) {

   documents_names <- c("document_id", "title", "href")
   documents_optional <- c("document_id", "title", "href")

   name_check <- all(names(cols) %in% documents_names)
   if (!name_check | is.null(names(cols))) {
      cli_abort(c(
         "x" = "Incorrect column names supplied for {.var documents}",
         "i" = "The column vector {.arg cols} must be named with a subset of {.val {documents_names}}",
         "i" = "The columns {.val {documents_optional}} are optional"
      ))
   }

   if (!is.null(sheet)) {
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
      # If no matching sheets found, return NULL (documents are optional)
      if (length(doc) == 0) {
         return(NULL)
      }
   }

   create_tbl(doc, cols, documents_optional, context = "spec_type_to_documents") |>
      distinct() |>
      reorder_by_schema("ds_documents")
}

#' Spec to comments
#'
#' Creates the comments table from a Comments sheet in the specification.
#' The Comments sheet should contain comment_id (ID column) and comment text
#' (Description column). Comments are linked to variables via the comment_id
#' in the value_spec table.
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object (comments table)
#' @export
#'
#' @family spec builders
spec_type_to_comments <- function(
      doc,
      cols = c(
         "comment_id" = "ID",
         "comment" = "[D|d]escription"
      ),
      sheet = "[C|c]omments?"
) {

   comments_names <- c("comment_id", "comment")
   comments_optional <- c()

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

   create_tbl(doc, cols, comments_optional, context = "spec_type_to_comments") |>
      distinct() |>
      filter(!is.na(comment_id)) |>
      reorder_by_schema("comments")
}

#' Create supp table
#'
#' Creates the supp table from value_spec, codelist, and comments by identifying
#' supplemental datasets (SUPP*) and extracting their metadata. For each SUPP domain,
#' extracts variable names from the QNAM codelist, identifying variables from the
#' IDVAR comment, and evaluator from the QEVAL codelist.
#'
#' Note for future: length is populated only when VLM exists
#'
#' @param value_spec value_spec table from metacore object
#' @param codelist codelist table from metacore object
#' @param comments comments table from metacore object (optional)
#'
#' @return a dataset formatted for the metacore object (supp table)
#' @export
#'
#' @family spec builders
create_supp_table <- function(
      doc,
      cols = c(
         "dataset" = "[D|d]ataset|[D|d]omain",
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
      comments = NULL
) {

   names <- c("dataset", "where", "type", "length", "origin")

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

   out <- create_tbl(doc, cols, context = "create_supp_table") |>
      filter(str_detect(dataset, "^SUPP")) |>
      mutate(dataset = gsub("^SUPP", "", dataset)) |>
      distinct()

   if (where_sep_sheet && "where" %in% names(out)) {
      where_df <- create_tbl(doc, where_cols, context = "create_supp_table") |>
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

   supp_vars <- value_spec |>
      filter(str_detect(dataset, "^SUPP")) |>
      mutate(dataset = gsub("^SUPP", "", dataset)) |>
      arrange(dataset, variable)

   if (nrow(supp_vars) == 0) {
      return(tibble(dataset = character(), variable = character(), idvar = character(), qeval = character()))
   }

   if (is.null(comments)) {
      comments <- tibble(comment_id = character(), comment = character())
   }

   supp_qnam <- supp_vars |>
      filter(variable == "QNAM") |>
      select(dataset, code_id) |>
      left_join(codelist |> select(code_id, codes), by = "code_id") |>
      mutate(codes = map(codes, \(x) if (is.data.frame(x)) x else tibble(code = unlist(x), decode = unlist(x)))) |>
      unnest(codes) |>
      select(dataset, variable = code, label = decode)

   qeval_lookup <- supp_vars |>
      filter(variable == "QEVAL") |>
      select(dataset, code_id) |>
      left_join(codelist |> select(code_id, codes), by = "code_id") |>
      mutate(codes = map(codes, \(x) if (is.data.frame(x)) x else tibble(code = unlist(x), decode = unlist(x)))) |>
      unnest(codes) |>
      select(dataset, qeval = code)

   idvar_lookup <- supp_vars |>
      filter(variable == "IDVAR") |>
      select(dataset, comment_id) |>
      left_join(comments |> select(comment_id, comment), by = "comment_id") |>
      mutate(idvar = str_replace(comment, '^IDVAR="(.*)"$', "\\1")) |>
      select(dataset, idvar)

   supp_qnam |>
      left_join(out, by = c("dataset", "variable")) |>
      left_join(idvar_lookup, by = "dataset") |>
      left_join(qeval_lookup, by = "dataset") |>
      mutate(length = as.integer(length)) |>
      distinct() |>
      filter(!is.na(variable))
}

add_supp_to_table <- function(supp, target, target_schema) {
   if (is.null(supp) || nrow(supp) == 0) return(target)

   # Derive parent domain from SUPP dataset name (SUPPAE -> AE)
   new_rows <- supp |>
      mutate(dataset = str_remove(dataset, "^SUPP")) |>
      select(any_of(names(target_schema)))

   # Join key: dataset+variable for tables that carry both, variable-only for var_spec
   join_key <- intersect(c("dataset", "variable"), names(target_schema))

   to_add <- new_rows |>
      anti_join(target, by = join_key) |>
      distinct()

   if (nrow(to_add) == 0) return(target)

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
#'
#' @return dataset (or list of datasets if not specific enough)
#' @export
create_tbl <- function(doc, cols, optional = NULL, context = NULL) {
   # Split cols into required (must match to identify sheet) and optional
   # (attempted if present in the sheet, silently omitted and filled with NA
   # if absent — backwards-compatible with specs that predate these columns).
   required_cols <- cols[!names(cols) %in% optional]
   optional_cols  <- cols[names(cols) %in% optional]

   # Find sheets where every required column can be matched
   matches <- doc |>
      keep(function(x) {
         required_cols |>
            map_lgl(~ any(str_detect(names(x), .))) |>
            all()
      })

   # If no matches throw error with closest matches
   if (length(matches) == 0) {
      mismatch_per_sheet <- doc |>
         map(function(x) {
            required_cols |>
               map_lgl(~ any(str_detect(names(x), .))) |>
               discard(~.)
         })
      mis_lens <- mismatch_per_sheet |> map_int(length)
      closest_sheets <- mis_lens |> keep(~ . == min(mis_lens)) |> names()
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

   # Else if no unique match but all columns optional return empty dataframe
   if (length(matches) != 1 && length(required_cols) == 0) {
      return(data.frame())
   }

   # If a match is found, build the table from the matching sheet
   build_from_sheet <- function(sheet_data) {
      sheet_names <- names(sheet_data)

      # Determine which optional cols exist in this sheet
      found_optional <- optional_cols |> keep(~ any(str_detect(sheet_names, .)))

      # Active cols = required + whichever optional are present
      active_cols <- c(required_cols, found_optional)

      # Duplicate-match check and tightening of regex to exact anchors if needed
      nm_test <- active_cols |>
         map(~ str_detect(sheet_names, .)) |>
         map(~ sheet_names[.]) |>
         keep(~ length(.) > 1)

      if (length(nm_test) > 0) {
         test_exact <- active_cols[names(nm_test)] |>
            paste0("^", ., "$") |>
            map_int(~ sum(str_detect(sheet_names, .))) |>
            keep(~ . != 1)
         if (length(test_exact) == 0) {
            active_cols[names(nm_test)] <- active_cols[names(nm_test)] |> paste0("^", ., "$")
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
                  "Unable to rename the following columns in {names(matches)}",
                  set_names(errors, rep("x", length(errors))),
                  "i" = context_hint
               )
            )
         }
      }

      result <- select_rename_w_dups(sheet_data, active_cols)

      # Add any optional cols that were absent from the sheet as NA
      missing_optional <- setdiff(names(optional_cols), names(found_optional))
      for (col in missing_optional) {
         result[[col]] <- NA_character_
      }

      result
   }

   if (length(matches) == 1) {
      build_from_sheet(matches[[1]])
   } else {
      sheets_mats <- names(matches)
      cli_warn(
         c(
            "Column names are not specific enough to identify a single sheet.",
            "The following {length(sheets_mats)} match the criteria set:"
         ),
         ansi_collapse(sheets_mats)
      )
      matches |> map(build_from_sheet)
   }
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
