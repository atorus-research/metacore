#' Internal helper: fill columns and apply labels for the 7 base tables.
#' add_labs() silently ignores entries for columns not present in the table, so
#' the same label map works whether the schema is base or extended.
#'
#' @param private  the R6 private environment
#' @param schema   named list of zero-row schema tibbles (base or full)
#' @noRd
#' @importFrom stringr str_to_lower
.metacore_init_base_tables <- function(
      private, schema, ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp
) {
   # snap() strips to schema columns then back-fills missing ones.
   # Used only for tables that gained new columns in the extended schema so that
   # passing an extended-schema table to a base-schema initialiser (or vice versa)
   # is handled gracefully.  Tables without extended columns (var_spec, codelist,
   # supp) use plain fill_cols so that genuinely wrong extra columns are still
   # detected by var_name_check.
   snap <- function(tbl, schema) fill_cols(select(tbl, any_of(names(schema))), schema)

   private$.ds_spec <- snap(ds_spec, schema$.ds_spec) %>%
      add_labs(
         dataset = "Dataset Name", structure = "Value Structure", label = "Dataset Label",
         class = "Dataset Class", repeating = "Repeating (Boolean)",
         reference = "Reference Data (Boolean)", purpose = "Dataset Purpose"
      )
   private$.ds_vars <- snap(ds_vars, schema$.ds_vars) %>%
      add_labs(
         dataset = "Dataset Name", variable = "Variable Name",
         key_seq = "Sequence Key", order = "Variable Order",
         mandatory = "Mandatory (Boolean)", core = "ADaM core (Expected, Required, Permissible)",
         supp_flag = "Supplemental Flag", role = "Variable Role"
      )
   private$.var_spec <- fill_cols(var_spec, schema$.var_spec) %>%
      add_labs(
         variable = "Variable Name", length = "Variable Length",
         label = "Variable Label", type = "Variable Class",
         common = "Common Across ADaM", format = "Variable Format",
         sas_field_name = "SAS Field Name"
      )
   private$.value_spec <- snap(value_spec, schema$.value_spec) %>%
      add_labs(
         type = "Value Type", origin = "Origin of Value",
         code_id = "ID of the Code List", dataset = "Dataset Name",
         variable = "Variable Name", where = "Value of the Variable",
         derivation_id = "ID of Derivation", where_label = "Where Clause Label",
         comment_id = "Comment ID"
      ) %>%
      mutate(origin = str_to_lower(.data$origin))
   private$.derivations <- snap(derivations, schema$.derivations) %>%
      add_labs(
         derivation_id = "ID of Derivation", derivation = "Derivation",
         method_name = "Method Name", method_type = "Method Type",
         document_id = "Document ID", pages = "Document Page References"
      )
   private$.codelist <- fill_cols(codelist, schema$.codelist) %>%
      add_labs(
         code_id = "ID of the Code List", names = "Name of the Code List",
         type = "Code List/Permitted Values/External Library", codes = "List of Codes"
      )
   private$.supp <- fill_cols(supp, schema$.supp) %>%
      add_labs(
         dataset = "Dataset Name", variable = "Variable Name",
         idvar = "Identifying Variable", qeval = "Evaluator"
      )
   private$.ds_len    <- nrow(private$.ds_spec)
   private$.ds_names  <- private$.ds_spec %>% pull(dataset)
   private$.ds_labels <- private$.ds_spec %>% pull(label)
}


#' Base Metacore initializer (original 7-table schema)
#' @family Metacore
#' @noRd
MetaCore_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations,
                                codelist, supp, quiet = FALSE, verbose = "message") {
   deprecate_soft(
      when = "0.3.0",
      what = "MetaCore_initialize(quiet)",
      with = "MetaCore_initialize(verbose)"
   )
   .metacore_init_base_tables(private, base_column_schema(),
                              ds_spec, ds_vars, var_spec,
                              value_spec, derivations, codelist, supp)
   self$validate()
   if (inherits_only(self, c("Metacore", "R6"))) {
      private$.greet(quiet)
   }
}


#' Metacore class print function
#'
#' @param ... pass in the dataframes to be validated
#' @family Metacore
#' @noRd
#'
MetaCore_print <- function(...) {
   cli_par()
   cli_rule("Metacore object contains metadata for {private$.ds_len} datasets")
   for (i in 1:private$.ds_len) {
      cli_bullets(c(">" = "{private$.ds_names[i]} ({private$.ds_labels[i]})"))
   }
   cli_end()

   cli_inform("To use the {.obj Metacore} object with {.pkg metatools} package, first subset a dataset using {.fn metacore::select_dataset}")
}


#' Base Metacore validation — checks the 7 core tables only
#' @family Metacore
#' @noRd
MetaCore_validate <- function() {
   if (var_name_check(private, define_fields = FALSE)) {
      if (nrow(private$.ds_spec) == 0 &
          nrow(private$.ds_vars) == 0 &
          nrow(private$.var_spec) == 0 &
          nrow(private$.value_spec) == 0 &
          nrow(private$.derivations) == 0 &
          nrow(private$.codelist) == 0 &
          nrow(private$.supp) == 0) {
         cli_warn("Other checks were not performed, because all datasets are empty",
                  call. = FALSE
         )
      } else {
         check_columns(
            private$.ds_spec, private$.ds_vars, private$.var_spec,
            private$.value_spec, private$.derivations, private$.codelist,
            schema = base_column_schema()
         )
         ds_vars_check(private$.ds_vars, private$.var_spec)
         value_check(private$.ds_vars, private$.value_spec)
         derivation_check(private$.value_spec, private$.derivations)
         codelist_check(private$.value_spec, private$.codelist)
         if (nrow(private$.supp) > 0) {
            check_columns(supp = private$.supp, schema = base_column_schema())
            supp_check(private$.ds_vars, private$.supp)
         }
      }
   } else {
      cli_warn("Other checks were not performed, because column names were incorrect",
               call. = FALSE
      )
   }
}


#' readonly function factory
#'
#' This function is used inside the R6 active method and allows us
#' to read the selected dataframe and prevents overwriting
#'
#' @param name the name of the readonly object
#' @param value any attempt at assignment to the readonly object
#' @family Metacore
#' @noRd
#'
readonly <- function(name) {
   private <- NULL
   inside <- function(value) {
      name <- attr(sys.function(sys.parent()), "name")
      if (missing(value)) {
         private[[paste0(".", name)]]
      } else {
         cli_abort("{name} is read only", call. = FALSE)
      }
   }
   attributes(inside) <- list(name = name)
   inside
}

#' Select method to subset by a single dataframe
#' @param value the dataframe to subset by
#'
MetaCore_filter <- function(value) {
   private$.ds_spec <- private$.ds_spec %>% filter(dataset == value)
   if (nrow(private$.ds_spec) == 0) {
      cli_abort("{value} is not a dataset in the metacore object", call. = FALSE)
   }
   private$.ds_vars <- private$.ds_vars %>% filter(dataset == value)
   private$.value_spec <- private$.value_spec %>% filter(dataset == value)


   # Need clarity on X.Y.Z situation: SUPPY8.QVAL
   private$.var_spec <- private$.var_spec %>%
      # variables have the dataset prefix so we make this into its own column
      mutate(
         dataset = ifelse(str_detect(variable, "\\."), str_extract(variable, "^.*(?=\\.)"), ""),
         variable = str_remove(variable, "^.*\\.")
      ) %>%
      # then keep the variables that occur once or in the dataset to filter
      filter(dataset == "" | dataset == value) %>%
      # remove the temporary column
      select(-dataset) %>%
      # right join
      right_join(private$.ds_vars %>% select(variable),
                 by = "variable",
                 multiple = "all"
      ) %>%
      distinct(variable, .keep_all = TRUE) # for when duplicates gett through and have different lables but the same name

   # Get values/variables that need derivations
   val_deriv <- private$.value_spec %>%
      distinct(.data$derivation_id) %>%
      na.omit()

   private$.derivations <- private$.derivations %>%
      right_join(val_deriv, by = "derivation_id", multiple = "all")

   private$.codelist <- private$.codelist %>%
      right_join(private$.value_spec %>%
                    distinct(.data$code_id) %>%
                    na.omit(), by = "code_id", multiple = "all")

   private$.supp <- private$.supp %>% filter(dataset == value)
}

#' The base Metacore R6 Class (original 7-table schema)
#' @family Metacore
#' @noRd
MetaCore <- R6::R6Class("Metacore",
                        public = list(
                           initialize = MetaCore_initialize,
                           print = MetaCore_print,
                           validate = MetaCore_validate,
                           metacore_filter = MetaCore_filter
                        ),
                        private = list(
                           .ds_spec = tibble(),
                           .ds_vars = tibble(),
                           .var_spec = tibble(),
                           .value_spec = tibble(),
                           .derivations = tibble(),
                           .codelist = tibble(),
                           .supp = tibble(),
                           .ds_len = NA,
                           .ds_names = list(),
                           .ds_labels = list(),
                           .greet = function(quiet = FALSE) {
                              cli_alert_success("Metadata successfully imported")
                              cli_inform(c("i" = "To use the {.obj Metacore} object with {.pkg metatools} package, first subset a dataset using {.fn metacore::select_dataset}"))
                           }
                        ),
                        active = list(
                           ds_spec = readonly("ds_spec"),
                           ds_vars = readonly("ds_vars"),
                           var_spec = readonly("var_spec"),
                           value_spec = readonly("value_spec"),
                           derivations = readonly("derivations"),
                           codelist = readonly("codelist"),
                           supp = readonly("supp")
                        )
)



#' R6 Class wrapper to create your own metacore object
#'
#' @param ds_spec contains each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to have the same derivation
#' @param codelist contains the code/decode information
#' @param supp contains the idvar and qeval information for supplemental variables
#' @param study_level contains study-level metadata used when generating a
#'   Define.xml (study name, description, protocol, standard and Define-XML
#'   versions, language). Optional; a single-row table.
#' @param documents contains references to external documents (e.g. the Analysis
#'   Data Reviewer's Guide) used for Define.xml page references. Optional; one
#'   row per document with `document_id`, `title` and `href`.
#' @param comments contains variable comments used for Define.xml CommentDef
#'   elements. Optional; one row per comment with `comment_id` and `comment`.
#'   Links to variables via `value_spec$comment_id`.
#' @param define_fields logical; when `TRUE` the object includes the extended
#'   columns required for Define.xml generation (`class`, `repeating`,
#'   `reference`, `purpose` in `ds_spec`; `role` in `ds_vars`; `where_label`
#'   and `comment_id` in `value_spec`; `method_name`, `method_type`,
#'   `document_id`, `pages` in `derivations`) as well as the `study_level`,
#'   `documents`, and `comments` tables. When `FALSE` (default) those fields
#'   are stripped so the object matches the original metacore schema.
#' @param quiet `r lifecycle::badge("superseded")` Option to quietly load in, this
#'   will suppress warnings, but not errors. Expects either `TRUE` or `FALSE`.
#'   Default behaviour is `FALSE`. As of v0.3.0 this argument is deprecated in favour
#'   of `verbose`.
#' @param verbose A character string specifying the desired verbosity level.
#'   Must be one of:
#'   \describe{
#'     \item{"message"}{ (default) Messages and warnings are handled normally.}
#'     \item{"warn"}{Messages are suppressed, but warnings are allowed.}
#'     \item{"collapse"}{Warnings are collapsed into a single message indicating the
#'     number of suppressed warnings.}
#'     \item{"silent"}{Both messages and warnings are suppressed.}
#'   }
#'
#' @family Metacore
#'
#' @export
metacore <- function(ds_spec = NULL, ds_vars = NULL, var_spec = NULL, value_spec = NULL,
                     derivations = NULL, codelist = NULL, supp = NULL, study_level = NULL,
                     documents = NULL, comments = NULL, define_fields = FALSE,
                     quiet = deprecated(), verbose = "message") {

   # Check if user has supplied `quiet` instead of `verbose`
   if (lifecycle::is_present(quiet)) {
      deprecate_soft(when = "0.3.0", what = "metacore(quiet)", with = "metacore(verbose)")
   } else {
      quiet <- FALSE
   } # Else deal with deprecated argument for compatability

   with_verbosity(
      {
         # Fill NULL tables with empty schema using the appropriate schema.
         schema <- if (define_fields) define_column_schema() else base_column_schema()
         if (is.null(ds_spec))     ds_spec     <- schema$.ds_spec
         if (is.null(ds_vars))     ds_vars     <- schema$.ds_vars
         if (is.null(var_spec))    var_spec    <- schema$.var_spec
         if (is.null(value_spec))  value_spec  <- schema$.value_spec
         if (is.null(derivations)) derivations <- schema$.derivations
         if (is.null(codelist))    codelist    <- schema$.codelist
         if (is.null(supp))        supp        <- schema$.supp

         # Signal deprecation warning for ds_vars$keep column.
         if (!is.null(ds_vars) && "keep" %in% names(ds_vars)) {
            cli_warn(c("The column {.var ds_vars$keep} in the {.var ds_vars} table was deprecated
as of 0.3.0 in favour of {.var ds_vars$mandatory} and will be removed in a future release.
The input for the supplied column {.var keep} has been mapped to the new column {.var mandatory}."))

            ds_vars <- ds_vars %>%
               mutate(mandatory = keep) %>%
               select(-keep)
         }

         if (define_fields) {
            MetaCoreDefine$new(
               ds_spec = ds_spec, ds_vars = ds_vars, var_spec = var_spec,
               value_spec = value_spec, derivations = derivations,
               codelist = codelist, supp = supp,
               study_level = study_level, documents = documents, comments = comments,
               quiet = quiet, verbose = verbose
            )
         } else {
            MetaCore$new(
               ds_spec = ds_spec, ds_vars = ds_vars, var_spec = var_spec,
               value_spec = value_spec, derivations = derivations,
               codelist = codelist, supp = supp,
               quiet = quiet, verbose = verbose
            )
         }
      },
      quiet,
      verbose
   )
}


#' Select metacore object to single dataset
#'
#' @param .data the metacore object of dataframes
#' @param dataset the specific dataset to subset by
#' @param simplify return a single dataframe
#' @param quiet `r lifecycle::badge("superseded")` Option to quietly load in, this
#'   will suppress warnings, but not errors. Expects either `TRUE` or `FALSE`.
#'   Default behaviour is `FALSE`. As of v0.3.0 this argument is deprecated in favour
#'   of `verbose`.
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
#' @return a filtered subset of the metacore object
#' @export
select_dataset <- function(.data, dataset, simplify = FALSE, quiet = deprecated(), verbose = "message") {
   # Check if user has supplied `quiet` instead of `verbose`
   if (lifecycle::is_present(quiet)) {
      deprecate_soft(when = "0.3.0", what = "select_dataset(quiet)", with = "select_dataset(verbose)")
   } else {
      quiet <- FALSE
   } # Else deal with deprecated argument for compatability

   with_verbosity(
      {
         cl <- .data$clone()
         cl$metacore_filter(dataset)

         if (simplify) {
            test <- list(
               cl$ds_vars,
               cl$var_spec,
               cl$value_spec,
               cl$derivations,
               select(cl$codelist, code_id, codes),
               cl$supp
            ) %>%
               reduce(left_join)
         } else {
            test <- DatasetMeta$new(metacore = cl, quiet = quiet)
         }
      },
      quiet,
      verbose
   )
}


#' Get Control Term
#'
#' Returns the control term (a vector for permitted values and a tibble for code
#' lists) for a given variable. The dataset can be optionally specified if there
#' is different control terminology for different datasets
#'
#' @param metacode metacore object
#' @param variable A variable name to get the controlled terms for. This can
#'   either be a string or just the name of the variable
#' @param dataset A dataset name. This is not required if there is only one set
#'   of control terminology across all datasets
#'
#' @return a vector for permitted values and a 2-column tibble for codelists
#' @export
#'
#' @importFrom rlang as_label enexpr as_name
#'
#' @examples
#' \dontrun{
#' meta_ex <- spec_to_metacore(metacore_example("p21_mock.xlsx"))
#' get_control_term(meta_ex, QVAL, SUPPAE)
#' get_control_term(meta_ex, "QVAL", "SUPPAE")
#' }
get_control_term <- function(metacode, variable, dataset = NULL) {
   var_str <- ifelse(str_detect(as_label(enexpr(variable)), "\""),
                     as_name(variable), as_label(enexpr(variable))
   )
   dataset_val <- ifelse(str_detect(as_label(enexpr(dataset)), "\""),
                         as_name(dataset), as_label(enexpr(dataset))
   ) # to make the filter more explicit
   if (!var_str %in% metacode$value_spec$variable) {
      cli_abort("{var_str} not found in the value_spec table. Please check the variable name")
   }
   if (dataset_val == "NULL") {
      var_code_id <- metacode$value_spec %>%
         filter(variable == var_str) %>%
         pull(code_id) %>%
         unique()
   } else {
      subset_data <- metacode$value_spec %>%
         filter(dataset == dataset_val)
      if (nrow(subset_data) == 0) {
         cli_abort("{dataset_val} not found in the value_spec table. Please check the dataset name")
      }
      var_code_id <- subset_data %>%
         filter(variable == var_str) %>%
         pull(code_id) %>%
         unique()
   }
   if (length(var_code_id) > 1) {
      cli_abort("{var_str} does not have a unique control term, consider spcificing a dataset")
   }
   ct <- metacode$codelist %>%
      filter(code_id == var_code_id) %>%
      pull(codes)
   if (length(ct) == 0) {
      cli_inform("{var_str} has no control terminology")
   } else {
      return(ct[[1]])
   }
}


#' Get Dataset Keys
#'
#' Returns the dataset keys for a given dataset
#'
#' @param metacode metacore object
#' @param dataset A dataset name
#'
#' @return a 2-column tibble with dataset key variables and key sequence
#' @export
#'
#' @importFrom rlang as_label enexpr as_name
#'
#' @examples
#' \dontrun{
#' meta_ex <- spec_to_metacore(metacore_example("p21_mock.xlsx"))
#' get_keys(meta_ex, "AE")
#' get_keys(meta_ex, AE)
#' }
get_keys <- function(metacode, dataset) {
   dataset_val <- ifelse(str_detect(as_label(enexpr(dataset)), "\""),
                         as_name(dataset), as_label(enexpr(dataset))
   ) # to make the filter more explicit

   subset_data <- metacode$ds_vars %>%
      filter(dataset == dataset_val)
   if (nrow(subset_data) == 0) {
      cli_abort("{dataset_val} not found in the ds_vars table. Please check the dataset name")
   }

   keys <- subset_data %>%
      filter(!is.na(key_seq)) %>%
      select(variable, key_seq)

   keys <- keys[order(keys$key_seq), ]

   return(keys)
}


#' save metacore object
#'
#' @param metacore_object the metacore object in memory to save to disc
#' @param path file path and file name to save metacore object
#'
#' @return an .rda file
#' @export
#'
save_metacore <- function(metacore_object, path = NULL) {
   # if no path save to working directory
   # with same name as object
   if (is.null(path)) {
      nm <- deparse(substitute(metacore_object))
      path <- paste0(nm, ".rds")

      # check the suffix of the path
   } else {
      suffix <- str_extract(path, "\\.\\w*$")
      # if the extension is .rda keep it
      if (suffix == ".rds") {
         path <- path

         # otherwise we need to replace it with .rda
      } else {
         prefix <- str_remove(path, "\\.\\w*$")
         path <- paste0(prefix, ".rds")
      }
   }
   saveRDS(metacore_object, path)
}

#' load metacore object
#'
#' @param path location of the metacore object to load into memory
#'
#' @return metacore object in memory
#' @export
load_metacore <- function(path = NULL) {
   if (is.null(path)) {
      rdss <- list.files(".", ".rds")
      if (length(rdss) == 0) {
         cli_abort("please supply path to metacore object ending with extension .rds", call. = FALSE)
      } else {
         cli_abort("metacore object path required, did you mean:",
                   paste("   ", rdss, sep = "\n   "),
                   call. = FALSE
         )
      }
   }
   readRDS(path)
}
