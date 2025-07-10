#' Checks the variables in ds_var are in var_spec
#'
#' @param ds_vars ds_var table
#' @param var_spec var_spec table
#'
#' @return writes warning to console if there is an issue
#' @noRd
ds_vars_check <- function(ds_vars, var_spec){
   var_check <- anti_join(ds_vars, var_spec, by = "variable")

   if(var_check %>% nrow()){
      var_ls <- var_check %>%
         pull(.data$variable) %>%
         unique()

      var_check_dbl <- ds_vars %>%
         filter(.data$variable %in% var_ls) %>%
         mutate(var_name = paste0(.data$dataset, ".", .data$variable)) %>%
         anti_join(var_spec, by = c("var_name" = "variable")) %>%
         pull(.data$variable) %>%
         unique()

      if(var_check_dbl %>% length() != 0){
         cli_warn(c(
            "The following variable(s) do not have labels and lengths:",
            "i" = ansi_collapse(var_check_dbl, last = ", ")
         ), call. = FALSE)
      }
   }
}


#' Check Values
#'
#'
#' Check the value spec table works with the ds_var tables. All variables in the
#' ds_var should be in the value_spec and all variables in value_spec  should be
#' in ds_vars
#' @param ds_vars ds_vars table
#' @param value_spec value_spec table
#'
#' @return writes warning to console if there is an issue
#' @noRd
value_check <- function(ds_vars, value_spec){
   value_vars <- value_spec %>%
      distinct(.data$dataset, .data$variable)

   #Check the variables in ds_vars that don't have value specs
   not_in_val <- anti_join(ds_vars, value_vars, by = c("dataset", "variable"))
   if(nrow(not_in_val) != 0){
      variables <- not_in_val %>%
         mutate(full = str_c(.data$dataset, .data$variable, sep = ".")) %>%
         pull(.data$full)
      cli_warn(c(
         "The following variables are in the ds_vars table, but don't have value specs:",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
   # Check the variables in value spec that aren't in ds_vars
   not_in_ds <- anti_join(value_vars, ds_vars, by = c("dataset", "variable"))
   if(nrow(not_in_ds) != 0){
      variables <- not_in_ds %>%
         pull(.data$variable)
      cli_warn(c(
         "The following variables have value specifications, but aren't in the ds_vars table:",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
}


#' Derivation check
#'
#' @param value_spec value_spec table
#' @param derivations derivation table
#'
#' @return writes warning to console if there is an issue
#' @noRd
derivation_check <- function(value_spec, derivations){
   deriv_vars <- value_spec %>%
      filter(!is.na(.data$derivation_id)) %>%
      distinct(.data$variable,.data$ derivation_id)

   #Check the variables that don't have derivations in derivations
   not_in_val <- anti_join(deriv_vars, derivations, by = c("derivation_id"))
   if(nrow(not_in_val) != 0){
      variables <- not_in_val %>%
         pull(.data$variable)
      cli_warn(c(
         "The following variables have derivation ids not found in the derivations table:",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
   # Check the derivations in deriavtion that aren't  in value spec
   not_in_deriv <- anti_join(derivations, deriv_vars, by = c("derivation_id"))
   if(nrow(not_in_deriv) != 0){
      deriv <- not_in_deriv %>%
         mutate(message = paste0(.data$derivation_id)) %>%
         pull(.data$message)
      cli_warn(c(
         "The following derivations are never used:",
         "i" = ansi_collapse(deriv, last = ", ")
      ), call. = FALSE)
   }

}

#' Codelist Check
#'
#' @param value_spec value spec table
#' @param codelist codelist table
#'
#' @return writes warning to console if there is an issue
#' @noRd
codelist_check <- function(value_spec, codelist){
   code_vars <- value_spec %>%
      filter(!is.na(.data$code_id)) %>%
      distinct(.data$variable, .data$code_id)

   #Check the variables in don't codelists have codelist
   not_in_val <- anti_join(code_vars, codelist, by = c("code_id"))
   if(nrow(not_in_val)){
      variables <- not_in_val %>%
         pull(.data$variable)
      cli_warn(c(
         "The following variables have code ids not found in the codelist(s):",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
   # Check the code_ids in codelist that aren't in value spec
   not_in_cl <- anti_join(codelist, code_vars, by = c("code_id"))
   if(nrow(not_in_cl)){
      cl_nm <- not_in_cl %>%
         pull(.data$name)
      cli_warn(c(
         "The following codelists are never used:",
         "i" = ansi_collapse(cl_nm, last = ", ")
      ), call. = FALSE)
   }
}


#' Check Supp
#'
#'
#' Check the supp table works with the ds_var tables. All variables in the
#' ds_var  with a TRUE supp flag should be in the supp and all variables in supp
#' should be in ds_vars
#' @param ds_vars ds_vars table
#' @param supp supp table
#'
#' @return writes warning to console if there is an issue
#' @noRd
supp_check <- function(ds_vars, supp){
   dist_test <- supp %>%
      distinct(.data$dataset, .data$variable) %>%
      nrow() == nrow(supp)
   if(!dist_test){
      cli_warn("Supp table contains non-unique dataset/variable combinations")
   }

   ds_vars <- ds_vars %>%
      filter(.data$supp_flag)

   #Check the variables in ds_vars that don't have value specs
   not_in_supp <- anti_join(ds_vars, supp, by = c("dataset", "variable"))
   if(nrow(not_in_supp) != 0){
      variables <- not_in_supp %>%
         mutate(full = str_c(.data$dataset, .data$variable, sep = ".")) %>%
         pull(.data$full)
      cli_warn(c(
         "The following variables are in the ds_vars table and tagged as supplement, but don't have supp specs:",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
   # Check the variables in value spec that aren't in ds_vars
   not_in_ds <- anti_join(supp, ds_vars, by = c("dataset", "variable"))
   if(nrow(not_in_ds) != 0){
      variables <- not_in_ds %>%
         pull(.data$variable)
      cli_warn(c(
         "The following variables are have supp specifications, but aren't in the ds_vars table:",
         "i" = ansi_collapse(variables, last = ", ")
      ), call. = FALSE)
   }
}


#' Column Names by dataset
#'
#' @return list of column names by dataset
#' @noRd
col_vars <- function(){
   list(.ds_spec = c("dataset", "structure", "label"),
        .ds_vars = c("dataset", "variable", "key_seq", "order","keep", "core", "supp_flag"),
        .var_spec = c("variable", "length", "label", "type", "common", "format"),
        .value_spec = c("dataset", "variable", "type", "origin","sig_dig", "code_id", "where", "derivation_id"),
        .derivations = c("derivation_id", "derivation"),
        .codelist= c("code_id", "name","type", "codes"),
        .supp = c("dataset", "variable", "idvar", "qeval"))
}


#' Check Variable names
#'
#' @param envrionment the private environment of the object
#'
#' @return warning messages to the console if there is an issue
#' @noRd
var_name_check <- function(envrionment){
    # Set the name as they should be
   col_names <- col_vars()
   # Get the tables and table names from the environment
   tbl_name <- ls(envrionment, all.names = TRUE)
   tbls <- map(tbl_name, get, envir = envrionment)
   # Checks is names match the table above, returns T if so F else. If the names
   # don't match, will also produce a warning of what the names should be
   map2_lgl(tbl_name, tbls, function(name, tbl){
      name
      if(is.null(tbl)){
         # Checks for null tables
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste("is null")
         cli_warn(print_message, call. = FALSE)
         FALSE
      } else if(!setequal(names(tbl),col_names[[name]])){
         # writes a message if the column names don't match
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste0("'", ., "' has incorrect column names. It should be:\n",
                  str_c(col_names[[name]], collapse = ", "), "\n")
         cli_warn(print_message, call. = FALSE)
         FALSE
      } else {
         TRUE
      }
   }) %>%
      all()

}



#' Column Data Check <- lol horrible name
#'
#' @return a data frame of the datasets, column
#' @noRd
#'
all_message <- function() {
   tribble(
   ~dataset,     ~var,             ~test,                 ~any_na_acceptable,
   "ds_spec",     "dataset",       is.character,                FALSE,
   "ds_spec",     "structure",     is.character,                TRUE,
   "ds_spec",     "label",         is.character,                TRUE,
   "ds_vars",     "dataset",       is.character,                FALSE,
   "ds_vars",     "variable",      is.character,                FALSE,
   "ds_vars",     "key_seq",       is.numeric,                  TRUE,
   "ds_vars",     "order",         is.numeric,                  TRUE,
   "ds_vars",     "keep",          is.logical,                  TRUE,
   "ds_vars",     "core",          check_words("Expected", "Required", "Permissible", "Conditionally Required", "Conditionally Expected", NA), TRUE,
   "ds_vars",     "supp_flag",     is.logical,                  TRUE,
   "var_spec",    "variable",      is.character,                FALSE,
   "var_spec",    "type",          is.character,                TRUE,
   "var_spec",    "length",        is.numeric,                  TRUE,
   "var_spec",    "label",         is.character,                TRUE,
   "var_spec",    "format",        is.character,                TRUE,
   "var_spec",    "common",        is.logical,                  TRUE,
   "value_spec",  "type",          is.character,                TRUE,
   "value_spec",  "sig_dig",       is.integer,                  TRUE,
   "value_spec",  "origin",        function(x){str_detect(x, "collected|derived|assigned|protocol|predecessor|crf.*")||is.na(x)},                TRUE,
   "value_spec",  "code_id",       is.character,                TRUE,
   "value_spec",  "dataset",       is.character,                FALSE,
   "value_spec",  "where",         is.character,                TRUE,
   "value_spec",  "derivation_id", is.character,                TRUE,
   "derivations", "derivation_id", is.character,                FALSE,
   "derivations", "derivation",    is.character,                TRUE,
   "codelist",    "code_id",      is.character,                FALSE,
   "codelist",    "name",        is.character,                TRUE,
   "codelist",    "codes",        function(x){!is.null(x)},    TRUE,
   "codelist",    "type",         is.character,                TRUE,
   "supp",        "dataset",       is.character,                FALSE,
   "supp",        "variable",      is.character,                FALSE,
   "supp",        "idvar",        is.character,                TRUE,
   "supp",        "qeval",        is.character,                TRUE,
)
}


#' Check all data frames include the correct types of columns
#'
#' This function checks for vector types and accepted words
#'
#' @param ds_spec dataset specification
#' @param ds_vars dataset variables
#' @param var_spec variable specification
#' @param value_spec value specification
#' @param derivations derivation information
#' @param codelist codelist information
#' @param supp supp information
#'
check_columns <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp) {


   messages <- purrr::pmap(all_message(),
               ~check_structure(
                  get(..1), sym(..2), ..3, ..4, ..1)
   )

   # errors
   errors <- map(messages, "error") %>%
      compact() %>%
      vapply(`[[`, character(1), 1)
   if (length(errors) > 0) {
      msg <- c(
         "Tried to load dataset metadata but exited with errors",
         set_names(errors, rep("x", length(errors)))
      )
      cli_abort(msg, call. = FALSE)
   }

   # warnings
   warnings <- map(messages, "warning") %>%
      compact()
   if (length(warnings) > 0) {
      for (warning in warnings) {
         cli_warn(warning[1], call. = FALSE)
      }
   }
}

#' Is metacore object
#'
#' @param x object to check
#'
#' @return `TRUE` if metacore, `FALSE` if not
#' @export
#'
#' @examples
#' # Loads in a metacore obj called metacore
#' load(metacore_example("pilot_ADaM.rda"))
#' is_metacore(metacore)
#'
is_metacore <- function(x){
   inherits(x, "Metacore")
}


#' Is DatasetMeta object
#'
#' @param x object to check
#'
#' @return `TRUE` if DatasetMeta, `FALSE` if not
#' @export
#'
#' @examples
#' load(metacore_example("pilot_ADaM.rda"))
#' adsl <- select_dataset(metacore, "ADSL", quiet = TRUE)
#' is_DatasetMeta("DUMMY")   # Expect FALSE
#' is_DatasetMeta(metacore)  # Expect FALSE
#' is_DatasetMeta(adsl)      # Expect TRUE
is_DatasetMeta <- function(x){
   inherits(x, "DatasetMeta")
}


#' Verify that the Class Type of an object is DatasetMeta with warnings
#'
#' @description
#' This function that is a wrapper to the functions `is_metacore` and
#' `is_DatasetMeta`.
#'
#' This function is not intended to be called directly by the user. It is
#' used as a guard clause in many features of the `{metatools}` package that are
#' intended only to be used with the subsetted Metacore object of class type
#' `DatasetMeta`. If either of the wrapped functions return `FALSE `then
#' execution is stopped and an appropriate error message is displayed.
#'
#' @param metacore An object whose class type needs to be checked.
#' @return Logical: TRUE if the class type of `metacore` is `DatasetMeta`,
#'   otherwise abort with errors.
#'
#' @export
#'
#' @examples
#' load(metacore_example("pilot_ADaM.rda"))
#' adsl <- select_dataset(metacore, "ADSL", quiet = TRUE)
#' \dontrun{
#' verify_DatasetMeta("DUMMY")   # Expect error
#' verify_DatasetMeta(metacore)  # Expect error
#' }
#' verify_DatasetMeta(adsl)      # Expect valid, i.e., return TRUE
verify_DatasetMeta <- function(metacore) {
   if (!is_metacore(metacore)) {
      cli_abort(col_red("The object supplied to the argument {.arg metacore} is not a Metacore object. You have supplied an object of class {class(metacore)}."))
   }

   if (!is_DatasetMeta(metacore)) {
      cli_abort(col_red("The object supplied to the argument {.arg metacore} is not a subsetted Metacore object. Use {.fn metacore::select_dataset} to subset metadata for the required dataset."))
   }

   return(TRUE)
}
