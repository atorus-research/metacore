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
         cat("\n")
         warning(
            "The following variable(s) do not have labels and lengths: ",
            paste("   ", var_check_dbl, sep = "\n   "),
            "\n\n",
            call. = FALSE
         )
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
         pull(.data$full) %>%
         str_c(collapse = ", ")
      message <- paste("The following variables are in the ds_vars table, but don't have value specs:\n",
                       variables, "\n\n")
      warning(message, call. = FALSE)
   }
   # Check the variables in value spec that aren't in ds_vars
   not_in_ds <- anti_join(value_vars, ds_vars, by = c("dataset", "variable"))
   if(nrow(not_in_ds) != 0){
      variables <- not_in_ds %>%
         pull(.data$variable) %>%
         str_c(collapse = ", ")
      message <- paste("The following variables are have value specifications, but aren't in the ds_vars table:\n",
                       variables, "\n\n")
      warning(message, call. = FALSE)
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
         pull(.data$variable) %>%
         str_c(collapse = "\n ")
      message <- paste("The following variables have derivation ids not found in the derivations table:\n",
                       variables, "\n\n")
      warning(message, call. = FALSE)
   }
   # Check the derivations in deriavtion that aren't  in value spec
   not_in_deriv <- anti_join(derivations, deriv_vars, by = c("derivation_id"))
   if(nrow(not_in_deriv) != 0){
      deriv <- not_in_deriv %>%
         mutate(message = paste0(.data$derivation_id, ": ", .data$derivation)) %>%
         pull(.data$message) %>%
         str_c(collapse = "\n ")
      message <- paste("The following derivations are never used:\n",
                       deriv, "\n\n")
      warning(message, call. = FALSE)
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
         pull(.data$variable) %>%
         str_c(collapse = "\n ")
      message <- paste("The following variables hace code ids not found in the codelist(s):\n",
                       variables, "\n")
      warning(message, call. = FALSE)
   }
   # Check the code_ids in codelist that aren't in value spec
   not_in_cl <- anti_join(codelist, code_vars, by = c("code_id"))
   if(nrow(not_in_cl)){
      cl_nm <- not_in_cl %>%
         pull(.data$name) %>%
         str_c(collapse = "\n ")
      message <- paste("The following codelist(s) are never used:\n",
                       cl_nm, "\n\n")
      warning(message, call. = FALSE)
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
      warning("Supp table contains non-unique dataset/variable combinations")
   }

   ds_vars <- ds_vars %>%
      filter(.data$supp_flag)

   #Check the variables in ds_vars that don't have value specs
   not_in_supp <- anti_join(ds_vars, supp, by = c("dataset", "variable"))
   if(nrow(not_in_supp) != 0){
      variables <- not_in_supp %>%
         mutate(full = str_c(.data$dataset, .data$variable, sep = ".")) %>%
         pull(.data$full) %>%
         str_c(collapse = ", ")
      message <- paste("The following variables are in the ds_vars table and tagged as supplement, but don't have supp specs:\n",
                       variables, "\n\n")
      warning(message, call. = FALSE)
   }
   # Check the variables in value spec that aren't in ds_vars
   not_in_ds <- anti_join(supp, ds_vars, by = c("dataset", "variable"))
   if(nrow(not_in_ds) != 0){
      variables <- not_in_ds %>%
         pull(.data$variable) %>%
         str_c(collapse = ", ")
      message <- paste("The following variables are have supp specifications, but aren't in the ds_vars table:\n",
                       variables, "\n\n")
      warning(message, call. = FALSE)
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
         warning(print_message, call. = FALSE)
         FALSE
      } else if(!setequal(names(tbl),col_names[[name]])){
         # writes a message if the column names don't match
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste0("'", ., "' has incorrect column names. It should be:\n",
                  str_c(col_names[[name]], collapse = ", "), "\n")
         warning(print_message, call. = FALSE)
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
   "ds_vars",     "core",          check_words("Expected", "Required", "Permissible", "Conditionally Required", "Conditionally Expected"), TRUE,
   "ds_vars",     "supp_flag",     is.logical,                  TRUE,
   "var_spec",    "variable",      is.character,                FALSE,
   "var_spec",    "type",          is.character,                TRUE,
   "var_spec",    "length",        is.numeric,                  TRUE,
   "var_spec",    "label",         is.character,                TRUE,
   "var_spec",    "format",        is.character,                TRUE,
   "var_spec",    "common",        is.logical,                  TRUE,
   "value_spec",  "type",          is.character,                TRUE,
   "value_spec",  "sig_dig",       is.integer,                  TRUE,
   "value_spec",  "origin",        is.character,                TRUE,
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
   errors <-  map(messages, "error") %>%
      compact() %>%
      paste0(., collapse = "\n\n")
   if(errors != "")
      stop(paste0(errors, "\n\n"), call. = FALSE)

   # warnings
   warnings <- map(messages, "warning") %>%
      compact() %>%
      paste0(., collapse = "\n\n")
   if(warnings != "")
      warning(paste0(warnings, "\n\n"), call. = FALSE)



}
