#' Checks the variables in ds_var are in var_spec
#'
#' @param ds_vars ds_var table
#' @param var_spec var_spec table
#'
#' @return writes warning to console if there is an issue
#' @noRd
ds_vars_check <- function(ds_vars, var_spec){
   var_check <- anti_join(ds_vars, var_spec, by = "variable")

   if(var_check %>% nrow() != 0){
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
                       variables)
      warning(message, call. = FALSE)
   }
   # Check the variables in value spec that aren't in ds_vars
   not_in_ds <- anti_join(value_vars, ds_vars, by = c("dataset", "variable"))
   if(nrow(not_in_ds) != 0){
      variables <- not_in_ds %>%
         pull(.data$variable) %>%
         str_c(collapse = ", ")
      message <- paste("The following variables are have value specifications, but aren't in the ds_vars table:\n",
                       variables)
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
         str_c(collapse = ", ")
      message <- paste("The following variables are missing derivations:\n",
                       variables)
      warning(message, call. = FALSE)
   }
   # Check the derivations in deriavtion that aren't  in value spec
   not_in_deriv <- anti_join(derivations, deriv_vars, by = c("derivation_id"))
   if(nrow(not_in_deriv) != 0){
      deriv <- not_in_deriv %>%
         pull(.data$derivation) %>%
         str_c(collapse = ", ")
      message <- paste("The following derivations are never used:\n",
                       deriv)
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
         str_c(collapse = ", ")
      message <- paste("The following variables are missing codelist(s):\n",
                       variables)
      warning(message, call. = FALSE)
   }
   # Check the code_ids in codelist that aren't in value spec
   not_in_cl <- anti_join(codelist, code_vars, by = c("code_id"))
   if(nrow(not_in_cl)){
      cl_nm <- not_in_cl %>%
         pull(.data$names) %>%
         str_c(collapse = ", ")
      message <- paste("The following codelist(s) are never used:\n",
                       cl_nm)
      warning(message, call. = FALSE)
   }
}

#' Column Names by dataset
#'
#' @return list of colum names by dataset
#' @noRd
col_vars <- function(){
   list(.ds_spec = c("dataset", "structure", "label"),
        .ds_vars = c("dataset", "variable", "key_seq", "keep", "core"),
        .var_spec = c("variable", "length", "label", "type", "common"),
        .value_spec = c("type", "origin", "code_id", "dataset", "variable", "where", "derivation_id"),
        .derivations = c("derivation_id", "derivation"),
        .codelist= c("code_id", "names","type", "codes"),
        .change_log = c("table_chg", "column_chg", "what_chg"))
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
   # Get the tables and table names from the envrionment
   tbl_name <- ls(envrionment, all.names = TRUE)
   tbls <- map(tbl_name, get, envir = envrionment)
   # Checks is names match the table above, returns T if so F else. If the names
   # don't match, will also produce a warning of what the names should be
   map2_lgl(tbl_name, tbls, function(name, tbl){
      if(is.null(tbl)){
         # Checks for null tables
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste("is null")
         warning(print_message, call. = FALSE)
         FALSE
      } else if(!all(names(tbl) %in% col_names[[name]])){
         # writes a message if the column names don't match
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste("has incorrect column names. It should be:\n",
                  str_c(col_names[[name]], collapse = ", "))
         warning(print_message, call. = FALSE)
         FALSE
      } else {
         TRUE
      }
   }) %>%
      all()

}
