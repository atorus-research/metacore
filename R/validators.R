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
         anti_join(., var_spec, by = c("var_name" = "variable")) %>%
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

var_name_check <- function(envrionment){
    # Set the name as they should be
   col_names <- list(.ds_spec = c("dataset", "structure", "label"),
                     .ds_vars = c("dataset", "variable", "key_seq", "keep" ),
                     .var_spec = c("variable", "length", "label"),
                     .value_spec = c("type", "origin", "code_id", "dataset", "variable", "where", "derivation_id"),
                     .derivations = c("derivation_id", "derivation"),
                     .codelist= c("code_id", "names","type", "codes"),
                     .change_log = c("table_chg", "column_chg", "what_chg"))
   # Get the tables and table names from the envrionment
   tbl_name <- ls(envrionment, all.names = TRUE)
   tbls <- map(tbl_name, get, envir = envrionment)

   map2(tbl_name, tbls, function(name, tbl){
      if(is.null(tbl)){
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste("is null")
         warning(print_message, call. = FALSE)
      } else if(!all(names(tbl) %in% col_names[[name]])){
         print_message <- name %>%
            str_remove("[:punct:]") %>%
            paste("has incorrect column names. It should be:\n",
                  str_c(col_names[[name]], collapse = ", "))
         warning(print_message, call. = FALSE)

      }
   })

}
