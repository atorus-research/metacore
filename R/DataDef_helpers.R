# This file includes the internal functions needed to create the readonly DataDef R6 object



#' DataDef class initilization function
#'
#' This function assigns the datasets to readonly object
#' and runs validation on those objects
#' @param ds_spec contians each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to have the same derivation
#' @param code_list contains the code/decode information

DataDef_initialize <- function(ds_spec, ds_vars, var_spec,
                               value_spec, derivations, code_list){

   private$.ds_spec <- ds_spec
   private$.ds_vars <- ds_vars
   private$.var_spec <- var_spec
   private$.value_spec <- value_spec
   private$.derivations <- derivations
   private$.codelist <- code_list

   self$validate()
   message("Metadata successfully imported")
   # TO DO: Cross-ref functions:
   # * derivations, codelist, variables x2
   # type coulnm has a limited number of types
}


#' DataDef class print function
DataDef_print <- function(...){
   # the domain name and how many data set specs
   cat(private$.ds_spec %>% as.character() %>% paste0(collapse = "\n"))
}


#' DataDef R6 object validation function
#'
#' This checks that the labels and lengths of ds_vars match var_spec
DataDef_validate <-  function() {

   var_check <- anti_join(private$.ds_vars, private$.var_spec, by = "variable")

   if(var_check %>% nrow() != 0){
      var_ls <- var_check %>%
         pull(variable) %>%
         unique()

      warning(
         "The following variable(s) do not have labels and lengths: ",
         paste("   ", var_ls, sep = "\n   "),
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
readonly <- function(name) {
   inside <- function(value) {
      name <- attr(sys.function(sys.parent()), "name")
      if (missing(value)) {
         private[[paste0(".", name)]]
      } else {
         stop(paste0(name, " is read only"), call. = FALSE)
      }
   }
   attributes(inside) <- list(name = name)
   inside
}


