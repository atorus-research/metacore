#' This file includes the internal functions needed to create the readonly
#' DataDef R6 object
#'
#' @param ds_spec contians each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus
#'   dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs
#'   for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to
#'   have the same derivation
#' @param code_list contains the code/decode information
#'
#' @family DataDef
#' @noRd
#'
#'
DataDef_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist){

   private$.ds_spec <- ds_spec %>%
      add_labels(dataset = "Dataset Name",
                 structure = "Value Structure",
                 label = "Value Label")

   private$.ds_vars <- ds_vars %>%
      add_labels(dataset = "Dataset Name",
                 variable = "Variable Name",
                 key_seq = "Sequence Key",
                 keep = "Keep (Boolean)",
                 core = "ADaM core (Expected, Required, Permissable)")

   private$.var_spec <- var_spec %>%
      add_labels(variable = "Variable Name",
                 length = "Variable Length",
                 label = "Variable Label",
                 type = "Variable Class",
                 common = "Common Across ADaM")

   private$.value_spec <- value_spec %>%
      add_labels(type = "Value Type",
                 orgin = "Origin of Value",
                 code_id = "ID of the Code List",
                 dataset = "Dataset Name",
                 variable = "Variable Name",
                 where = "Value of the Variable",
                 derivation_id = "ID of Derivation")

   private$.derivations <- derivations %>%
      add_labels(derivation_id = "ID of Derivation",
                 derivation = "Derivation")

   private$.codelist <- code_list %>%
      add_labels(code_id = "ID of the Code List",
                 names = "Name of the Code List",
                 type = "Code List/Permitted Values/External Library",
                 codes = "List of Codes")

   self$validate()
   message("\n Metadata successfully imported")
}


#' DataDef class print function
#'
#' @param ... pass in the dataframes to be validated
#' @family DataDef
#' @noRd
#'
DataDef_print <- function(...){
   ds_len <- private$.ds_spec %>% pull(.data$dataset) %>% length()
   paste0("DataDef object contains metadata for ", ds_len, " datasets\n") %>%
      cat()
}


#' DataDef R6 object validation function
#'
#' This checks that the labels and lengths of ds_vars match var_spec
#' @family DataDef
#' @noRd
#'
DataDef_validate <-  function() {
   if(var_name_check(private)){

      check_columns(private$.ds_spec,
                    private$.ds_vars,
                    private$.var_spec,
                    private$.value_spec,
                    private$.derivations,
                    private$.codelist
      )

      ds_vars_check(private$.ds_vars, private$.var_spec)
      value_check(private$.ds_vars, private$.value_spec)
      derivation_check(private$.value_spec, private$.derivations)
      codelist_check(private$.value_spec, private$.codelist)
   } else {
      warning("Other checks were not preformed, because column names were incorrect",
              call. = FALSE)
   }
}



#' readonly function factory
#'
#' This function is used inside the R6 active method and allows us
#' to read the selected dataframe and prevents overwriting
#'
#' @param name the name of the readonly object
#' @param value any attempt at assignment to the readonly object
#' @family DataDef
#' @noRd
#'
readonly <- function(name) {
   private <- NULL
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


#' The DataDef R6 Class
#'
#' This uses the initialize, print, and validate functions above to create a single object
#' The user can query
#'
#' @family DataDef
#' @noRd
#
DataDef <- R6::R6Class("DataDef",
                       public = list(
                          initialize = DataDef_initialize,
                          print = DataDef_print,
                          validate =  DataDef_validate
                       ),
                       private = list(
                          .ds_spec = tibble(dataset = character(), label = character()),
                          .ds_vars = tibble(dataset = character(), variable = character(), keep = logical(),
                                            key = integer(), codelist = character(), origin = character(),
                                            derivation_id = character()),
                          .var_spec = tibble(variable = character(), label = character(), length = integer()),
                          .value_spec = tibble(dataset = character(),
                                               variable = character(),
                                               where  = character(),
                                               type = character(),
                                               codelist = character(),
                                               origin = character(),
                                               derivation_id = integer()),
                          .derivations = tibble(derivation_id = integer(), derivation = character()),
                          # code_type == df | permitted_val | external_lib
                          .codelist = tibble(code_id = character(), code_type = character(), codelist = list()),
                          .change_log = tibble(table_chg = character(), column_chg = character(), what_chg = list())
                       ),
                       active = list(
                          ds_spec = readonly('ds_spec'),
                          ds_vars =  readonly('ds_vars'),
                          var_spec = readonly('var_spec'),
                          value_spec = readonly('value_spec'),
                          derivations = readonly('derivations'),
                          codelist = readonly('codelist'),
                          changelog = readonly('changelog')
                       )
)


#' R6 Class wrapper to create your own datadef object
#'
#' @param ds_spec contians each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to have the same derivation
#' @param code_list contains the code/decode information
#'
#' @family DataDef
#'
#' @export
#'
datadef <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist) {
   DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist)
}
