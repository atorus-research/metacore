#' This file includes the internal functions needed to create the readonly DataDef R6 object
DataDef_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, code_list){

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


#' The DataDef R6 Class
#'
#' This uses the initialize, print, and validate functions above to create a single object
#' The user can query
#'
#' @param ds_spec things and stuff
#' @param ds_vars things and stuff
#' @param var_spec things and stuff
#' @param value_spec things and stuff
#' @param derivations things and stuff
#' @param code_list things and stuff
#' @param ... things and stuff
#'
#'
#'
#' @field ds_spec First or full name of the person.
#' @field ds_vars First or full name of the person.
#' @field var_spec First or full name of the person.
#' @field value_spec First or full name of the person.
#' @field derivations First or full name of the person.
#' @field codelist First or full name of the person.
#' @field changelog First or full name of the person.
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
#' @export
datadef <- function(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list) {
   DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list)
}
