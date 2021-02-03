#' The DataDef R6 Class
#'
#' This uses the initialize, print, and validate functions above to create a single object
#' The user can query
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


datadef <- function(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list) {
   DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list)
}
