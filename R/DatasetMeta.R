#' The DatasetMeta R6 Class
#'
#' This uses the initialize, print, and validate functions above to create a single object
#' The user can query
#'
#' @family Metacore
#' @noRd
#
DatasetMeta <- R6::R6Class("DatasetMeta",
  inherit = MetaCore,
  private = list(
     name  = NA,
     label = NA,
     num_vars = NA,

     greet = function() {
        cli_alert_success("{private$name} dataset successfully selected")
     }
  ),

  public = list(
     initialize = function(metacore) {
        super$initialize(
           ds_spec = metacore$ds_spec,
           ds_vars = metacore$ds_vars,
           var_spec = metacore$var_spec,
           value_spec = metacore$value_spec,
           derivations = metacore$derivations,
           codelist = metacore$codelist,
           supp = metacore$supp
        )
        private$name  = metacore$ds_spec$dataset[1]
        private$label = metacore$ds_spec$label[1]
        private$num_vars = metacore$ds_vars |> nrow()

        private$greet()
     },

     print = function() {
        cli_par()
        cli_rule(left = "Dataset specification object for: {private$name} ({private$label})")
        cli_text("The dataset contains {private$num_vars} variables")
        cli_end()

        cli_par()
        cli_rule(left = "The structure of the dataset specification object is")
        tables <- ls(envir = self)
        for (table in tables) {
           obj <- get(table, envir = self)
           if (!is.list(obj)) { next }

           cli_text("{table}: {typeof(table)} [{dim(obj)[1]} x {dim(obj)[2]}] {paste(names(obj), collapse = ' ')}")
        }
        cli_end()
     }
  )
)
