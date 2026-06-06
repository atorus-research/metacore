#' The DatasetMeta R6 Class
#'
#' A subclass of the class Metacore. Creates a programatic distinction between
#' the full specification object of type Metacore and the subsetted specification
#' for a single dataset of type DatasetMeta.
#'
#' @family Metacore
#' @noRd
#
DatasetMeta <- R6::R6Class("DatasetMeta",
  inherit = MetaCore,
  private = list(
    .name = NA,
    .label = NA,
    .num_vars = NA,
    .key_vars = NA,
    .greet = function(quiet, verbose) {
      with_verbosity(
        {
          cli_alert_success("{private$.name} dataset successfully selected")
        },
        quiet,
        verbose
      )
    }
  ),
  public = list(
    initialize = function(metacore, quiet = deprecated(), verbose = "message") {
      super$initialize(
        ds_spec = metacore$ds_spec,
        ds_vars = metacore$ds_vars,
        var_spec = metacore$var_spec,
        value_spec = metacore$value_spec,
        derivations = metacore$derivations,
        codelist = metacore$codelist,
        supp = metacore$supp
      )
      private$.name <- metacore$ds_spec$dataset[1]
      private$.label <- metacore$ds_spec$label[1]
      private$.num_vars <- metacore$ds_vars |> nrow()
      private$.key_vars <- metacore$ds_vars |>
        filter(!is.na(key_seq)) |>
        pull(variable)

      private$.greet(quiet, verbose)
    },
    print = function() {
      tables <- ls(envir = self)

      cli_par()
      cli_rule(left = "Dataset specification object for {private$.name} ({private$.label})")
      cli_text("The dataset contains {private$.num_vars} variable{?s}")
      cli_text("Dataset key{?s}: {ansi_collapse(private$.key_vars, last = ', ')}")
      cli_end()

      cli_par()
      cli_text("The structure of the specification object is:")
      for (table in tables) {
        obj <- get(table, envir = self)
        if (!is.list(obj)) {
          next
        }
        cli_bullets(c(">" = "{table}: {typeof(table)} [{dim(obj)[1]} x {dim(obj)[2]}] {ansi_collapse(names(obj), last = ', ')}"))
      }
      cli_end()

      cli_div()
      cli_text("To inspect the specification object use {.fn View} in the console.")
      cli_end()
    }
  )
)
