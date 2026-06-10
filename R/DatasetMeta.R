#' Shared print method for DatasetMeta variants
#' @noRd
DatasetMeta_print <- function(...) {
  tables <- ls(envir = self)
  cli_par()
  cli_rule(left = "Dataset specification object for {private$.name} ({private$.label})")
  cli_text("The dataset contains {private$.num_vars} variable{?s}")
  cli_text("Dataset key{?s}: {private$.key_vars}")
  cli_end()
  cli_par()
  cli_text("The structure of the specification object is:")
  for (table in tables) {
    obj <- get(table, envir = self)
    if (!is.list(obj)) next
    cli_bullets(c(">" = "{table}: {typeof(table)} [{dim(obj)[1]} x {dim(obj)[2]}] {names(obj)}"))
  }
  cli_end()
  cli_div()
  cli_text("To inspect the specification object use {.fn View} in the console.")
  cli_end()
}

#' Shared private fields and greet for DatasetMeta variants
#' @noRd
DatasetMeta_private <- list(
  .name = NA,
  .label = NA,
  .num_vars = NA,
  .key_vars = NA,
  .greet = function(quiet, verbose) {
    with_verbosity(
      cli_alert_success("{private$.name} dataset successfully selected"),
      quiet,
      verbose
    )
  }
)

#' Shared post-initialize bookkeeping for DatasetMeta variants
#' @noRd
DatasetMeta_post_init <- function(metacore, quiet, verbose, private) {
  private$.name <- metacore$ds_spec$dataset[[1]]
  private$.label <- metacore$ds_spec$label[[1]]
  private$.num_vars <- nrow(metacore$ds_vars)
  private$.key_vars <- metacore$ds_vars |>
    filter(!is.na(key_seq)) |>
    pull(variable)
  private$.greet(quiet, verbose)
}


#' The DatasetMeta R6 Class (base schema)
#'
#' Subsets a \code{Metacore} object to a single dataset.
#' Class vector: \code{c("DatasetMeta", "Metacore", "R6")}.
#'
#' @family Metacore
#' @noRd
DatasetMeta <- R6::R6Class("DatasetMeta",
  inherit = MetaCore,
  private = DatasetMeta_private,
  public = list(
    initialize = function(metacore, quiet = deprecated(), verbose = "message") {
      super$initialize(
        ds_spec     = metacore$ds_spec,
        ds_vars     = metacore$ds_vars,
        var_spec    = metacore$var_spec,
        value_spec  = metacore$value_spec,
        derivations = metacore$derivations,
        codelist    = metacore$codelist,
        supp        = metacore$supp
      )
      DatasetMeta_post_init(metacore, quiet, verbose, private)
    },
    print = DatasetMeta_print
  )
)


#' The DatasetMetaDefine R6 Class (Define.xml schema)
#'
#' Subsets a \code{MetacoreDefine} object to a single dataset, preserving
#' \code{study_level}, \code{documents}, and \code{comments}.
#' Class vector: \code{c("DatasetMeta", "MetacoreDefine", "Metacore", "R6")}.
#' The R-level name is \code{DatasetMetaDefine} but the R6 classname string is
#' \code{"DatasetMeta"}, so \code{inherits(x, "DatasetMeta")} returns \code{TRUE}
#' for objects of both variants.
#'
#' @family Metacore
#' @noRd
DatasetMetaDefine <- R6::R6Class("DatasetMeta",
  inherit = MetaCoreDefine,
  private = DatasetMeta_private,
  public = list(
    initialize = function(metacore, quiet = deprecated(), verbose = "message") {
      super$initialize(
        ds_spec     = metacore$ds_spec,
        ds_vars     = metacore$ds_vars,
        var_spec    = metacore$var_spec,
        value_spec  = metacore$value_spec,
        derivations = metacore$derivations,
        codelist    = metacore$codelist,
        supp        = metacore$supp,
        study_level = metacore$study_level,
        documents   = metacore$documents,
        comments    = metacore$comments,
        quiet       = quiet,
        verbose     = verbose
      )
      DatasetMeta_post_init(metacore, quiet, verbose, private)
    },
    print = DatasetMeta_print
  )
)
