#' Optional checks to consistency of metadata
#'
#' @description These functions check to see if values (e.g labels, formats)
#'   that should be consistent for a variable across all data are actually
#'   consistent.
#'
#' @param metacore metacore object to check
#'
#' @return If all variables are consistent it will return a message. If there
#'   are inconsistencies it will return a message and a dataset of the variables
#'   with inconsistencies.
#'
#' @export
#'
#' @examples
#' ## EXAMPLE WITH DUPLICATES
#' # Loads in a metacore obj called metacore
#' load(metacore_example("pilot_ADaM.rda"))
#' check_inconsistent_labels(metacore)
#'
#' check_inconsistent_types(metacore)
#'
#' ## EXAMPLE WITHOUT DUPLICATES
#' # Loads in a metacore obj called metacore
#' load(metacore_example("pilot_SDTM.rda"))
#' check_inconsistent_labels(metacore)
#'
#' check_inconsistent_formats(metacore)
#'
#' check_inconsistent_types(metacore)
#' @rdname checks
check_inconsistent_labels <- function(metacore){
   basic_check(label, metacore)
}

#' @export
#' @rdname checks
check_inconsistent_types <- function(metacore){
   basic_check(type, metacore)
}

#' @export
#' @rdname checks
check_inconsistent_formats <- function(metacore){
   basic_check(format, metacore)
}

#' Base code for running cross variable checks
#'
#' @param col_to_check column to check
#' @param metacore metacore obj
#'
#' @return A message if all is well or dataset with all the variables with
#'   duplicated variables.
#' @noRd
#' @importFrom stringr str_glue
#' @importFrom dplyr across
basic_check <- function(col_to_check, metacore){
   if(!is_metacore(metacore)){
      stop("Expects a metacore object", call. = FALSE)
   }

   report_df <- metacore$var_spec %>%
      mutate(var1 = str_remove(variable, "[[:alnum:]]+\\.")) %>%
      group_by(var1) %>%
      mutate(n_lab = n_distinct({{col_to_check}})) %>%
      filter(n_lab > 1) %>%
      mutate(across(everything(), remove_label)) %>%
      group_by(var1, {{col_to_check}}) %>%
      summarise(n_vars = n(),
                ls_of_vars = list(variable),
                .groups = "drop") %>%
      select(variable = var1, everything())

   if(nrow(report_df) > 0){
      message(str_glue("Mismatch {as_label(enexpr(col_to_check))}s detected"))
      return(report_df)
   } else {
      message(str_glue("No mismatch {as_label(enexpr(col_to_check))}s detected"))
   }
}

remove_label <- function(x) {
   attr(x, "label") <- NULL
   x
}
