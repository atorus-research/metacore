#' Add a label to a vector
#'
#' @param x Vector to add label to
#' @param label Label to add to vector
#' @return Labeled vector
#' @noRd

add_lab <- function(x, label) {
   if (length(label) == 0) {
      label  <- NULL
   }
   attr(x, "label") <- label
   x
}

#' Add Labels to Dataframe
#' @param .data Dataframe that you want to add labels to
#' @param ... Labeled vector of labels, with the name of the element equal to
#' the name of the column and the element equaling the label, or comma-separated name-value pair
#' @importFrom purrr map2
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter pull
#' @return Dataframe with labels
#' @noRd
add_labs <- function(.data,...) {
   name_list <- c(...)
   df <- tibble(col = names(name_list), lab = name_list)
   .data %>%
      purrr::map2(names(.data), function(x, name) {
         label <- df %>%
            filter(col == name) %>%
            pull(lab) %>%
            unname()
         if(length(label) > 0) {
            add_lab(x, label)
         } else {
            x
         }
      }) %>%
      as_tibble()
}


#' Column Validation Function
#'
#' @param .data the dataframe to check the column for
#' @param col the column to test
#' @param func the function to use to assert column structure
#' @param any_na_acceptable boolean, testing if the column can have missing
#' @param nm name of column to check (for warning and error clarification)
#'
check_structure <- function(.data, col, func, any_na_acceptable, nm) {

   column <- as_string(col)

   vec <- .data %>% pull(!!col)

   if(any(is.na(vec)) & !any_na_acceptable) {
      error_message <- paste(column, "from the", nm, "table contains missing values. Actual values are needed.")
      warning_string <- NULL
   } else if (all(is.na(vec))){
      warning_string <- paste(column, "from the", nm,
                    "table only contain missing values.")
      error_message <- NULL
   } else {

      failures <-  vec %>%
         discard(~do.call(func, list(.))) %>%
         unique()

      all_fails <- paste("   ", failures, collapse = "\n")
      error_message <- NULL

      if (length(failures) > 0 ) {

         if (is.primitive(func)) {

            assertion_func <- prim_name(func)
            warning_string <- paste0(nm, "$", column, " fails ", assertion_func, " check \n")

         } else {
            warning_string <- paste0("The following words in ", nm, "$", column, " are not allowed: \n", all_fails, "\n")
         }

      } else {
         warning_string <- NULL
      }

   }

   list(warning = warning_string, error = error_message)
}

#' Check Words in Column
#'
#' @param ... permissible words in the column
#' @param col the column to check for specific words
check_words <- function(..., col) {
   accepted_words <- unlist(c(...))
   expr <- expr(function(col) col %in% !!accepted_words)
   make_function(body = expr, env = parent.frame())()
}

make_function <- function(args = pairlist(), body, env = parent.frame())  {
   eval(call("function", args, body), env)
}


#' Get path to metacore example
#'
#' metacore comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access. When testing or writing
#' examples in other packages, it is best to use the 'pilot_ADaM.rda' example as
#' it loads fastest.
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' metacore_example()
#' metacore_example("mock_spec.xlsx")
metacore_example <- function(file = NULL) {
   if (is.null(file)) {
      dir(system.file("extdata", package = "metacore"))
   } else {
      system.file("extdata", file, package = "metacore", mustWork = TRUE)
   }
}
