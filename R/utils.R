#' Add a label to a vector
#'
#' @param x Vector to add label to
#' @param label Label to add to vector
#' @return Labeled vector

add_label <- function(x, label) {
   if (length(label) == 0) {
      label  <- NULL
   }
   attr(x, "label") <- label
   x
}

#' Add Labels to Dataframe
#' @param .data Dataframe that you want to add labels to
#' @param ... Labeled vector of labels, with the name of the element equal to
#' the name of the column and the element equaling the label, or comma-seperated name-value pair
#' @importFrom purrr map2
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter pull
#' @return Dataframe with labels

add_labels <- function(.data,...) {
   name_list <- c(...)
   df <- tibble(col = names(name_list), lab = name_list)
   .data %>%
      purrr::map2(names(.data), function(x, name) {
         label <- df %>%
            filter(col == name) %>%
            pull(lab) %>%
            unname()
         if(length(label) > 0) {
            add_label(x, label)
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
#'
check_structure <- function(.data, col, func, any_na_acceptable, env) {

   dat <- rlang::as_string(.data)

   column <- rlang::as_string(col)
   vec <- rlang::eval_tidy(.data, env = env) %>%
      pull(!!col)
   if(any(is.na(vec)) & !any_na_acceptable){
      error_message <- (message = paste(column, "from the", dat,
                 "table contains missing values. Actual values are needed."))
      warning_string <- NULL
   } else if (all(is.na(vec))){
      warning_string <- paste(column, "from the", dat,
                    "table only contain missing values.")
      error_message <- NULL
   } else {
      failures <-  vec %>%
         discard(~do.call(func, list(.))) %>%
         unique()

      all_fails <- paste("   ", failures, collapse = "\n")
      error_message <- NULL
      if (length(failures) > 0) {

         if (is.primitive(func)) {
            assertion_func <- rlang::prim_name(func)
            # call the function so we can grab its name for the error
            # force(func)
            # assertion_func <- deparse(rlang::enexpr(func))
            # assertion_func <- sub('.*\\"(.*)\\").*', "\\1", assertion_func)
            warning_string <- paste0(dat, "$", column, " fails ", assertion_func, " check \n")

         } else {
            warning_string <- paste0("The following words in ", dat, "$", column, " are not allowed: \n", all_fails, "\n")
         }

      } else {
         warning_string <- NULL
      }

   }

   list(warning_string, error_message)
}

#' Check Words in Column
#'
#' @param ... permissable words in the column
#' @param col the column to check for specific words
check_words <- function(..., col) {
   accepted_words <- unlist(c(...))
   expr <- expr(function(col) col %in% !!accepted_words)
   make_function(body = expr, env = parent.frame())()
}

make_function <- function(args = pairlist(), body, env = parent.frame())  {
   eval(call("function", args, body), env)
}
