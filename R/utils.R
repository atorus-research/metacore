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
#'
check_structure <- function(.data, col, func) {

   dat <- deparse(substitute(.data))
   column <- deparse(substitute(col))
   assertion_func <- rlang::enexpr(func)

   failures <- .data[[column]] %>%
      discard(~do.call(rlang::eval_tidy(func), list(.))) %>%
      unique()

   all_fails <- paste("   ", failures, collapse = "\n")

   if (length(failures) > 0) {


      warning_string <-
         case_when(
         as.character(assertion_func)[[1]] == "check_words" ~
            paste0("The following words in ", dat, "$", column, " are not allowed: \n", all_fails, "\n"),
      TRUE ~ paste0(dat, "$", column, " fails ", as.character(assertion_func)[[1]], " check\n")
      )

      warning(warning_string, call. = FALSE)
   }
}

#' Check Words in Column
#'
#' @param ... permissable words in the column
#' @param col the column to check for specific words
check_words <- function(..., col) {
   accepted_words <- unlist(c(...))
   expr(function(col) col %in% !!accepted_words)
}


get_base_obj <- function(.something=NULL, lhs=NULL){
   if (!is.null(lhs)) {
      target_stack <- stacks[[length(stacks)-1]]
   } else {
      target_stack <- stacks[[1]]
   }
   if (typeof(target_stack$lhs) != "symbol") {
      lhs=as.list(target_stack$lhs)[[2]]
      return(get_base_obj(lhs=lhs))
   } else {
      return(target_stack$lhs)
   }
}

