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

