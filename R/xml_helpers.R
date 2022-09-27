
#' id to dataset
#'
#' @param id vector of id's
#'
#' @return vector of datasets
#' @noRd
id_to_ds <- function(id) {
   id %>% str_extract("(?<=^IT\\.)[:alnum:]+(?=\\..*)")
}

#' id to variable
#'
#' @param id vector of id's
#' @param ds vector of ds's
#'
#' @return vector of variable names
#' @noRd
id_to_var <- function(id) {
   ds <- id %>% str_extract("(?<=^IT\\.)[:alnum:]+(?=\\..*)")
   extract <- if_else(is.na(ds), "(?<=^IT\\.)[:alnum:]*",
                      str_c("(?<=^IT\\.", ds, "\\.)[:alnum:]*")
   )
   id %>%
      str_extract(extract)
}



