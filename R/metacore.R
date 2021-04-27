#' This file includes the internal functions needed to create the readonly
#' Metacore R6 object
#'
#' @param ds_spec contians each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus
#'   dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs
#'   for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to
#'   have the same derivation
#' @param code_list contains the code/decode information
#'
#' @family Metacore
#' @noRd
#'
#'
MetaCore_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist){

   private$.ds_spec <- ds_spec %>%
      add_labels(dataset = "Dataset Name",
                 structure = "Value Structure",
                 label = "Dataset Label")

   private$.ds_vars <- ds_vars %>%
      add_labels(dataset = "Dataset Name",
                 variable = "Variable Name",
                 key_seq = "Sequence Key",
                 keep = "Keep (Boolean)",
                 core = "ADaM core (Expected, Required, Permissible)")

   private$.var_spec <- var_spec %>%
      add_labels(variable = "Variable Name",
                 length = "Variable Length",
                 label = "Variable Label",
                 type = "Variable Class",
                 common = "Common Across ADaM",
                 format = "Variable Format")

   private$.value_spec <- value_spec %>%
      add_labels(type = "Value Type",
                 orgin = "Origin of Value",
                 code_id = "ID of the Code List",
                 dataset = "Dataset Name",
                 variable = "Variable Name",
                 where = "Value of the Variable",
                 derivation_id = "ID of Derivation")

   private$.derivations <- derivations %>%
      add_labels(derivation_id = "ID of Derivation",
                 derivation = "Derivation")

   private$.codelist <- codelist %>%
      add_labels(code_id = "ID of the Code List",
                 names = "Name of the Code List",
                 type = "Code List/Permitted Values/External Library",
                 codes = "List of Codes")

   self$validate()
   message("\n Metadata successfully imported")
}


#' Metacore class print function
#'
#' @param ... pass in the dataframes to be validated
#' @family Metacore
#' @noRd
#'
MetaCore_print <- function(...){
   ds_len <- private$.ds_spec %>% pull(.data$dataset) %>% length()
   paste0("Metacore object contains metadata for ", ds_len, " datasets\n") %>%
      cat()
}


#' Metacore R6 object validation function
#'
#' This checks that the labels and lengths of ds_vars match var_spec
#' @family Metacore
#' @noRd
#'
MetaCore_validate <-  function() {
   if(var_name_check(private)){

      check_columns(private$.ds_spec,
                    private$.ds_vars,
                    private$.var_spec,
                    private$.value_spec,
                    private$.derivations,
                    private$.codelist
      )

      ds_vars_check(private$.ds_vars, private$.var_spec)
      value_check(private$.ds_vars, private$.value_spec)
      derivation_check(private$.value_spec, private$.derivations)
      codelist_check(private$.value_spec, private$.codelist)

   } else {
      warning("Other checks were not preformed, because column names were incorrect",
              call. = FALSE)
   }
}



#' readonly function factory
#'
#' This function is used inside the R6 active method and allows us
#' to read the selected dataframe and prevents overwriting
#'
#' @param name the name of the readonly object
#' @param value any attempt at assignment to the readonly object
#' @family Metacore
#' @noRd
#'
readonly <- function(name) {
   private <- NULL
   inside <- function(value) {
      name <- attr(sys.function(sys.parent()), "name")
      if (missing(value)) {
         private[[paste0(".", name)]]
      } else {
         stop(paste0(name, " is read only"), call. = FALSE)
      }
   }
   attributes(inside) <- list(name = name)
   inside
}

#' Select method to subset by a single dataframe
#' @param value the dataframe to subset by
#'
MetaCore_filter <- function(value) {
   # should we do a check of available filtering options?
   # like check DM, AE whatever?

   private$.ds_spec <- private$.ds_spec %>% filter(dataset == value)
   private$.ds_vars <- private$.ds_vars %>% filter(dataset == value)
   private$.value_spec <- private$.value_spec %>% filter(dataset == value)


   # Need clarity on X.Y.Z situation: SUPPY8.QVAL
   private$.var_spec <- private$.var_spec %>%
      # variables have the dataset prefix so we make this into its own column
      mutate(dataset = ifelse(str_detect(variable, "\\."), str_extract(variable, "^.*(?=\\.)"), ""),
             variable = str_remove(variable, "^.*\\.")
      ) %>%
      # then keep the variables that occur once or in the dataset to filter
      filter(dataset == "" | dataset == value) %>%
      # remove the temporary column
      select(-dataset) %>%
      # right join
      right_join(private$.ds_vars %>% select(variable), by="variable")

   private$.derivations <- private$.derivations %>%
      right_join(private$.value_spec %>% select(derivation_id) %>% na.omit(), by = "derivation_id")

   private$.codelist <- private$.codelist %>%
      right_join(private$.value_spec %>% select(code_id) %>% na.omit(), by = "code_id")
}

#' The Metacore R6 Class
#'
#' This uses the initialize, print, and validate functions above to create a single object
#' The user can query
#'
#' @family Metacore
#' @noRd
#
MetaCore <- R6::R6Class("Metacore",
                       public = list(
                          initialize = MetaCore_initialize,
                          print = MetaCore_print,
                          validate =  MetaCore_validate,
                          metacore_filter = MetaCore_filter
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


#' R6 Class wrapper to create your own metacore object
#'
#' @param ds_spec contains each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to have the same derivation
#' @param code_list contains the code/decode information
#'
#' @family Metacore
#'
#' @export
#'
metacore <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist) {
   MetaCore$new(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist)
}



#' Select metacore object to single dataset
#'
#' @param .data the metacore object of dataframes
#' @param dataset the specific dataset to subset by
#' @param simplify return a single dataframe
#'
#' @return
#' @export
#'
select_dataset <- function(.data, dataset, simplify = FALSE) {

   cl <- .data$clone()
   cl$metacore_filter(dataset)

   if (simplify) {

      suppressMessages(
         list(
            cl$ds_vars,
            cl$var_spec,
            cl$value_spec,
            cl$derivations,
            cl$codelist
         ) %>%
            reduce(left_join)
      )

   } else {
      return(cl)
   }
}


#' save metacore object
#'
#' @param metacore_object the metacore object in memory to save to disc
#' @param path file path and file name to save metacore object
#'
#' @return an .rda file
#' @export
#'
save_metacore <- function(metacore_object, path = NULL) {
   # if no path save to working directory
   # with same name as object
   if (is.null(path)) {
      nm <- deparse(substitute(metacore_object))
      path <- paste0(nm, ".rda")
   # check the suffix of the path
   } else {
      suffix <- str_extract(path, "\\.\\w*$")
      # if the extention is .rda keep it
      if (suffix == ".rda") {
         path <- path
      # otherwise we need to replace it with .rda
      } else {
         prefix <- str_remove(path, "\\.\\w*$")
         path <- paste0(prefix, ".rda")
      }
   }
   saveRDS(metacore_object, path)
}

#' load metacore object
#'
#' @param path location of the metacore object to load into memory
#'
#' @return
#' @export
load_metacore <- function(path = NULL) {
   if (is.null(path)) {
      rdas <- list.files(".", ".rda")
      if (length(rdas) == 0) {
         stop("please supply path to metacore object ending with extension .rda", call. = FALSE)
      } else {
         stop("metacore object path required, did you mean:",
              paste("   ", rdas, sep = "\n   "), call. = FALSE)
      }
   }
   readRDS(file = path)
}
