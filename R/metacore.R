#' This file includes the internal functions needed to create the readonly
#' Metacore R6 object
#'
#' @param ds_spec contains each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus
#'   dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs
#'   for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to
#'   have the same derivation
#' @param code_list contains the code/decode information
#' @param supp contains the idvar and qeval information for supplemental variables
#'
#' @family Metacore
#' @noRd
#'
#' @importFrom stringr str_to_lower
MetaCore_initialize <- function(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp){

   private$.ds_spec <- ds_spec %>%
      add_labs(dataset = "Dataset Name",
                 structure = "Value Structure",
                 label = "Dataset Label")

   private$.ds_vars <- ds_vars %>%
      add_labs(dataset = "Dataset Name",
                 variable = "Variable Name",
                 key_seq = "Sequence Key",
                 order = "Variable Order",
                 keep = "Keep (Boolean)",
                 core = "ADaM core (Expected, Required, Permissible)",
                 supp_flag = "Supplemental Flag")

   private$.var_spec <- var_spec %>%
      add_labs(variable = "Variable Name",
                 length = "Variable Length",
                 label = "Variable Label",
                 type = "Variable Class",
                 common = "Common Across ADaM",
                 format = "Variable Format")

   private$.value_spec <- value_spec %>%
      add_labs(type = "Value Type",
                 orgin = "Origin of Value",
                 code_id = "ID of the Code List",
                 dataset = "Dataset Name",
                 variable = "Variable Name",
                 where = "Value of the Variable",
                 derivation_id = "ID of Derivation") %>%
      mutate(origin = str_to_lower(.data$origin))


   private$.derivations <- derivations %>%
      add_labs(derivation_id = "ID of Derivation",
                 derivation = "Derivation")

   private$.codelist <- codelist %>%
      add_labs(code_id = "ID of the Code List",
                 names = "Name of the Code List",
                 type = "Code List/Permitted Values/External Library",
                 codes = "List of Codes")

   private$.codelist <- codelist %>%
      add_labs(code_id = "ID of the Code List",
               names = "Name of the Code List",
               type = "Code List/Permitted Values/External Library",
               codes = "List of Codes")

   private$.supp <- supp %>%
      add_labs(dataset = "Dataset Name",
               variable = "Variable Name",
               idvar = "Identifying Variable",
               qeval = "Evaluator")

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

      if(nrow(private$.ds_spec) == 0 &
      nrow(private$.ds_vars) == 0 &
      nrow(private$.var_spec) == 0 &
      nrow(private$.value_spec) == 0 &
      nrow(private$.derivations) == 0 &
      nrow(private$.codelist) == 0 &
      nrow(private$.supp) == 0 ){
         warning("Other checks were not preformed, because all datasets are empty",
                 call. = FALSE)
      } else {
         check_columns(private$.ds_spec,
                       private$.ds_vars,
                       private$.var_spec,
                       private$.value_spec,
                       private$.derivations,
                       private$.codelist,
                       private$.supp
         )

         ds_vars_check(private$.ds_vars, private$.var_spec)
         value_check(private$.ds_vars, private$.value_spec)
         derivation_check(private$.value_spec, private$.derivations)
         codelist_check(private$.value_spec, private$.codelist)
         if(nrow(private$.supp) == 0){
            supp_check(private$.ds_vars, private$.supp)
         }

      }

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

   private$.ds_spec <- private$.ds_spec %>% filter(dataset == value)
   if(nrow(private$.ds_spec) == 0){
      stop(paste0(value, " is not a dataset in the metacore object", call. = FALSE))
   }
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
      right_join(private$.ds_vars %>% select(variable), by="variable") %>%
      distinct(variable, .keep_all = TRUE) # for when duplicates gett through and have different lables but the same name

   private$.derivations <- private$.derivations %>%
      right_join(private$.value_spec %>% select(derivation_id) %>% na.omit(), by = "derivation_id")

   private$.codelist <- private$.codelist %>%
      right_join(private$.value_spec %>% select(code_id) %>% na.omit(), by = "code_id")

   private$.supp <- private$.supp %>% filter(dataset == value)
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
                          .ds_spec = tibble(dataset = character(), structure = character(), label = character()),
                          .ds_vars = tibble(dataset = character(), variable = character(), keep = logical(),
                                            key_seq = integer(), order = integer(), core = character(),
                                            supp_flag = logical()),
                          .var_spec = tibble(variable = character(), label = character(), length = integer(),
                                             type = character(), common = character(), format = character()),
                          .value_spec = tibble(dataset = character(),
                                               variable = character(),
                                               where  = character(),
                                               type = character(),
                                               sig_dig = integer(),
                                               code_id = character(),
                                               origin = character(),
                                               derivation_id = integer()),
                          .derivations = tibble(derivation_id = integer(), derivation = character()),
                          # code_type == df | permitted_val | external_lib
                          .codelist = tibble(code_id = character(), name = character(), type = character(), codes = list()),
                          .supp = tibble(dataset = character(), variable = character(), idvar = character(), qeval = character())
                       ),
                       active = list(
                          ds_spec = readonly('ds_spec'),
                          ds_vars =  readonly('ds_vars'),
                          var_spec = readonly('var_spec'),
                          value_spec = readonly('value_spec'),
                          derivations = readonly('derivations'),
                          codelist = readonly('codelist'),
                          supp = readonly('supp')
                       )
)


#' R6 Class wrapper to create your own metacore object
#'
#' @param ds_spec contains each dataset in the study, with the labels for each
#' @param ds_vars information on what variables are in each dataset + plus dataset specific variable information
#' @param var_spec variable information that is shared across all datasets
#' @param value_spec parameter specific information, as data is long the specs for wbc might be difference the hgb
#' @param derivations contains derivation, it allows for different variables to have the same derivation
#' @param codelist contains the code/decode information
#' @param supp contains the idvar and qeval information for supplemental variables
#'
#' @family Metacore
#'
#' @export
#'
metacore <- function(ds_spec = tibble(dataset = character(), structure = character(), label = character()),
                     ds_vars = tibble(dataset = character(), variable = character(), keep = logical(),
                                       key_seq = integer(), order = integer(), core = character(),
                                       supp_flag = logical()),
                     var_spec = tibble(variable = character(), label = character(), length = integer(),
                                       type = character(), common = character(), format = character()),
                     value_spec = tibble(dataset = character(),
                                         variable = character(),
                                         where  = character(),
                                         type = character(),
                                         sig_dig = integer(),
                                         code_id = character(),
                                         origin = character(),
                                         derivation_id = integer()),
                     derivations = tibble(derivation_id = integer(), derivation = character()),
                     codelist = tibble(code_id = character(), name = character(), type = character(), codes = list()),
                     supp = tibble(dataset = character(), variable = character(), idvar = character(), qeval = character())) {
   # Check if there are any empty datasets that need adding
   is_empty_df <- as.list(environment()) %>%
      keep(is.null)
   if(length(is_empty_df) > 0) {
      # Adding empty datasets
      to_replace <- all_message() %>%
         #get the type each variable needs to be
         mutate(convert =
                   map(.data$test, function(x){
                      if(identical(x, .Primitive("is.numeric"))){
                         numeric()
                      } else if(identical(x, .Primitive("is.logical"))){
                         logical()
                      } else {
                         character()
                      }
                   })) %>%
         filter(dataset %in% names(is_empty_df)) %>%
         group_by(dataset) %>%
         group_split()
      replaced <- to_replace %>%
         map(function(df){
            names(df$convert) <- df$var
            df$convert %>%
               as_tibble()
         })
      names(replaced) <- to_replace %>% map_chr(~unique(.$dataset))
      list2env(replaced, environment())
      }
   MetaCore$new(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp)
}



#' Select metacore object to single dataset
#'
#' @param .data the metacore object of dataframes
#' @param dataset the specific dataset to subset by
#' @param simplify return a single dataframe
#'
#' @return a filtered subset of the metacore object
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
            cl$codelist,
            cl$supp
         ) %>%
            reduce(left_join)
      )

   } else {
      return(cl)
   }
}



#' Get Control Term
#'
#' Returns the control term (a vector for permitted values and a tibble for code
#' lists) for a given variable. The dataset can be optionally specified if there
#' is different control terminology for different datasets
#'
#' @param metacode metacore object
#' @param variable A variable name to get the controlled terms for. This can
#'   either be a string or just the name of the variable
#' @param dataset A dataset name. This is not required if there is only one set
#'   of control terminology across all datasets
#'
#' @return a vector for permitted values and a 2-column tibble for codelists
#' @export
#'
#' @importFrom rlang as_label enexpr as_name
#'
#' @examples
#' meta_ex <- spec_to_metacore(metacore_example("p21_mock.xlsx"))
#' get_control_term(meta_ex, QVAL, SUPPAE)
#' get_control_term(meta_ex, "QVAL", "SUPPAE")
get_control_term <- function(metacode, variable, dataset = NULL){
   var_str <- ifelse(str_detect(as_label(enexpr(variable)), "\""),
                     as_name(variable), as_label(enexpr(variable)))
   dataset_val <- ifelse(str_detect(as_label(enexpr(dataset)), "\""),
                         as_name(dataset), as_label(enexpr(dataset))) # to make the filter more explicit
   if(!var_str %in% metacode$value_spec$variable){
      stop(paste0(var_str, " not found in the value_spec table. Please check the variable name"))
   }
   if(dataset_val == "NULL"){
      var_code_id <- metacode$value_spec %>%
         filter(variable == var_str) %>%
         pull(code_id) %>%
         unique()
   } else {
      subset_data <- metacode$value_spec %>%
         filter(dataset == dataset_val)
      if(nrow(subset_data) == 0){
         stop(paste0(dataset_val, " not found in the value_spec table. Please check the dataset name"))
      }
      var_code_id <- subset_data %>%
         filter(variable == var_str) %>%
         pull(code_id) %>%
         unique()
   }
   if(length(var_code_id) > 1){
      stop(paste0(var_str, " does not have a unique control term, consider spcificing a dataset"))
   }
   ct <- metacode$codelist %>%
      filter(code_id == var_code_id) %>%
      pull(codes)
   if(length(ct) == 0){
      message(paste0(var_str, " has no control terminology"))
   } else {
      return(ct[[1]])
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
      path <- paste0(nm, ".rds")

   # check the suffix of the path
   } else {
      suffix <- str_extract(path, "\\.\\w*$")
      # if the extension is .rda keep it
      if (suffix == ".rds") {
         path <- path

      # otherwise we need to replace it with .rda
      } else {
         prefix <- str_remove(path, "\\.\\w*$")
         path <- paste0(prefix, ".rds")
      }
   }
   saveRDS(metacore_object, path)
}

#' load metacore object
#'
#' @param path location of the metacore object to load into memory
#'
#' @return metacore object in memory
#' @export
load_metacore <- function(path = NULL) {
   if (is.null(path)) {
      rdss <- list.files(".", ".rds")
      if (length(rdss) == 0) {
         stop("please supply path to metacore object ending with extension .rds", call. = FALSE)
      } else {
         stop("metacore object path required, did you mean:",
              paste("   ", rdss, sep = "\n   "), call. = FALSE)
      }
   }
   readRDS(path)
}
