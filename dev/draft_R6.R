library(R6)
library(dplyr)
library(stringr)


source("R/builders.R")
path <- "C:/Users/cf124952/ATorus/GSK Atorus Open Source Collaboration - Metadata/GSK_SDTM_defines/mid207966/define.xml"


# Read in the file
doc <- xmlTreeParse(path, useInternalNodes = TRUE)


ds_spec <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)
var_spec <- xml_to_var_spec(doc)
value_spec <- xml_to_value_spec(doc)
code_list <- xml_to_code_list(doc)



datadef_initialize <- function(ds_spec, ds_vars, var_spec,
         value_spec, derivations, code_list){

   private$.ds_spec <- ds_spec
   private$.ds_vars <- ds_vars
   private$.var_spec <- var_spec
   private$.value_spec <- value_spec
   private$.derivations <- derivations
   private$.codelist <- code_list

   self$validate()
   message("Metadata successfully imported")
   # TO DO: Cross-ref functions:
   # * derivations, codelist, variables x2
   # type coulnm has a limited number of types
}

datadef_print <- function(...){
   # the domain name and how many data set specs
   cat(private$.ds_spec %>% as.character() %>% paste0(collapse = "\n"))
}

datadef_validate <-  function() {

   var_check <- anti_join(private$.ds_vars, private$.var_spec, by = "variable")

   if(var_check %>% nrow() != 0){
      var_ls <- var_check %>%
         pull(variable) %>%
         unique()

      warning(
         "The following variable(s) do not have labels and lengths: ",
         paste("   ", var_ls, sep = "\n   "),
         call. = FALSE
      )
   }
}

readonly <- function(name) {
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


DataDef <- R6Class("DataDef",
                   public = list(
                      initialize = datadef_initialize,
                      print = datadef_print,
                      validate =  datadef_validate
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


# Proof this catches things is here: IETESTCD

test <- DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list)
test
object.size(test)

# Notes from creation, derivations are sometimes duplicated, should the builder reduce the duplicates


# Potential controversial things I have done:
#   * Collapse format information to live in codelist table
#   * Move origin, codelist and derivation id to value_spec from var_spec
#   * argument to do autoclean on the where statement. Would still be a string but really for bang bang
#       Backlog


# List of tables needed
# 1) ds_spec (dataset, label):
#  This contians each dataset in the study, with the labels for each
# 2) ds_vars (dataset, variable, keep, key):
#  This has information on what variables are in each dataset + plus dataset specific variable information
# 3) var_spec (variable, label, length, ?format [might be the same as code list]):
#  This has variable information that is shared across all datasets
# 4) value_spec (dataset, variaable, where, type, codelist, origin, derivation_id):
#  This has parametere specific infromation, as data is long the specs for wbc might be difference the hgb
# 5) derivations (derivation_id, derivation):
#  This contains derivation, it allows for different variables to have the same derivation. (derivation just string)
# 6) codelist (codelist, code, decode):
#  This contains the code/decode information, [with the potential to have format as a decode]
# 7) change log



# Question:
# will there ever be a codelist with a permitted_val, No
# should the change log include the row that was changed
# lib_spec too complicated should it be split into 3
# Should lib_spec only have two?
#        Permitted_val would be for things without decode
#        pro: any missing values in decode will be intentional and something we could give warning for
#        Also with normalized data it will reduce data size in the long run
#        Con: Could result in more complex data entry, We will need a rule on knowing when to go
# Do we want sig dig? No
#   * argument to do autoclean on the where statement.
#     Would still be a string but really for bang bang

