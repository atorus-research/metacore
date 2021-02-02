library(R6)
library(dplyr)
library(stringr)

source("R/builders.R")
path <- "C:/Users/cf124952/ATorus/GSK Atorus Open Source Collaboration - Metadata/GSK_SDTM_defines/mid201584/define.xml"

# Read in the file
doc <- xmlTreeParse(path, useInternalNodes = TRUE)


ds_spec <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)
var_spec <- xml_to_var_spec(doc)
value_spec <- xml_to_value_spec(doc)
code_list <- xml_to_code_list(doc)


test <- var_spec %>%
   filter(str_detect(variable, "\\."))
test1 <- ds_vars %>%
   filter(dataset == "DM")
# Proof this catches things is here: IETESTCD


test <- DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list)
test
object.size(test)

DataDef <- R6Class("DataDef",
                   public = list(
                      initialize = function(ds_spec, ds_vars, var_spec,
                                            value_spec, derivations, code_list){
                         private$ds_spec <- ds_spec
                         private$ds_vars <- ds_vars
                         private$var_spec <- var_spec
                         private$value_spec <- value_spec
                         private$derivations <- derivations
                         private$lib_spec <- ls(code_list)

                         self$validate()
                         # TO DO: Cross-ref functions:
                           # * derivations, codelist, variables x2
                         # type coulnm has a limited number of types
                      },

                      print = function(...){
                         cat(private$ds_spec %>% as.character() %>% paste0(collapse = "\n"))
                         # Number of datasets
                         #
                      },

                      validate = function() {
                         # Check variables in ds_vars tables exists in var_spec
                         var_check <- anti_join(private$ds_vars, private$var_spec, by = "variable")

                         if(var_check %>% nrow() != 0){
                            var_ls <- var_check %>%
                               pull(variable) %>% unique()

                            warning(
                               "The following variable(s) do not have labels and lengths: ",
                               paste("   ", var_ls, sep = "\n   "),
                               call. = FALSE
                           )
                         }
                      }
                   ),
                   private = list(
                      ds_spec = tibble(dataset = character(), label = character()),
                      ds_vars = tibble(dataset = character(), variable = character(), keep = logical(),
                                       key = integer(), codelist = character(), origin = character(),
                                       derivation_id = character()),
                      var_spec = tibble(variable = character(), label = character(), length = integer()),
                      value_spec = tibble(dataset = character(),
                                          variable = character(),
                                          where  = character(),
                                          type = character(),
                                          codelist = character(),
                                          origin = character(),
                                          derivation_id = integer()),
                      derivations = tibble(derivation_id = integer(), derivation = character()),
                      lib_spec = tibble(lib_id = character(), lib = list()), # [code = ?(), decode = character()]
                           # 1 entry per lib_id
                           # lib rather than codelist so each entry can contain 1 of three types of information
                              # codelist: df (code, decode), by using nested lists it means the codes can be int or char
                              # permitted_val : vec of permitted values
                              #  external lib id
                      change_log = tibble(table_chg = character(), column_chg = character(), what_chg = list())
                   )
)

test <- DataDef$new(ds_spec, ds_vars, var_spec, value_spec, derivations = NULL, code_list)
test

# Notes from creation, derivations are sometimes duplicated, should the builder reduce the duplicates


# Potential contravertial things I have done:
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
