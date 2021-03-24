#' Specification document to metacore object
#'
#' This function takes the location of an excel specification document and reads
#' it in as a meta core object. At the moment it only supports specification in
#' the format of pinnacle 21 specifications. But, the @family spec builder can
#' be used as building blocks for bespoke specification documents
#' @param path string of file location
#'
#' @return
#' @export
spec_to_metacore <- function(path){
   doc <- read_all_sheets(path)
   if(spec_type(path) == "by_type"){
      browser()
      ds_spec <- spec_type_to_ds_spec(doc)
      ds_vars <- spec_type_to_ds_vars(doc)
      var_spec <- spec_type_to_var_spec(doc)
      value_spec <- spec_type_to_value_spec(doc)
      derivations <- spec_type_to_derivations(doc)
      code_list <- spec_type_to_code_list(doc)
      metacore(ds_spec, ds_vars, var_spec, value_spec,
               derivations, code_list)
   } else {
      stop("This specification format is not currently supported. You will need to write your own reader",
           call. = FALSE)
   }
}




#' Check the type of spec document
#'
#' @param path file location as a string
#'
#' @return returns string indicating the type of spec document
#'
#' @noRd
spec_type <- function(path){
   sheets <- excel_sheets(path)
   if(!any(sheets %>% str_detect("[D|d]omains?|[D|d]atasets?"))){
      stop("File does not contain a Domain/Datasets tab, which is needed. Please either modify the spec document or write a reader (see documentation for more information)",
           call. = FALSE)
   } else if(any(sheets %>% str_detect("ADSL|DM"))){
      type <- "by_ds"
   } else if(any(sheets %>% str_detect("[V|v]ariables?"))){
      type <- "by_type"
   } else {
      stop("File in an unknown format. Please either modify the spec document or write a reader (see documentation for more information)",
           call. = FALSE)
   }
   type
}




#' Read in all Sheets
#'
#' Given a path to a file, this function reads in all sheets of an excle file
#'
#' @param path string of the file path
#'
#' @return
read_all_sheets <- function(path){
   sheets <- excel_sheets(path)
   all_dat <- sheets %>%
      map(~read_excel(path, sheet = .))
   names(all_dat) <- sheets
   all_dat
}


#' Spec to ds_spec
#'
#' Creates the ds_spec from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_ds_spec <- function(doc, cols = c("dataset" = "[N|n]ame|[D|d]ataset|[D|d]omain",
                                               "structure" = "[S|s]tructure",
                                               "label" = "[L|l]abel|[D|d]escription"), sheet = NULL){

   name_check <- names(cols) %in% c("dataset", "structure", "label") %>%
      all()
   if(!name_check){
      stop("Supplied column vector must be named using the following names:
              'dataset', 'structure', 'label'")
   }
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   create_tbl(doc, cols) %>%
      distinct()
}

#' Spec to ds_vars
#'
#' Creates the ds_vars from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_ds_vars <- function(doc, cols = c("dataset" = "[D|d]ataset|[D|d]omain",
                                               "variable" = "[V|v]ariable [[N|n]ame]?|[V|v]ariables?",
                                               "key_seq" = "[V|v]ariable [O|o]rder|[S|s]eq|[O|o]rder",
                                               "keep" = "[K|k]eep|[M|m]andatory"),
                                 sheet = "[V|v]ar"){
   name_check <- names(cols) %in% c("variable", "dataset", "key_seq",
                                    "keep") %>%
      all()
   if(!name_check){
      stop("Supplied column vector must be named using the following names:
              'variable', 'dataset', 'key_seq', 'keep'")
   }

   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   doc %>%
      create_tbl(cols) %>%
      distinct()
}

#' Spec to var_spec
#'
#' Creates the var_spec from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_var_spec <- function(doc, cols = c("variable" = "[N|n]ame|[V|v]ariables?",
                                                "length" = "[L|l]ength",
                                                "label" = "[L|l]abel",
                                                "type" = "[T|t]ype",
                                                "dataset" = "[D|d]ataset|[D|d]omain"),
                                  sheet = NULL){
   # Check the names
   name_check <- names(cols) %in% c("variable", "length", "label",
                                    "type", "dataset", "common") %>%
      all()
   if(!name_check){
      stop("Supplied column vector must be named using the following names:
              'variable', 'length', 'label', 'type', 'dataset', 'common'
              If common is not avaliable it can be excluded and will be automatically filled in.")
   }

   # Check if sheet is specified
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   out <- create_tbl(doc, cols)
   if(!"dataset" %in% names(out)){
      dups <- out %>%
         distinct() %>%
         group_by(variable) %>%
         summarise(n = n(), .groups = "drop") %>%
         filter(n > 1)
      if(nrow(dups) > 0){
         dups %>%
            pull(variable) %>%
            paste(collapse = "\n") %>%
            paste0("The following variables are repeated with different metadata for different datasets:\n",
                   ., "\nPlease add 'dataset' = [Name of dataset column] to your named cols vector, to correct for this") %>%
            stop(., call. = FALSE)
      }
   } else {
      if(!"common" %in% names(cols)){
         # Get the variable common to all datasets can only be calculated with ds present
         common_vars <- out %>%
            group_by(dataset) %>%
            select(dataset, variable) %>%
            group_split(.keep = FALSE) %>%
            reduce(inner_join, by = "variable") %>%
            mutate(common = TRUE)
         out <- out %>%
            left_join(common_vars, by = "variable") %>%
            replace_na(list(common = FALSE))
      }

      # Remove any multiples and add ds if different metadata for different ds's
      out <- out %>%
         distinct(variable, length, label, type, .keep_all = TRUE) %>%
         group_by(variable) %>%
         mutate(n = n(),
                variable = if_else(n > 1, paste0(dataset, ".", variable),
                                   variable)) %>%
         select(-n, -dataset)
   }
   out
}

#' Spec to value_spec
#'
#' Creates the value_spec from a list of datasets (optionally filtered by the
#' sheet input). The named vector `cols` is used to determine which is the
#' correct sheet and renames the columns
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#' @param where_sep_sheet Boolean value to control if the where information in a
#'   separate dataset. If the where information is on a separate sheet, set to
#'   true and provide the column information with the `where_cols` inputs.
#' @param where_cols Named list with an id and where field. All coumns in the
#'   where field will be collapsed together
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_value_spec <- function(doc, cols = c("dataset" = "[D|d]ataset|[D|d]omain",
                                                  "variable" = "[N|n]ame|[V|v]ariables?",
                                                  "origin" = "[O|o]rigin",
                                                  "type" = "[T|t]ype",
                                                  "code_id" = "[C|c]odelist|Controlled Term",
                                                  "where" = "[W|w]here"),
                                    sheet = NULL,
                                    where_sep_sheet = TRUE,
                                    where_cols = c("id" = "ID",
                                                   "where" = c("Variable", "Comparator", "Value"))){
   name_check <- names(cols) %in% c("variable", "origin", "code_id",
                                    "type", "dataset", "where", "derivation_id") %>%
      all()
   if(!name_check){
      stop("Supplied column vector must be named using the following names:
              'dataset', 'variable', 'origin', 'code_id', 'type', 'where', 'derivation_id'
              If derivation_id is not avaliable it can be excluded and dataset.variable will be used.

              If the where information is on a seperate sheet, put the column with cross ref as where.")
   }
   # Select a subset of sheets if specified
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   out <- create_tbl(doc, cols)
   if(where_sep_sheet){
      where_df <- create_tbl(doc, where_cols) %>%
         rowwise() %>%
         mutate(where_new = paste(c_across(matches("where")), collapse = " ")) %>%
         select(id, where_new)
      out <- out %>%
         left_join(where_df, by = c("where" = "id")) %>%
         select(-where, where = where_new)
   }

   if(!"derivation_id" %in% names(cols)){
      out <- out %>%
         mutate(derivation_id = paste0(dataset, ".", variable))
   }
   out

}

#' Spec to code_list
#'
#' Creates the value_spec from a list of datasets (optionally filtered by the
#' sheet input). The named vector `*_cols` is used to determine which is the
#' correct sheet and renames the columns.
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param codelist_cols Named vector of column names that make up the codelist.
#'   The column names can be regular expressions for more flexibility. But, the
#'   names must follow the given pattern
#' @param permitted_val_cols Named vector of column names that make up the
#'   permitted value The column names can be regular expressions for more
#'   flexibility. This is optional, can be left as null if there isn't a
#'   permitted value sheet
#' @param dict_cols Named vector of column names that make up the dictionary
#'   value The column names can be regular expressions for more flexibility.
#'   This is optional, can be left as null if there isn't a permitted value
#'   sheet
#' @param sheets Optional, regular expressions of the sheets
#' @param simplify Boolean value, if true will convert code/decode pairs that
#'   are all equal to a permitted value list
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_code_list <- function(doc, codelist_cols = c("code_id" = "ID",
                                                          "name" = "[N|n]ame",
                                                          "code" = "^[C|c]ode|^[T|t]erm",
                                                          "decode" = "[D|d]ecode"),
                                   permitted_val_cols = NULL,
                                   dict_cols = c("code_id" = "ID",
                                                 "name" = "[N|n]ame",
                                                 "dictionary" = "[D|d]ictionary",
                                                 "version" = "[V|v]ersion"),
                                   sheets = NULL, simplify = TRUE){
   if(is.null(codelist_cols)){
      stop("Codelist column names must be provided", call. = FALSE)
   } else {
      name_check <- names(codelist_cols) %in% c("code_id", "name", "code", "decode") %>%
         all()
      if(!name_check){
         stop("Supplied column vector for codelist_cols must be named using the following names:
              'code_id', 'name', 'code', 'decode'",
              call. = FALSE
         )
      }
   }

   if (!is.null(permitted_val_cols)){
      name_check <- names(permitted_val_cols) %in% c("code_id", "name", "code") %>%
         all()
      if(!name_check){
         stop("Supplied column vector for permitted_val_cols must be named using the following names:
              'code_id', 'name', 'code'",
              call. = FALSE)
      }
   }
   if(!is.null(dict_cols)){
      name_check <- names(dict_cols) %in% c("code_id", "name", "dictionary", "version") %>%
         all()
      if(!name_check){
         stop("Supplied column vector for `dict_cols` must be named using the following names:
              'code_id', 'name', 'dictionary', 'version',
              If a dictionary sheet isn't avaliable set `dict_cols` to NULL",
              call. = FALSE)
      }
   }

   # Select a subset of sheets if specified
   if(!is.null(sheets)){
      sheet_ls <- str_subset(names(doc), sheets)
      doc <- doc[sheet_ls]
   }

   # Create the base table with codes and decodes (min req output)
   cd_out <- create_tbl(doc, codelist_cols) %>%
      group_by(code_id) %>%
      mutate(type = case_when(simplify & all(code == decode) ~ "permitted_val",
                              TRUE ~ "code_decode")) %>%
      nest(codes = c(code, decode)) %>%
      mutate(codes = if_else(type ==  "permitted_val",
                             lapply(codes, function(df) df %>% pull(code)),
                             codes))
   # If available get a permitted value sheet
   if(!is.null(permitted_val_cols)){
      pv_out <- create_tbl(doc, permitted_val_cols) %>%
         mutate(type = "permitted_val") %>%
         group_by(code_id) %>%
         nest(codes = c(code, decode))
      cd_out <- bind_rows(cd_out, pv_out)
   }
   # Add dictionary if avaliable
   if(!is.null(dict_cols)){
      dic_out <- create_tbl(doc, dict_cols) %>%
         mutate(type = "external_library") %>%
         group_by(code_id) %>%
         nest(codes = c(dictionary, version))
      cd_out <- bind_rows(cd_out, dic_out)
   }

   cd_out
}

#' Spec to derivation
#'
#' Creates the derivation table from a list of datasets (optionally filtered by
#' the sheet input). The named vector `cols` is used to determine which is the
#' correct sheet and renames the columns
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return
#' @export
#'
#' @family spec builder
spec_type_to_derivations <- function(doc, cols = c("derivation_id" = "ID",
                                                   "derivation" = "[D|d]efinition|[D|d]escription"),
                                     sheet = "Method|Derivations?"){
   name_check <- names(cols) %in% c("derivation_id", "derivation") %>%
      all()
   if(!name_check){
      stop("Supplied column vector must be names using the following names:
              'derivation_id', 'derivation'")
   }
   # Select a subset of sheets if specified
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   out <- create_tbl(doc, cols)
   out
}


#' Select sheet
#'
#' @param doc list of sheets from a excel dos
#' @param cols vector of regex to get a datasets base on which columns it has.
#'   If the vector is named it will also rename the columns
#'
#' @return dataset (or list of datasets if not specific enough)
#' @noRd
create_tbl <- function(doc, cols){
   matches <- doc %>%
      keep(function(x){
         cols %>%
            map_lgl(~any(str_detect(names(x), .))) %>%
            all()
      })
   if(length(matches) == 0) {
      # Get which variable can't be matches
      mismatch_per_sheet <- doc %>%
         map(function(x){
            cols %>%
               map_lgl(~any(str_detect(names(x), .))) %>%
               discard(~.) # Remove the matched values
         })
      # Find the closest sheet by looking for the sheet(s) with the fewest mismatches
      mis_lens <- mismatch_per_sheet %>%
         map_int(length)
      closest_sheets <- mis_lens %>%
         keep(~ . == min(mis_lens)) %>%
         names()
      # Get the name of the sheets and which columns don't match
      sheets_to_error <- mismatch_per_sheet %>%
         keep(names(.) %in% closest_sheets)

      # Write out the error
      sheets_to_error %>%
         map2_chr(names(sheets_to_error), function(vars, sheet_name){
            paste0("Sheet '", sheet_name, "' is the closest match, but unable to match the following column(s)\n",
                   paste(names(vars), collapse = "\n"))
         }) %>%
         paste0(collapse = "\n") %>%
         paste0("Unable to identify a sheet with all cloumns.\n", . ) %>%
         stop(call. = FALSE)

   } else if(length(matches) == 1){
      matches[[1]] %>%
         select(matches(cols))
   } else {
      sheets_mats <- matches %>%
         names()
      paste("Column names are not specific enough to identify a single sheet. The following",
            length(sheets_mats),
            "match the criteria set:", paste(sheets_mats, collapse = ", ")) %>%
         warning(., call. = FALSE)
      matches %>%
         map(~select(., matches(cols)))
   }
}
