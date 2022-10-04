#' Specification document to metacore object
#'
#' This function takes the location of an excel specification document and reads
#' it in as a meta core object. At the moment it only supports specification in
#' the format of pinnacle 21 specifications. But, the @family spec builder can
#' be used as building blocks for bespoke specification documents
#'
#' @param path string of file location
#' @param quiet Option to quietly load in, this will suppress warnings, but not
#'   errors
#' @param where_sep_sheet Option to tell if the where is in a separate sheet,
#'   like in older p21 specs or in a single sheet like newer p21 specs
#'
#' @return given a spec document it returns a metacore object
#' @export
spec_to_metacore <- function(path, quiet = FALSE, where_sep_sheet = TRUE){
   doc <- read_all_sheets(path)

   if(spec_type(path) == "by_type"){
      ds_spec <- spec_type_to_ds_spec(doc)
      ds_vars <- spec_type_to_ds_vars(doc)
      var_spec <- spec_type_to_var_spec(doc)
      value_spec <- spec_type_to_value_spec(doc, where_sep_sheet = where_sep_sheet)
      derivations <- spec_type_to_derivations(doc)
      code_list <- spec_type_to_codelist(doc)
      if(!quiet){
         out <- metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist = code_list)
      } else{
         out<- suppressWarnings(metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist = code_list))
         message("Loading in metacore object with suppressed warnings")
      }
   } else {
      stop("This specification format is not currently supported. You will need to write your own reader",
           call. = FALSE)
   }
   out
}




#' Check the type of spec document
#'
#' @param path file location as a string
#'
#' @return returns string indicating the type of spec document
#' @export
#'
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
#' Given a path to a file, this function reads in all sheets of an excel file
#'
#' @param path string of the file path
#' @export
#'
#' @return a list of datasets
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
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
spec_type_to_ds_spec <- function(doc, cols = c("dataset" = "[N|n]ame|[D|d]ataset|[D|d]omain",
                                               "structure" = "[S|s]tructure",
                                               "label" = "[L|l]abel|[D|d]escription"), sheet = NULL){
   name_check <- names(cols) %in% c("dataset", "structure", "label") %>%
      all()
   if(!name_check | is.null(names(cols))){
      stop("Supplied column vector must be named using the following names:
              'dataset', 'structure', 'label'")
   }
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }

   # Get missing columns
   missing <- col_vars()$.ds_spec %>%
      discard(~. %in% names(cols))

   create_tbl(doc, cols) %>%
      distinct() %>%
      `is.na<-`(missing)
}

#' Spec to ds_vars
#'
#' Creates the ds_vars from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param sheet Regular expression for the sheet names
#' @param key_seq_sep_sheet A boolean to indicate if the key sequence is on a
#'   separate sheet. If set to false add the key_seq column name to the `cols`
#'   vector.
#' @param key_seq_cols names vector to get the key_sequence for each dataset
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
spec_type_to_ds_vars <- function(doc, cols = c("dataset" = "[D|d]ataset|[D|d]omain",
                                               "variable" = "[V|v]ariable [[N|n]ame]?|[V|v]ariables?",
                                               "order" = "[V|v]ariable [O|o]rder|[O|o]rder",
                                               "keep" = "[K|k]eep|[M|m]andatory"),
                                 key_seq_sep_sheet = TRUE,
                                 key_seq_cols = c("dataset" = "Dataset",
                                                  "key_seq" = "Key Variables"),
                                 sheet = "[V|v]ar|Datasets"){

   name_check <- names(cols) %in% c("variable", "dataset", "order",
                                    "keep", "key_seq", "core", "supp_flag") %>%
      all()

   name_check_extra <- names(key_seq_cols) %in% c("dataset", "key_seq") %>%
      all() %>%
      ifelse(key_seq_sep_sheet, ., TRUE) # Adding it cause we only want to check when sep sheet is true

   # Testing for names of vectors
   if(any(!name_check, !name_check_extra, is.null(names(cols)))){
      stop("Supplied column vector must be named using the following names:
              'variable', 'dataset', 'order', 'keep', 'core', 'key_seq', 'supp_flag'")
   }
   # Subsetting sheets
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   #Get base doc
   out <-doc %>%
      create_tbl(cols)

   # Getting the key seq values
   if(key_seq_sep_sheet){
      key_seq_df <- doc %>%
         create_tbl(key_seq_cols) %>%
         mutate(key_seq = str_split(key_seq, ",\\s"),
                key_seq = map(key_seq, function(x){
                   tibble(variable = x) %>%
                      mutate(key_seq = row_number())
                })) %>%
         unnest(key_seq)
      out <- left_join(out, key_seq_df, by = c("dataset", "variable"))
   }

   # Get missing columns
   missing <- col_vars()$.ds_vars %>%
      discard(~. %in% names(out))

   out %>%
      distinct() %>%
      `is.na<-`(missing) %>%
      mutate(key_seq = as.integer(key_seq),
             keep = yn_to_tf(keep),
             core = as.character(core),
             order = as.numeric(order))
}


#' Spec to var_spec
#'
#' Creates the var_spec from a list of datasets (optionally filtered by the sheet
#' input). The named vector `cols` is used to determine which is the correct
#' sheet and renames the columns. (Note: the keep column will be converted logical)
#'
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given pattern
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
spec_type_to_var_spec <- function(doc, cols = c("variable" = "[N|n]ame|[V|v]ariables?",
                                                "length" = "[L|l]ength",
                                                "label" = "[L|l]abel",
                                                "type" = "[T|t]ype",
                                                "dataset" = "[D|d]ataset|[D|d]omain",
                                                "format" = "[F|f]ormat"),
                                  sheet = "[V|v]ar"){
   # Check the names
   name_check <- names(cols) %in% c("variable", "length", "label",
                                    "type", "dataset", "common", "format") %>%
      all()
   if(!name_check | is.null(names(cols))){
      stop("Supplied column vector must be named using the following names:
              'variable', 'length', 'label', 'type', 'dataset', 'common', 'format'
              If common is not avaliable it can be excluded and will be automatically filled in.
           Additionally, dataset is only used to clarify if information differs by domain")
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
         group_by(variable) %>%
         mutate(unique = n_distinct(length, label, type),
                variable = if_else(unique == 1, variable,
                                   paste0(dataset, ".", variable)),
                length = as.numeric(length)) %>%
         distinct(variable, length, label, type, .keep_all = TRUE) %>%
         select(-dataset, -unique)
   }

   # Get missing columns
   missing <- col_vars()$.var_spec %>%
      discard(~. %in% names(out))
   out %>%
      `is.na<-`(missing) %>%
      distinct() %>%
      ungroup() %>%
      mutate(length = as.integer(length))
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
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param sheet Regular expression for the sheet name
#' @param where_sep_sheet Boolean value to control if the where information in a
#'   separate dataset. If the where information is on a separate sheet, set to
#'   true and provide the column information with the `where_cols` inputs.
#' @param where_cols Named list with an id and where field. All columns in the
#'   where field will be collapsed together
#' @param var_sheet Name of sheet with the Variable information on it. Metacore
#'   expects each variable will have a row in the value_spec. Because many
#'   specification only have information in the value tab this is added. If the
#'   information already exists in the value tab of your specification set to
#'   NULL
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
spec_type_to_value_spec <- function(doc, cols = c("dataset" = "[D|d]ataset|[D|d]omain",
                                                  "variable" = "[N|n]ame|[V|v]ariables?",
                                                  "origin" = "[O|o]rigin",
                                                  "type" = "[T|t]ype",
                                                  "code_id" = "[C|c]odelist|Controlled Term",
                                                  "sig_dig" = "[S|s]ignificant",
                                                  "where" = "[W|w]here",
                                                  "derivation_id" = "[M|m]ethod",
                                                  "predecessor" = "[P|p]redecessor"),
                                    sheet = NULL,
                                    where_sep_sheet = TRUE,
                                    where_cols = c("id" = "ID",
                                                   "where" = c("Variable", "Comparator", "Value")),
                                    var_sheet = "[V|v]ar"){
   name_check <- names(cols) %in% c("variable", "origin", "code_id", "sig_dig",
                                    "type", "dataset", "where", "derivation_id",
                                    "predecessor") %>%
      all()

   if(!name_check| is.null(names(cols))){
      stop("Supplied column vector must be named using the following names:
              'dataset', 'variable', 'origin', 'code_id', 'type', 'where', 'sig_dig', 'derivation_id',
              'predecessor'
              If derivation_id is not avaliable it can be excluded and dataset.variable will be used.

              If the where information is on a seperate sheet, put the column with cross ref as where.")
   }

   # Select a subset of sheets if specified
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }

   out <- create_tbl(doc, cols)

   # Does a var sheet exsist?
   if(!is.null(var_sheet)){
      var_sheet <- names(doc) %>%
         keep(~str_detect(., var_sheet))
   }

   # If so, add any variables not in the value sheet
   if(length(var_sheet) > 0){
      var_out <- doc[var_sheet] %>%
         map_dfr(function(x){
            var_out <- x %>%
               select_rename_w_dups(cols) %>%
               mutate(where = "TRUE")
            if(nrow(out) > 0){
               var_out  %>%
                  anti_join(out, by = "variable")
            } else {
               var_out
            }
         })

      # THIS ISN'T VERY PRETTY, IF SOMEONE HAS A BETTER IDEA PLEASE FIX
      # Needed in cause the value sheet is empty
      if(nrow(out) > 0 & nrow(var_out) > 0){
         out <- bind_rows(out, var_out)
      } else if(nrow(var_out) > 0) {
         out <- var_out
      } else {
         out
      }

   }

   if(where_sep_sheet & "where" %in% names(out)){
      where_df <- create_tbl(doc, where_cols) %>%
         mutate(
            where_new = pmap_chr(., function(...) {
               # Without c_across this gets a little weird
               # Use pmap and steal out the arg names
               vars <- list(...)
               # Filter down to only args that start with where
               wheres <- as.character(vars[which(str_starts(names(vars), 'where'))])
               # collapse it together
               paste(wheres, collapse=" ")
            })
         ) %>%
         select(id, where_new)
      out <- out %>%
         left_join(where_df, by = c("where" = "id")) %>%
         select(-where, where = where_new)
   } else if(where_sep_sheet) {
      warning("Not able to add where infromation from seperate sheet cause a where column is needed to cross-reference the information",
              call. = FALSE)
   }

   if(!"derivation_id" %in% names(cols)){
      out <- out %>%
         mutate(derivation_id = paste0(dataset, ".", variable))
   }

   # Get missing columns
   missing <- col_vars()$.value_spec %>%
      discard(~. %in% names(out))

   out %>%
      `is.na<-`(missing) %>%
      distinct() %>%
      mutate(sig_dig = as.integer(.data$sig_dig),
             derivation_id = case_when(
                !is.na(.data$derivation_id) ~ .data$derivation_id,
                str_to_lower(.data$origin) == "predecessor" ~ as.character(.data$predecessor),
                str_to_lower(.data$origin) == "assigned" ~ paste0(.data$dataset, ".", .data$variable))
      ) %>%
      select(-.data$predecessor)

}

#' Spec to codelist
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
#'   are all equal to a permitted value list. True by default
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
spec_type_to_codelist <- function(doc, codelist_cols = c("code_id" = "ID",
                                                         "name" = "[N|n]ame",
                                                         "code" = "^[C|c]ode|^[T|t]erm",
                                                         "decode" = "[D|d]ecode"),
                                  permitted_val_cols = NULL,
                                  dict_cols = c("code_id" = "ID",
                                                "name" = "[N|n]ame",
                                                "dictionary" = "[D|d]ictionary",
                                                "version" = "[V|v]ersion"),
                                  sheets = NULL, simplify = FALSE){
   if(is.null(codelist_cols)){
      stop("Codelist column names must be provided", call. = FALSE)
   } else {
      name_check <- names(codelist_cols) %in% c("code_id", "name", "code", "decode") %>%
         all()
      if(!name_check| is.null(names(codelist_cols))){
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
   # Get missing columns
   missing <- col_vars()$.codelist %>%
      discard(~. %in% names(cd_out))

   cd_out %>%
      `is.na<-`(missing) %>%
      distinct() %>%
      filter(!is.na(code_id)) %>%
      ungroup()
}

#' Spec to derivation
#'
#' Creates the derivation table from a list of datasets (optionally filtered by
#' the sheet input). The named vector `cols` is used to determine which is the
#' correct sheet and renames the columns. The derivation will be used for
#' "derived" origins, the comments for "assigned" origins, and predecessor for
#' "predecessor" origins.
#' @param doc Named list of datasets @seealso [read_all_sheets()] for exact
#'   format
#' @param cols Named vector of column names. The column names can be regular
#'   expressions for more flexibility. But, the names must follow the given
#'   pattern
#' @param var_cols Named vector of the name(s) of the origin, predecessor and
#'   comment columns. These do not have to be on the specified sheet.
#' @param sheet Regular expression for the sheet name
#'
#' @return a dataset formatted for the metacore object
#' @export
#'
#' @family spec builder
#' @importFrom purrr quietly
spec_type_to_derivations <- function(doc, cols = c("derivation_id" = "ID",
                                                   "derivation" = "[D|d]efinition|[D|d]escription"),
                                     sheet = "Method|Derivations?",
                                     var_cols = c("dataset" = "[D|d]ataset|[D|d]omain",
                                                  "variable" = "[N|n]ame|[V|v]ariables?",
                                        "origin" = "[O|o]rigin",
                                        "predecessor" = "[P|p]redecessor",
                                        "comment" = "[C|c]omment")){

   name_check <- names(cols) %in% c("derivation_id", "derivation") %>%
      all()
   if(!name_check| is.null(names(cols))){
      stop("Supplied column vector must be named using the following names:
              'derivation_id', 'derivation'")
   }

   name_check <- names(var_cols) %in% c('dataset', 'variable', 'origin', 'predecessor', 'comment') %>%
      all()
   if(!name_check| is.null(names(var_cols))){
      stop("Supplied variable column vector must be named using the following names:
               'dataset', 'variable', 'origin', 'predecessor', 'comment'")
   }
   # Get the predecessor
   ls_derivations <- quietly(create_tbl)(doc, var_cols)$result
   if(class(ls_derivations)[1] == "list"){
      ls_derivations <- ls_derivations %>%
         reduce(bind_rows)
   }
   other_derivations <- ls_derivations %>%
      mutate(
         derivation_id = case_when(
            str_to_lower(.data$origin) == "predecessor" ~ as.character(.data$predecessor),
            str_to_lower(.data$origin) == "assigned" ~ paste0(.data$dataset, ".", .data$variable),
            TRUE ~ NA_character_
            ),
         derivation = case_when(
            str_to_lower(.data$origin) == "predecessor" ~ as.character(.data$predecessor),
            str_to_lower(.data$origin) == "assigned" ~ .data$comment,
            TRUE ~ NA_character_
         )) %>%
      filter(!is.na(.data$derivation_id)) %>%
      select(.data$derivation, .data$derivation_id)

   # Select a subset of sheets if specified
   if(!is.null(sheet)){
      sheet_ls <- str_subset(names(doc), sheet)
      doc <- doc[sheet_ls]
   }
   out <- create_tbl(doc, cols)

   # Get missing columns
   missing <- col_vars()$.derivations %>%
      discard(~. %in% names(out))


   out %>%
      `is.na<-`(missing) %>%
      bind_rows(other_derivations) %>%
      distinct() %>%
      filter(!is.na(derivation_id))
}
### Helper Functions

#' Create table
#'
#' This function creates a table from excel sheets. This is mainly used
#' internally for building spec readers, but is exported so others who need to
#' build spec readers can use it.
#' @param doc list of sheets from a excel doc
#' @param cols vector of regex to get a datasets base on which columns it has.
#'   If the vector is named it will also rename the columns
#'
#' @return dataset (or list of datasets if not specific enough)
#' @export
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
         paste0("Unable to identify a sheet with all columns.\n", . ) %>%
         stop(call. = FALSE)

   } else if(length(matches) == 1){
      # Check names and write a better warning message if names don't work
      ds_nm <- matches[[1]] %>%
         names()
      nm_test <- cols %>%
         map_int(~sum(str_detect(ds_nm, .))) %>%
         keep(~ . != 1)
      if(length(nm_test) > 0) {
         # See if an exact match will
         test_exact <- cols[names(nm_test)] %>%
            paste0("^", ., "$") %>%
            map_int(~sum(str_detect(ds_nm, .))) %>%
            keep(~ . != 1)
         if(length(test_exact) == 0){
            cols[names(nm_test)] <- cols[names(nm_test)] %>%
               paste0("^", ., "$")
         } else {
            str_c(names(nm_test),  " matches ",nm_test, " columns") %>%
               str_c(collapse = "\n ") %>%
               paste0("Unable to rename the following columns in ", names(matches[1]), ":\n ", .,
                      "\nPlease check your regular expression ") %>%
               stop(call. = FALSE)
         }
      }

      # This needs to be done columnwise to allow for duplicate selection of the same column
      select_rename_w_dups(matches[[1]], cols)

   } else {
      sheets_mats <- matches %>%
         names()
      paste("Column names are not specific enough to identify a single sheet. The following",
            length(sheets_mats),
            "match the criteria set:", paste(sheets_mats, collapse = ", ")) %>%
         warning(., call. = FALSE)
      matches %>%
         map(~select_rename_w_dups(., cols))
   }
}


#' Yes No to True False
#'
#' @param x takes in a vector to convert
#'
#' @return returns a logical vector or normal vector with warning
#' @noRd
#'
yn_to_tf <- function(x){
   if(all(is.na(x) | str_detect(x, regex("^y$|^n$|^yes$|^no$", ignore_case = T)))){
      case_when(str_detect(x, regex("^y$|^yes$", ignore_case = T)) ~ TRUE,
                str_detect(x, regex("^n$|^no$", ignore_case = T)) ~ FALSE,
                is.na(x) ~ NA)
   } else if(is.logical(x)){
      x
   } else {
      warning("Keep column needs to be True or False, please correct before converting to a Metacore object",
              call. = FALSE)
      x
   }
}


#' Select in a dataset with renames
#'
#' This works like select, but if there are duplicates it won't cause issues
#'
#' @param .data dataset to select columns and rename
#' @param cols named vector
#'
#' @return dataset
#' @noRd
#' @importFrom purrr safely
select_rename_w_dups <- function(.data, cols){
   pull_safe <- safely(~select(.x, matches(.y, ignore.case = FALSE)))
   cols %>%
      map_dfr(function(col){
         out <- pull_safe(.data, col) %>%
            .$result
         if(ncol(out) == 1){
            out <- out %>% pull(1)
         } else {
            out <- NULL
         }
         out
      })
}
