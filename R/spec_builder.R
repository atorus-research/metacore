

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




#' Read information from spec document
#'
#' @param path string of the file path
#'
#' @return
#' @noRd
read_all_sheets <- function(path){
   sheets <- excel_sheets(path)
   all_dat <- sheets %>%
      map(~read_excel(path, sheet = .))
   names(all_dat) <- sheets
   all_dat
}


spec_type_to_ds_spec <- function(doc){
   cols <- c("dataset" = "[N|n]ame|[D|d]ataset|[D|d]omain",
             "structure" = "[S|s]tructure",
             "label" = "[L|l]abel|[D|d]escription")
   create_tbl(doc, cols) %>%
      distinct()
}

spec_type_to_ds_vars <- function(doc, cols = NULL){
   if(is.null(cols)){
      cols <- c("dataset" = "[D|d]ataset|[D|d]omain",
                "variable" = "[V|v]ariable [[N|n]ame]?|[V|v]ariables?",
                "key_seq" = "[V|v]ariable [O|o]rder|[S|s]eq|[O|o]rder",
                "keep" = "[K|k]eep|[M|m]andatory")
   }
   #TODO add error or warning if not named?

   var_sheet <- str_subset(names(doc), "[V|v]ar")
   doc[var_sheet] %>%
      create_tbl(cols) %>%
      distinct()
}

spec_type_to_var_spec <- function(doc, col = NULL){
   if(is.null(cols)){
      cols <- c("variable" = "[N|n]ame|[V|v]ariables?",
                "length" = "[L|l]ength",
                "label" = "[L|l]abel",
                "type" = "[T|t]ype",
                "dataset" = "[D|d]ataset|[D|d]omain")
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

spec_type_to_value_spec <- function(doc){
   doc$val_meta %>%
      rename(ds_var = matches("VLM [N|n]ame"),
             origin = matches("[O|o]rigin"),
             type = matches("[T|t]ype"),
             code_id = matches("Controlled Term"),
             where = matches("Parameter Code"),
             dev = matches("[D|d]efinition")) %>%
      select(ds_var, origin, type, code_id, where, dev) %>%
      distinct() %>%
      filter(str_detect(ds_var, "\\.|\\_")) %>%
      separate(ds_var, c("dataset", "variable"),
               sep = "\\.|\\_") %>%
      mutate(derivation_id = if_else(is.na(dev), NA_character_,
                                     paste0(variable, "_", row_number()))
      ) %>%
      select(-dev)
}

spec_type_to_code_list <- function(doc){
   doc$ct %>%
      rename(code_id = "Codelist Code",
             names = matches("[N|n]ame"),
             codes = "Coded Value",
             decodes = matches ("[D|d]ecode")) %>%
      select(code_id, names, codes, decodes) %>%
      group_by(code_id) %>%
      mutate(type = if_else(all(codes == decodes), "permitted_val",
                            "code_decode")) %>%
      nest(codes = c(codes, decodes)) %>%
      mutate(codes = if_else(type ==  "permitted_val",
                             lapply(codes, function(df) df %>% pull(codes)),
                             codes))
}

spec_type_to_derivations <- function(doc){
   doc$val_meta %>%
      rename(ds_var = matches("VLM [N|n]ame"),
             origin = matches("[O|o]rigin"),
             type = matches("[T|t]ype"),
             code_id = matches("Controlled Term"),
             where = matches("Parameter Code"),
             derivation = matches("[D|d]efinition")) %>%
      select(ds_var, origin, type, code_id, where, derivation) %>%
      distinct() %>%
      filter(str_detect(ds_var, "\\.|\\_")) %>%
      separate(ds_var, c("dataset", "variable"),
               sep = "\\.|\\_") %>%
      mutate(derivation_id = paste0(variable, "_", row_number())) %>%
      select(-everything(), derivation_id, derivation) %>%
      filter(!is.na(derivation))
}


#' Select sheet
#'
#' @param doc list of sheets from a excel dos
#' @param cols vector of regex to get a datasets base on which columns it has.
#'   If the vector is named it will also rename the columns
#'
#' @return dataset (or list of datasets if not spefic engough)
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
            paste0("Sheet '", sheet_name, "' is the closest match, but unable to match the following column(s)",
                   paste(names(vars), collapse = ","))
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
