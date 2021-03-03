

#' Read information from spec document
#'
#' @param path string of the file path
#'
#' @return
#' @export
read_spec1 <- function(path){
   sheets <- excel_sheets(path)
   dom <- sheets %>%
      sheet_name("[D|d]omains?")
   spec <- sheets %>%
      sheet_name("[S|s]pec[ification|s]?")
   var <- sheets %>%
      sheet_name("[V|v]ar.*")
   val_meta <- sheets %>%
      sheet_name("[V|v]al[ue]?.*")
   ct <- sheets %>%
      sheet_name("CT|ct|[C|c]ontrolled.*")
   derviation <- sheets %>%
      sheet_name(".*Method|[D|d]erivations?")
   to_read <- c(dom, spec, var, val_meta, ct, derviation)

   data <- to_read %>% map(~read_excel(path, sheet = .))
   names(data) <- c("dom", "spec", "var", "val_meta", "ct", "derviation")
   data
}


#      .var_spec = c("variable", "length", "label", "type", "common"),
#      .value_spec = c("type", "origin", "code_id", "dataset", "variable", "where", "derivation_id"),
#      .derivations = c("derivation_id", "derivation"),
#      .codelist= c("code_id", "names","type", "codes"),
#      .change_log = c("table_chg", "column_chg", "what_chg"))

spec1_to_ds_spec <- function(doc){
   doc$dom %>%
      rename(dataset = matches("[N|n]ame|dataset|[D|d]omain"),
             structure = matches("[S|s]tructure"),
             label = matches("[L|l]abel")) %>%
      select(dataset, structure, label) %>%
      distinct()
}

spec1_to_ds_vars <- function(doc){
   doc$spec %>%
      rename(dataset = matches("dataset|[D|d]omain"),
             variable = matches("[V|v]ariable [N|n]ame"),
             key_seq = matches("[V|v]ariable [O|o]rder|[S|s]eq"),
             keep = matches("[K|k]eep|[M|m]andatory"),
             core = matches("[C|c]ore|[R|r]equired")) %>%
      select(dataset, variable, key_seq, keep, core) %>%
      distinct()
}

spec1_to_var_spec <- function(doc){
   var_df <- doc$var %>%
      rename(variable = matches("[N|n]ame"),
             length = matches("[L|l]ength"),
             label = matches("[L|l]abel"),
             type = matches("[T|t]ype") ) %>%
      select(variable, length, label, type) %>%
      distinct()
   common_vars <- doc$dom$`Key Variables` %>%
      str_split(",") %>%
      map(str_trim)
   common_vars <- common_vars[[1]]%>%
      keep(function(x){
         common_vars%>% map_lgl(~x%in% .) %>% all()
      })

   var_df %>%
      mutate(common = variable %in% common_vars)
}

spec1_to_value_spec <- function(doc){
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

spec1_to_code_list <- function(doc){
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

spec1_to_derivations <- function(doc){
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


#' Get name of sheet
#'
#' @param sheets list of all sheet names
#' @param regex regex to parse out desired sheet
#'
#' @return string of the matching
#' @noRd
sheet_name <- function(sheets, regex){
   sheet_name <- sheets %>%
      str_extract(regex) %>%
      discard(is.na)
   if(length(sheet_name) > 1){
      stop("Sheet names don't follow required naming convention. Please look at the documentation and change accordingly")
   }
   sheet_name
}
