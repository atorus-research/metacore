#' Define XML to DataDef Object
#'
#' Given a path, this function converts the define xml to a DataDef Object
#'
#' @param path location of the define xml as a string
#' @param quiet Option to quietly load in, this will suppress warnings, but not errors
#'
#' @return DataDef Object
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_ns_strip
define_to_metacore <- function(path, quiet = FALSE){

   xml <- read_xml(path)
   xml_ns_strip(xml)

   define_version <- xml_find_all(xml, "//MetaDataVersion") %>%
      xml_attr("DefineVersion") %>%
      as.numeric_version()


   ds_spec <- xml_to_ds_spec(xml)
   ds_vars <- xml_to_ds_vars(xml)
   var_spec <- xml_to_var_spec(xml)
   value_spec <- xml_to_value_spec(xml)
   code_list <- xml_to_codelist(xml)
   derivations <- xml_to_derivations(xml)
   if(!quiet){
      out <- metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist = code_list)
   } else{
      out<- suppressWarnings(metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, codelist = code_list))
      message("Loading in metacore object with suppressed warnings")
   }
   out
}


#' XML to Data Set Spec
#'
#' Creates a dataset specification, which has the domain name and label for each dataset
#' @param doc xml document
#'
#' @return data frame with the data set specifications
#' @family xml builder
#' @export
#' @importFrom xml2 xml_attr xml_find_first xml_text
xml_to_ds_spec <- function(doc) {
   # Read in the dataset level nodes
   xml_find_all(doc, "//MetaDataVersion/ItemGroupDef[contains(@OID, 'IG')]") %>%
   map_dfr(function(node){
      tibble(
         dataset = xml_attr(node, "Name"),
         structure = xml_attr(node, "Structure"),
         label = xml_find_first(node, "./Description") %>% xml_text()
      )
   })
}


#' XML to Data Set Var table
#'
#' Creates the ds_vars table, which acts as a key between the datasets and the var spec
#' @param doc xml document
#'
#' @return data frame with the dataset and variables
#' @family xml builder
#' @export
#'
xml_to_ds_vars <- function(doc) {
   # Each dataset is an ItemGroupDef
   xml_find_all(doc, "//ItemGroupDef") %>%
      map_dfr(function(node){
         # Each Variable is a Item Ref
         child_node <- xml_find_all(node, "./ItemRef")
         tibble(
            dataset = xml_attr(node, "Name"),
            oid = xml_attr(child_node, "ItemOID"),
            mandatory = xml_attr(child_node, "Mandatory"),
            key_seq = xml_attr(child_node, "KeySequence") %>%
               as.integer(),
            order = xml_attr(child_node, "OrderNumber") %>%
               as.integer()
         )
      }) %>%
      mutate(
         variable = id_to_var(.data$oid),
         keep = .data$mandatory == "Yes",
         core = NA_character_,
         supp_flag = NA
      ) %>%
      select(.data$dataset, .data$variable, .data$key_seq,
             .data$order, .data$keep, .data$core, .data$supp_flag,
             -.data$mandatory, -.data$oid)
}


#' XML to variable spec
#'
#' Takes a define xml and returns a dataset with specifications for each
#' variable. The variable will just be the variable, unless the specification
#' for that variable differ between datasets
#'
#' @param doc define xml document
#'
#' @return data frame with variable, length, label columns
#' @family xml builder
#' @export
#' @importFrom xml2 xml_attr xml_find_all xml_text
xml_to_var_spec <- function(doc) {

   # Gets the name, id, and length from the variable node and the description from the child
   var_info <- xml_find_all(doc, "//ItemDef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_attr(node,"OID") %>% as.character(),
            variable = xml_attr(node, "Name") %>% as.character(),
            type = xml_attr(node, "DataType"),
            length = xml_attr(node, "Length") %>% as.integer(),
            format = xml_attr(node, "DisplayFormat"),
            label =  xml_find_first(node, "./Description/TranslatedText") %>%
               xml_text()
         )
      })

   possible_vars <- xml_find_all(doc, "//ItemGroupDef/ItemRef") %>%
      map_chr(function(node){
         oid = xml_attr(node, "ItemOID")
      })

   # Get for each variable, get the number of distinct lengths and labels
   dist_df <- var_info %>%
      filter(.data$oid %in% possible_vars) %>%
      distinct(.data$variable, .data$length, .data$label, .data$type, .keep_all = TRUE) %>%
      group_by(.data$variable) %>%
      mutate(
         n = n(),
         common = NA
      ) %>%
      ungroup()

   # For variables with more than one distinct label, this gets all the full
   # variable names with that root. Sometimes 3 variables will have the same root
   # (i.e. ARMCD), 2 of them will match, but one of them won't. This means the
   # two matching will have been collapsed to one in the distinct and we have to
   # bring back the one that got dropped. Cause all of them need to be DS.var
   full_name_vars <- dist_df %>%
      filter(n > 1) %>%
      select(.data$variable) %>%
      inner_join(var_info, by = "variable")  %>%
      mutate(variable = str_remove(.data$oid, "^IT\\.")) %>%
      distinct()

   # Combine the variables that need full names with the variables that don't
   dist_df %>%
      filter(n == 1) %>%
      bind_rows(full_name_vars) %>%
      select(.data$variable, .data$type, .data$length, .data$label,
             .data$format, .data$common, -.data$n, -.data$oid)
}


#' XML to value spec
#'
#' Takes a define xml and pulls out the value level metadata including codelist_id's,
#' defines_id's, and where clause. There is one row per variable expect when there
#' is a where clause, at which point there is one row per value.
#' @param doc xml document
#'
#' @return tibble with the value level information
#' @family xml builder
#' @export
#'
#' @importFrom xml2 xml_attr xml_find_first xml_parent xml_find_all
xml_to_value_spec <- function(doc) {
   # Get information in the item definition
   item_def <- xml_find_all(doc, "//ItemDef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_attr(node,"OID") %>% as.character(),
            variable = xml_attr(node, "Name") %>% as.character(),
            type = xml_attr(node, "DataType"),
            sig_dig = xml_attr(node, "SignificantDigits") %>% as.integer(),
            origin = xml_find_first(node, "./def:Origin") %>% xml_attr("Type"),
            page_num =  xml_find_first(node, "./def:Origin/def:DocumentRef/def:PDFPageRef") %>% xml_attr("PageRefs"),
            predecessor = xml_find_first(node, "./def:Origin") %>% xml_text(),
            comment_id = xml_attr(node,"CommentOID"),
            code_id = xml_find_first(node, "CodeListRef") %>% xml_attr("CodeListOID"),
            varname = xml_attr(node, "SASFieldName") %>% as.character()
         )
      }) %>%
      mutate(
         origin = if_else(.data$origin == "Collected" & !is.na(.data$page_num),
                          paste0(.data$origin,", page_num = ", .data$page_num),
                          .data$origin)
      ) %>%
      select(-.data$page_num)

   # Pull the information from the item reference only for dataset variable, not where level information
   derivations <- xml_find_all(doc, "//ItemGroupDef/ItemRef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_attr(node, "ItemOID") %>% as.character(),
            dataset = xml_parent(node) %>% xml_attr("Name") %>% as.character(),
            derivation_id = xml_attr(node, "MethodOID")
         )
      })
   # Combine all the item information but
   item_info <- left_join(derivations, item_def, by = "oid")

   where_to_merge <- xml_find_all(doc, "//def:ValueListDef/ItemRef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_parent(node) %>% xml_attr("OID") %>% as.character(),
            item_oid = xml_attr(node, "ItemOID"),
            ord = xml_attr(node, "OrderNumber"),
            where_oid = xml_find_all(node, "./def:WhereClauseRef") %>%
               xml_attr("WhereClauseOID"),
            derivation_id = xml_attr(node, "MethodOID")
         )
      }
      )

   where_eqs <- xml_find_all(doc, "//def:WhereClauseDef[@OID]/RangeCheck") %>%
      map_dfr(function(node){
         tibble(
            where_oid = xml_parent(node) %>% xml_attr("OID"),
            left = xml_attr(node, "ItemOID"),
            test = xml_attr(node, "Comparator"),
            right = xml_find_all(node, "./CheckValue") %>% xml_text()
         )
      }
      )
   # create 0x4 tibble if where_eqs is 0x0
   # tmp workaround until below bug is resolved in purrr
   # https://github.com/tidyverse/purrr/issues/824
   if(nrow(where_eqs) == 0){
      where_eqs <- tibble(where_oid=character(),
                          left=character(),
                          test=character(),
                          right = character())
   }

   if(nrow(where_to_merge) == 0){
      where_eqs <- where_eqs %>%
         mutate(item_oid = .data$left,
                derivation_id = paste0("MT", str_remove(.data$left, "IT"), ".", .data$right),
                ord = NA,
                oid = .data$left) %>%
         left_join(item_def, by = c("oid")) %>%
         left_join(select(derivations, -.data$derivation_id), by = c("oid"))

   } else{
      where_eqs<- full_join(where_to_merge, where_eqs, by = "where_oid") %>%
         left_join(item_def, by = c("item_oid" = "oid")) %>%
         # Allow for merging with the derivations to get the dataset
         mutate(oid = paste0("IT", str_remove(.data$oid, "^VL")),
                variable = .data$varname) %>%
         left_join(select(derivations, -.data$derivation_id), by = c("oid"))
   }

   all_where_eqs <- where_eqs  %>%
      group_by(.data$where_oid) %>%
      mutate(var = str_extract(.data$left, "\\w*$"),
             right = paste0("'", .data$right, "'"),
             test = case_when(.data$test == "EQ" ~ "==",
                              .data$test == "LT" ~ "<",
                              .data$test == "LE" ~ "<=",
                              .data$test == "GT" ~ ">",
                              .data$test == "GE" ~ ">=",
                              .data$test == "NE" ~ "!=",
                              TRUE ~ .data$test),
             eq = case_when( test == "IN" ~ paste(.data$var, "%in%", "c(",
                                                  paste(.data$right, collapse = ","),
                                                  ")"),
                             test == "NOTIN" ~ paste("!", .data$var, "%in%", "c(",
                                                     paste(.data$right, collapse = ","),
                                                     ")"),
                             TRUE ~ paste(.data$var, .data$test, .data$right, collapse = " & "))
      ) %>%
      select(-.data$left, -.data$var, -.data$test, -.data$right) %>%
      distinct() %>%
      group_by(.data$item_oid, .data$derivation_id) %>%
      mutate(full_eq = str_c(.data$eq, collapse = "||")) %>%
      filter(!is.na(.data$item_oid)) %>%
      ungroup() %>%
      select(-.data$eq, where = .data$full_eq, .data$derivation_id,
             -.data$where_oid, -.data$ord, -.data$item_oid)

   val_spec <- item_info %>%
      anti_join(all_where_eqs, by = c("oid")) %>%  #remove any variables with a where
      bind_rows(all_where_eqs) %>%
      mutate(derivation_id = case_when(
         .data$origin == "Predecessor" & !is.na(.data$predecessor) ~ .data$predecessor,
         .data$origin == "Assigned" & !is.na(.data$comment_id) ~ .data$comment_id,
         TRUE ~ .data$derivation_id)) %>%
      select(.data$dataset, .data$variable, .data$code_id, .data$derivation_id,
             .data$type, .data$origin, .data$where, .data$sig_dig,
         -.data$predecessor, -.data$comment_id, -.data$varname,
             -.data$oid)

   val_spec
}


#' XML to code list
#'
#' Reads in a define xml and creates a code_list table. The code_list table is a
#' nested tibble where each row is a code list or permitted value list. The code
#' column contains a vector of a tibble depending on if it is a permitted values
#' or code list
#'
#' @param doc xml document
#'
#' @return a tibble containing the code list and permitted value information
#' @family xml builder
#' @export
xml_to_codelist <- function(doc) {
   cl_pv <- xml_find_all(doc, "//CodeList") %>%
      map_dfr(function(node){
         #Values within the code/decode
         node_children_EI <- xml_find_all(node, "./EnumeratedItem")
         node_children_CL <- xml_find_all(node, "./CodeListItem")

         decodes <- c(xml_find_all(node_children_EI, "./Decode") %>% xml_text(),
                      xml_find_all(node_children_CL, "./Decode") %>% xml_text())
         if(length(decodes) == 0){
            decodes <- NA_character_
         }

         tibble(
            code_id = xml_attr(node, "OID"),
            name = xml_attr(node, "Name"),
            code = c(xml_attr(node_children_EI, "CodedValue"),
                     xml_attr(node_children_CL, "CodedValue")),
            decode = decodes
         )
      }) %>%
      group_by(code_id) %>%
      mutate(type = if_else(all(is.na(decode)),
                            "permitted_val", "code_decode"))

   permitted_val <- cl_pv %>%
      filter(type == "permitted_val") %>%
      select(-.data$decode) %>%
      nest(codes = c(.data$code))

   code_decode <- cl_pv %>%
      filter(type == "code_decode") %>%
      nest(codes = c(.data$code, .data$decode))


   external_libs <- xml_find_all(doc, "//CodeList/ExternalCodeList") %>%
      map_dfr(function(node){
         tibble(
            code_id = xml_parent(node) %>% xml_attr("OID"),
            name = xml_parent(node) %>% xml_attr("Name"),
            dictionary = xml_attr(node, "Dictionary"),
            version = xml_attr(node, "Version"),
            type = "external_library"
         )
      })
   if(nrow(external_libs) > 0){
      external_libs <- external_libs |>
         nest(codes = c(.data$dictionary, .data$version))
   }


   # Combinging the code decode with the permitted values
   bind_rows(code_decode, permitted_val, external_libs) %>%
      ungroup()
}



#' XML to derivation table
#'
#' This reads in a xml document and gets all the derivations/comments. These can
#' be cross referenced to variables using the derivation_id's
#' @param doc xml document
#'
#' @return dataframe with derivation id's and derivations
#' @family xml builder
#' @export
#'
xml_to_derivations <- function(doc) {

   derivation <-
      xml_find_all(doc, "//MethodDef") %>%
      map_dfr(function(node){
         tibble(derivation_id = xml_attr(node, "OID"),
                derivation = xml_find_first(node, "./Description/TranslatedText") %>%
                   xml_text())
      })

   comment <-
      xml_find_all(doc, "//def:CommentDef") %>%
      map_dfr(function(node){
         tibble(derivation_id = xml_attr(node, "OID"),
                derivation = xml_find_first(node, "./Description/TranslatedText") %>%
                   xml_text())
      })


   predecessor <- xml_find_all(doc, "//ItemDef") %>%
      map_dfr(function(node){
         tibble(
            derivation_id = xml_find_first(node, "./def:Origin") %>% xml_text(),
            derivation = derivation_id
         )
      }) %>%
      filter(!is.na(.data$derivation) & str_length(.data$derivation) > 0)

   bind_rows(derivation,
             comment,
             predecessor) %>%
      distinct()
}
