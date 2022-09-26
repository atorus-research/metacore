#' Define XML to DataDef Object
#'
#' Given a path, this function converts the define xml to a DataDef Object
#'
#' @param path location of the define xml as a string
#' @param quiet Option to quietly load in, this will suppress warnings, but not errors
#'
#' @return DataDef Object
#' @export
#'
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
   map_dfr(xml_find_all(doc, "//MetaDataVersion/ItemGroupDef[contains(@OID, 'IG')]"), function(nn){
      tibble(
         dataset = xml_attr(nn, "Name"),
         structure = xml_attr(nn, "Structure"),
         lable = xml_find_first(nn, "./Description") %>% xml_text()
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
   # Get the name of each dataset
   dataset_nodes <- get_ds_lvl_nodes(doc)

   # Get the variable names, key sequence and keep for each variable in each ds
   dataset_nodes %>%
      map_dfr(function(x) {
         # Gets the name attribute of the dataset node
         dataset <- xmlGetAttr(x, "Name")
         # Gets the child node, Item Ref, which contains variable level information
         child_var_nodes <- xmlElementsByTagName(x, "ItemRef")
         # Pulls the relevant information from child node
         tibble(
            dataset = dataset,
            variable = child_var_nodes %>% get_node_attr("ItemOID"),
            mandatory = child_var_nodes %>% get_node_attr("Mandatory"),
            key_seq = child_var_nodes %>%
               get_node_attr("KeySequence") %>%
               as.integer(),
            order = child_var_nodes %>%
               get_node_attr("OrderNumber") %>%
               as.integer()
         )
      }) %>%
      mutate(
         variable = id_to_var(.data$variable),
         keep = .data$mandatory == "Yes",
         core = NA_character_,
         supp_flag = NA
      ) %>%
      select(-.data$mandatory)
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
xml_to_var_spec <- function(doc) {
   # Get information from the variable level nodes
   var_nodes <- get_var_lvl_nodes(doc)

   # Gets the name, id, and length from the variable node and the description from the child
   var_info <- tibble(
      var_full = var_nodes %>% get_node_attr("OID"),
      variable = var_nodes %>% get_node_attr("Name"),
      type = var_nodes %>% get_node_attr("DataType"),
      length = var_nodes %>% get_node_attr("Length") %>%
         as.integer(),
      # Get labels
      label = var_nodes %>% map_chr(get_node_description),
      format = var_nodes %>% get_node_attr("DisplayFormat")
   )

   possible_vars <- ds_var_ls(doc) %>%
      pull(.data$variable) %>%
      unique()

   # Get for each variable, get the number of distinct lengths and labels
   dist_df <- var_info %>%
      filter(.data$variable %in% possible_vars) %>%
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
      mutate(variable = str_remove(.data$var_full, "^IT\\.")) %>%
      distinct()

   # Combine the variables that need full names with the variables that don't
   dist_df %>%
      filter(n == 1) %>%
      bind_rows(full_name_vars) %>%
      select(-.data$n, -.data$var_full)
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
#' @importFrom xml2 xml_attr xml_find_first
xml_to_value_spec <- function(doc) {
   # Get information in the item definition
   item_def <- xml_find_all(xml, "//ItemDef") %>%
      map_dfr(function(node){
         data.frame(
            oid = xml_attr(node,"OID"),
            variable = xml_attr(node, "Name"),
            type = xml_attr(node, "DataType"),
            sig_dig = xml_attr(node, "SignificantDigits"),
            origin = xml_find_first(node, "./def:Origin") %>% xml_attr("Type"),
            page_num =  xml_find_first(node, "./def:Origin/def:DocumentRef/def:PDFPageRef") %>% xml_attr("PageRefs"),
            predecessor = xml_find_first(node, "./def:Origin") %>% xml_text(),
            comment_id = xml_attr(node,"CommentOID"),
            codelist_id = xml_find_first(node, "CodeListRef") %>% xml_attr("CodeListOID")
         )
      }) %>%
      mutate(
         origin = if_else(.data$origin == "Collected" & !is.na(.data$page_num),
                          paste0(.data$origin,", page_num = ", .data$page_num),
                          .data$origin)
      ) %>%
      select(-.data$page_num)

   # Pull the information from the item reference
   derivations <- xml_find_all(xml, "//ItemRef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_attr(node, "ItemOID"),
            derivation_id_all = xml_attr(node, "MethodOID")
         )
      })
   # Combine all the item information
   item_info <- left_join(item_def, derivations, by = "oid")

   where_to_merge <- xml_find_all(xml, "//def:ValueListDef/ItemRef") %>%
      map_dfr(function(node){
         tibble(
            oid = xml_parent(node) %>% xml_attr("OID"),
            item_oid = xml_attr(node, "ItemOID"),
            ord = xml_attr(node, "OrderNumber"),
            where_oid = xml_find_all(node, "./def:WhereClauseRef") %>%
               xml_attr("WhereClauseOID"),
            derivation_id = xml_attr(node, "MethodOID")
         )
      }
      )

   where_eqs <- xml_find_all(xml, "//def:WhereClauseDef[@OID]/RangeCheck") %>%
      map_dfr(function(node){
         tibble(
            where_oid = xml_parent(node) %>% xml_attr("OID"),
            left = xml_attr(node, "ItemOID"),
            test = xml_attr(node, "Comparator"),
            right = xml_find_all(node, "./CheckValue") %>% xml_text()
         )
      }
      )

   if(nrow(where_to_merge) == 0){
      where_eqs <- where_eqs %>%
         mutate(item_oid = .data$left,
                derivation_id = paste0("MT", str_remove(.data$left, "IT"), ".", .data$right))
   } else{
      where_eqs<- full_join(where_to_merge, where_eqs, by = "where_oid")
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
      select(.data$item_oid, where = .data$full_eq, .data$derivation_id)



   val_spec <- item_info %>%
      left_join(all_where_eqs, by = c("oid" = "item_oid")) %>%
      mutate(derivation_id = case_when(
         origin == "Predecessor" & !is.na(predecessor) ~ predecessor,
         origin == "Assigned" & !is.na(comment_id) ~ comment_id,
         !is.na(derivation_id_all) ~ derivation_id_all,
         TRUE ~ derivation_id)) %>%
      select(-predecessor, -comment_id, -derivation_id_all)
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
   cl_nodes <- get_nodes(doc, "//ns:CodeList[ns:CodeListItem]")
   # Get a table with the information about the code list
   # Done like this because map_chr is faster than map_dfr
   code_decode <- tibble(
      code_id = cl_nodes %>% get_node_attr("OID"),
      name = cl_nodes %>% get_node_attr("Name"),
      dataType = cl_nodes %>% get_node_attr("DataType"),
      codes = cl_nodes %>% get_codes(),
      type = "code_decode"
   )

   # Permitted Values
   # following the same method as above, get permitted value information
   permitted_nodes <- get_nodes(doc, "//ns:CodeList[ns:EnumeratedItem]")
   permitted <- tibble(
      code_id = permitted_nodes %>% get_node_attr("OID"),
      name = permitted_nodes %>% get_node_attr("Name"),
      dataType = permitted_nodes %>% get_node_attr("DataType"),
      type = "permitted_val",
      codes = permitted_nodes %>% get_permitted_vals()
   )

   # External Libraries
   ex_lib_nodes <- get_nodes(doc, "//ns:CodeList[ns:ExternalCodeList]")
   ex_lib <- tibble(
      code_id = ex_lib_nodes %>% get_node_attr("OID"),
      name = ex_lib_nodes %>% get_node_attr("Name"),
      dataType = ex_lib_nodes %>% get_node_attr("DataType"),
      dictionary = ex_lib_nodes %>%
         map_chr(~ get_child_attr(., "ExternalCodeList", "Dictionary")),
      version = ex_lib_nodes %>%
         map_chr(~ get_child_attr(., "ExternalCodeList", "Version")),
      type = "external_library"
   ) %>%
      nest(codes = c(.data$dictionary, .data$version))

   # Combinging the code decode with the permitted values
   bind_rows(code_decode, permitted, ex_lib) %>%
      select(-.data$dataType)
}



#' XML to derivation table
#'
#' This reads in a xml document and gets all the derivations/comments. These can
#' be cross referenced to variables using the derivation_id's
#' @param doc ?
#'
#' @return dataframe with derivation id's and derivations
#' @family xml builder
#' @export
#'
xml_to_derivations <- function(doc) {
   # Gets derivation node
   method_nodes <- get_nodes(doc, path = "//ns:MethodDef")

   # Gets the derivation ID and description for each node
   derivation <- tibble(
      derivation_id = method_nodes %>%
         get_node_attr("OID"),
      derivation = method_nodes %>%
         map_chr(get_node_description)
   )
}
