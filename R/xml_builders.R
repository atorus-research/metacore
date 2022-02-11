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
   doc <- xmlTreeParse(path, useInternalNodes = TRUE)

   ds_spec <- xml_to_ds_spec(doc)
   ds_vars <- xml_to_ds_vars(doc)
   var_spec <- xml_to_var_spec(doc)
   value_spec <- xml_to_value_spec(doc)
   code_list <- xml_to_codelist(doc)
   derivations <- xml_to_derivations(doc)
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
xml_to_ds_spec <- function(doc) {
   # Read in the dataset level nodes
   ds_nodes <- get_ds_lvl_nodes(doc)
   # Name and structure are attributes of the node, but description is a child
   tibble(
      dataset = ds_nodes %>% get_node_attr("Name"),
      structure = ds_nodes %>% get_node_attr("Structure"),
      label = ds_nodes %>% map_chr(get_node_description)
   )
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


#' xml to value spec
#'
#' Takes a define xml and pulls out the value level metadata including codelist_id's,
#' defines_id's, and where clause. There is one row per variable expect when there
#' is a where clause, at which point there is one row per value.
#' @param doc ?
#'
#' @return tibble with the value level information
#' @family xml builder
#' @export
#'
xml_to_value_spec <- function(doc) {
   # Variable/value level node set
   var_nodes <- get_var_lvl_nodes(doc)
   # Gets the origin information for each node
   or_vec <- var_nodes %>%
      map_chr(function(node) {
         # gets the origin from the child
         origin <- get_child_attr(node, "Origin", "Type")
         if (!is.na(origin) && origin == "CRF") {
            # gets the page number from the origin's child
            page_num <- get_child_attr(node, "PDFPageRef", "PageRefs", TRUE) %>%
               str_replace_all("\\s", ", ") %>%
               str_replace_na(replacement = "")
            origin <- str_c(origin, page_num)
         }
         origin
      })


   # Get a vector of code_id
   # in each variable node there is a codelistRef, which has the value of the ID
   code_id_vec <- var_nodes %>%
      map_chr(~ get_child_attr(.x, "CodeListRef", "CodeListOID"))

   # Get variable/value information from each node
   var_info <- tibble(
      id = var_nodes %>% get_node_attr("OID"),
      type = var_nodes %>% get_node_attr("DataType"),
      origin = or_vec,
      code_id = code_id_vec
   ) %>%
      mutate(
         dataset = id_to_ds(id),
         variable = id_to_var(id)
      )

   # All the ID information which needs the ItemRef node set rather than ItemDef
   ref_nodes <- get_nodes(doc, path = "//ns:ItemRef")
   id_df <- tibble(
      derivation_id = ref_nodes %>% get_node_attr("MethodOID"),
      id = ref_nodes %>% get_node_attr("ItemOID"),
      where_id = ref_nodes %>%
         map_chr(~ get_child_attr(., "WhereClauseRef", "WhereClauseOID"))
   ) %>%
      distinct()

   # get where statements and add them to the id's
   id_df <- get_where(doc) %>%
      full_join(id_df, by = c("where_id")) %>%
      select(-.data$where_id)


   # Fill-in missing dataset information for some variables such as studyid
   full_var_ds <- ds_var_ls(doc)
   miss_ds <- var_info %>%
      filter(is.na(.data$dataset)) %>%
      select(-.data$dataset) %>%
      inner_join(full_var_ds, by = "variable")


   # Combining all the data
   all_data <- var_info %>%
      filter(!is.na(.data$dataset)) %>%
      bind_rows(miss_ds) %>%
      full_join(id_df, by = "id")

   # Remove duplicate information for variables that appear multiple times
   clean_data <- all_data %>%
      group_by(.data$variable) %>%
      mutate(
         remove = str_c("^.*", .data$variable),
         sub_cat = str_remove(id, remove),
         cat_test = !all(.data$sub_cat == ""), # T if there are sub categories
         rm_flg = (.data$cat_test & (.data$sub_cat == "")) & is.na(.data$code_id)
      ) %>% # T if is a sub cat var, and not a sub-cat
      filter(!.data$rm_flg) %>%
      select(-.data$remove, -.data$sub_cat, -.data$cat_test, -.data$rm_flg, -.data$id)
   clean_data %>%
      select(dataset, variable, everything()) %>%
      ungroup()
}


#' xml to code list
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
