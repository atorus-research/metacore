
#' XML to Data Set Spec
#'
#' Creates a dataset specification, which has the domain name and label for each dataset
#' @param doc xml document
#'
#' @return data frame with the data set specifications
#' @family xml builder
#' @export
xml_to_ds_spec <- function(doc) {
   # Read in the dataset levle nodes
   ds_nodes <- get_ds_lvl_nodes(doc)
   # Name and structure are attributes of the node, but decribtion is a child
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
         dataset <- xmlGetAttr(x, "Domain")
         # Gets the child node, Item Ref, which contains variable level information
         child_var_nodes <- xmlElementsByTagName(x, "ItemRef")
         # Pulls the relevant information from child node
         tibble(
            dataset = dataset,
            variable = child_var_nodes %>% get_node_attr("ItemOID"),
            mandatory = child_var_nodes %>% get_node_attr("Mandatory"),
            key_seq = child_var_nodes %>% get_node_attr("KeySequence")
         )
      }) %>%
      mutate(
         variable = id_to_var(variable),
         keep = mandatory == "Yes"
      ) %>%
      select(-mandatory)
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
      length = var_nodes %>% get_node_attr("Length") %>%
         as.integer(),
      # Get labels
      label = var_nodes %>% map_chr(get_node_description)
   )

   possible_vars <- ds_var_ls(doc) %>%
      pull(variable) %>%
      unique()

   var_info %>%
      filter(variable %in% possible_vars) %>%
      distinct(variable, length, label, .keep_all = TRUE) %>%
      group_by(variable) %>%
      mutate(
         n = n(),
         variable = if_else(n > 1, var_full, variable) %>%
            str_remove(., "^IT\\.")
      ) %>%
      select(-n, -var_full)
}


#' xml to value spec
#'
#' Takes a define xml and pulls out the value level metadata including codelist_id's,
#' defines_id's, and where claus. There is one row per variable expect when there
#' is a where, at which point there is one row per value. {this is bad english please help}
#' @param doc
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
               str_replace_all("\\s", ", ")
            origin <- paste(origin, page_num)
         }
         origin
      })


   # Get a vector of code_id
   # in each variable node there is a codelistRef, which has the value of the ID
   code_id_vec <- var_nodes %>%
      map_chr(~ get_child_attr(., "CodeListRef", "CodeListOID"))

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
      select(-where_id)


   # Fill-in missing dataset information for some variables such as studyid
   full_var_ds <- ds_var_ls(doc)
   miss_ds <- var_info %>%
      filter(is.na(dataset)) %>%
      select(-dataset) %>%
      inner_join(full_var_ds, by = "variable")


   # Combining all the data
   all_data <- var_info %>%
      bind_rows(miss_ds) %>%
      full_join(id_df, by = "id")

   # Remove duplicate information for variables that appear multiple times
   clean_data <- all_data %>%
      group_by(variable) %>%
      mutate(
         remove = str_c("^.*", variable),
         sub_cat = str_remove(id, remove),
         cat_test = !all(sub_cat == ""), # T if there are sub categories
         rm_flg = cat_test & (sub_cat == "")
      ) %>% # T if is a sub cat var, and not a sub-cat
      filter(!rm_flg) %>%
      select(-remove, -sub_cat, -cat_test, -rm_flg, -id)
   clean_data
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
xml_to_code_list <- function(doc) {
   cl_nodes <- get_nodes(doc, "//ns:CodeList[ns:CodeListItem]")
   # Get a table with the information about the code list
   # Done like this because map_chr is faster than map_dfr
   code_grps <- tibble(
      code_id = cl_nodes %>% get_node_attr("OID"),
      names = cl_nodes %>% get_node_attr("Name"),
      dataType = cl_nodes %>% get_node_attr("DataType")
   )

   # get the code for each code_id and unnest to match with the decodes
   codes <- code_grps %>%
      mutate(codes = code_id %>% map(get_codes, doc)) %>%
      unnest(codes)

   # gets a vector of all the decodes
   decodes <- get_nodes(doc, "//ns:Decode") %>%
      map_chr(function(node) {
         xmlValue(node)
      })

   # combines codes and decodes then renests
   code_decode <- codes %>%
      mutate(decodes = decodes) %>%
      group_by(code_id) %>%
      mutate(type = "code_decode") %>%
      nest(codes = c(codes, decodes))


   # Permitted Values
   # following the same method as above, get permitted value information
   permitted_nodes <- get_nodes(doc, "//ns:CodeList[ns:EnumeratedItem]")
   permitted <- tibble(
      code_id = permitted_nodes %>% get_node_attr("OID"),
      names = permitted_nodes %>% get_node_attr("Name"),
      dataType = permitted_nodes %>% get_node_attr("DataType")
   )

   permitted <- permitted %>%
      mutate(
         codes = code_id %>% map(get_permitted_vals, doc),
         type = "permitted_val"
      )

   # Combinging the code decode with the permitted values
   bind_rows(code_decode, permitted) %>%
      select(-dataType)
}



#' XML to derivation table
#'
#' This reads in a xml document and gets all the derivations/comments. These can
#' be cross referenced to variables using the derivation_id's
#' @param doc
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
