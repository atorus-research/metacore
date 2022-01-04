#' get nodes
#'
#' This function gets a set of nodes from the document basedon the x-path given
#'
#' @param doc xml document to look through
#' @param path string of xpath the get the specified nodes
#'
#' @return nodeSet
#' @noRd
get_nodes <- function(doc, path) {
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces) <- names(namespaces) %>%
      if_else(str_length(.) == 0, "ns", .)
   item_group <- getNodeSet(doc, path, namespaces)
}


#' get_ds_lvl_nodes
#'
#' Given a document this function returns the nodes which are at the dataset level
#' @param doc xml document
#'
#' @return nodeSet for the datasets
#' @noRd
get_ds_lvl_nodes <- function(doc) {
   get_nodes(doc, "//ns:ItemGroupDef")
}


#' get_var_lvl_nodes
#'
#' Given a document this function returns the variable level nodes
#' @param doc xml document
#'
#' @return nodeSet for the variables
#' @noRd
get_var_lvl_nodes <- function(doc) {
   get_nodes(doc, path = "//ns:ItemDef")
}

#' get node attribute
#'
#' @param nodeSet xml node set
#' @param attrtibute string of the attribute
#'
#' @return character vector of the attribute searched
#' @noRd
get_node_attr <- function(nodeSet, attrtibute) {
   out <- nodeSet %>%
      map_chr(~ xmlGetAttr(., attrtibute, default = NA))
   attr(out, "names") <- NULL
   out
}


#' Get the attribute of a child
#'
#' Gets the attribute of a child. This only works if their is only one child
#' @param node current node
#' @param child tag of the child node
#' @param attribute attribute for that child
#'
#' @return value of the child attribute
#' @noRd
get_child_attr <- function(node, child, attribute, ...) {
   child_node <- xmlElementsByTagName(node, child, ...)
   if (length(child_node) == 1) {
      value <- xmlGetAttr(child_node[[1]], attribute, default = NA)
   } else {
      value <- NA
   }
   value
}


#' get node description
#'
#' @param node xml node that has a description tag
#'
#' @return string, description
#' @noRd
get_node_description <- function(node) {
   out <- xmlElementsByTagName(node, "Description") %>%
      xmlValue("ns:TranslatedText[@xml:lang = \"en\"]")
   if(length(out) == 0){
      out <- NA
   }
   out
}


#' Get the where_id to statement decode
#'
#' @param doc xml document
#'
#' @return data frame with a where_id column and a where column
#' @noRd
get_where <- function(doc) {
   # Gets the where caluse node set
   where_nodes <- get_nodes(doc, path = "//def:WhereClauseDef")
   where_list <- where_nodes %>%
      map_dfr(function(node) {
         where_id <- xmlGetAttr(node, "OID", default = NA)
         # gets the child of the where_id, rangeCheck which contains the where statement
         where_statement <- xmlElementsByTagName(node, "RangeCheck")
         if (length(where_statement) > 0) {
            id <- xmlGetAttr(where_statement[[1]], "ItemOID", default = NA)
            operator <- xmlGetAttr(where_statement[[1]], "Comparator", default = NA)
            val <- xmlValue(where_statement[[1]])
         } else {
            id <- operator <- val <- NA
         }
         c(where_id = where_id, id = id, operator = operator, val = val)
      }) %>%
      mutate(
         var = id %>% str_extract("[:alnum:]*$"),
         where = case_when(
            .data$operator == "EQ" ~ paste0(.data$var, " = '",.data$val, "'"),
            TRUE ~ paste(.data$var, .data$operator, .data$val)
         )
      ) %>%
      select(-id, -.data$operator, -.data$val, -.data$var)
   where_list
}

#' id to dataset
#'
#' @param id vector of id's
#'
#' @return vector of datasets
#' @noRd
id_to_ds <- function(id) {
   id %>% str_extract("(?<=^IT\\.)[:alnum:]+(?=\\..*)")
}

#' id to variable
#'
#' @param id vector of id's
#' @param ds vector of ds's
#'
#' @return vector of variable names
#' @noRd
id_to_var <- function(id) {
   ds <- id %>% str_extract("(?<=^IT\\.)[:alnum:]+(?=\\..*)")
   extract <- if_else(is.na(ds), "(?<=^IT\\.)[:alnum:]*",
                      str_c("(?<=^IT\\.", ds, "\\.)[:alnum:]*")
   )
   id %>%
      str_extract(extract)
}

#' Get codes
#'
#' @param cl_nodes codelist nodes, got using the following xpath "CodeList[ns:CodeListItem]"
#'
#' @return list of codes and decodes
#' @noRd
get_codes <- function(cl_nodes) {
   cl_nodes %>%
      map(function(node){
         # Gets each code value node in the overall code group node
         cl_vals <- node %>% xmlElementsByTagName("CodeListItem")
         cl_vals %>%
            xmlToDataFrame(stringsAsFactors = FALSE) %>% #converting to data frames gets the decodes
            #gets the codes from the list attributes
            mutate(code = cl_vals %>% get_node_attr("CodedValue")) %>%
            select(.data$code, decode = .data$Decode) %>%
            as_tibble()
      })
}

#' Get permitted values
#'
#' @param permitted_nodes permitted value nodes
#'
#' @return list of permitted values
#' @noRd
get_permitted_vals <- function(permitted_nodes) {
   permitted_nodes %>%
      map(function(node){
         node %>%
            xmlElementsByTagName("EnumeratedItem") %>%
            get_node_attr("CodedValue")
      })
}

#' Dataset Variable List
#'
#' @param doc
#'
#' @return a dataframe of the variables and their corresponding datasets
#' @noRd
ds_var_ls <- function(doc) {
   # Get the name of each dataset
   dataset_nodes <- get_ds_lvl_nodes(doc)

   var_ls <- dataset_nodes %>%
      map_dfr(function(x) {
         # Gets the name attribute of the dataset node
         dataset <- xmlGetAttr(x, "Name")
         # Gets the child node, Item Ref, and gets the OID
         vars <- xmlElementsByTagName(x, "ItemRef") %>%
            get_node_attr("ItemOID")
         tibble(dataset = dataset, variable = vars)
      }) %>%
      mutate(variable = id_to_var(.data$variable))
   var_ls
}
