library(tidyverse)
library(XML)



### Exported functions ---------------------------------------------------------
# Create the ds_spec
xml_to_ds_spec <- function(doc){
   item_grp <- get_nodes(doc, "//ns:ItemGroupDef")
   ds_name <- item_grp %>%
      map_chr(function(x){
         xmlGetAttr(x, "Name", default = NA)
      })
   ds_struct <- item_grp %>%
      map_chr(function(x){
         xmlGetAttr(x, "Structure", default = NA)
      })
   des_item_grp <- get_nodes(doc, path = "//ns:ItemGroupDef/ns:Description")

   ds_title <- des_item_grp %>%
      map_chr(function(x){
         xmlValue(x, "def:leaf/def:title")
      })
   tibble(dataset = ds_name,
          structure = ds_struct,
          label = ds_title)
}

# Create the ds_vars table
xml_to_ds_vars <- function(doc){
   item_grp <- get_nodes(doc, "//ns:ItemGroupDef")
   # Get a list of the datasets in the xml file
   ds_name <- item_grp %>%
      map_chr(function(x){
         xmlGetAttr(x, "Name", default = NA)
      })

   # Get the node for each dataset
   item_ref_node <- ds_name %>%
      map(~get_nodes(doc,
                     paste("//ns:ItemGroupDef[@Name ='", ., "']//ns:ItemRef", sep = ""))
      )

   # Creates ds with dataset variable, mandatory, and key columns
   map2_dfr(ds_name, item_ref_node, function(nm, item){
      var_info <- item %>%
         map(function(x){
            variable = xmlGetAttr(x, "ItemOID", default = NA)
            mandatory = xmlGetAttr(x, "Mandatory", default = NA)
            key_seq = xmlGetAttr(x, "KeySequence", default = NA)
            c(variable = variable,
              keep = mandatory == "Yes",
              key_seq = key_seq)
         }) %>%
         bind_rows()
      var_info %>%
         mutate(dataset = nm,
                variable = variable %>%
                   str_extract("\\.[:alnum:]*$") %>%
                   str_remove("[:punct:]"))
   })

}

# Create the var spec table
xml_to_var_spec <- function(doc){
   # Get infromation from the itemDef Nodes
   item_def <- get_nodes(doc, path = "//ns:ItemDef")
   var_info <- item_def %>%
      map(function(item){
         var_full = xmlGetAttr(item, "OID", default = NA)
         variable = xmlGetAttr(item, "Name", default = NA)
         length = xmlGetAttr(item, "Length", default = NA) %>%
            as.integer()
         list(var_full= var_full, variable=variable, length = length)
      }) %>%
      bind_rows()

   # Get infromation from the itemDef Description Nodes
   item_def_desc <- get_nodes(doc, path ="//ns:ItemDef/ns:Description")
   label <- item_def_desc %>%
      map_chr(function(item){
         xmlValue(item, "ns:TranslatedText[@xml:lang = \"en\"]")
      })

   possible_vars <- ds_var_ls(doc) %>%
      pull(variable) %>%
      unique()

   var_info %>%
      mutate(label = label) %>%
      filter(variable %in% possible_vars) %>%
      distinct(variable, length, label, .keep_all = TRUE) %>%
      group_by(variable) %>%
      mutate(n = n(),
             variable = if_else(n > 1, var_full, variable) %>%
                str_remove(., "^IT\\.")) %>%
      select(-n, -var_full)

}

xml_to_value_spec <- function(doc){
   # Variable level information
   item_def <- get_nodes(doc, path = "//ns:ItemDef")
   var_info <- item_def %>%
      map_dfr(function(item){
         id = xmlGetAttr(item, "OID", default = NA)
         type = xmlGetAttr(item, "DataType", default = NA)
         # Get the child node about the origin
         or_child <- xmlElementsByTagName(item, "Origin")
         if(length(or_child) > 0){
            origin <- xmlGetAttr(or_child[[1]], "Type", default = NA)
            if(origin == "CRF"){
               page_num <- xmlElementsByTagName(or_child[[1]], "PDFPageRef", TRUE)[[1]] %>%
                  xmlGetAttr("PageRefs", default = NA) %>%
                  str_replace_all("\\s", ", ")
               origin <- paste(origin, page_num)
            }
         } else {
            origin = NA
         }
         list(id= id, type = type, origin = origin)
      }) %>%
      bind_rows() %>%
      mutate(dataset = id %>%
                str_extract("(?<=^IT\\.)[:alnum:]+(?=\\..*)"),
             variable = id_to_var (id))

   # Get code list information
   code_list <- item_def %>%
      map_chr(function(node){
         code_ls_nodes <- xmlElementsByTagName(node, "CodeListRef")
         if(length(code_ls_nodes) > 0){
            xmlGetAttr(code_ls_nodes[[1]], "CodeListOID", default = NA)
         } else {
            NA
         }
      })

   var_info <- var_info %>%
      mutate(code_id = code_list)

   # Value level information
   ref_nodes <- get_nodes(doc, path = "//ns:ItemRef")
   value_ids <- ref_nodes %>%
      map_dfr(function(node){
         derivation_id <- xmlGetAttr(node, "MethodOID", default = NA)
         id <- xmlGetAttr(node, "ItemOID", default = NA)
         where_node <- xmlElementsByTagName(node, "WhereClauseRef")
         if(length(where_node) > 0){
            where_id <- xmlGetAttr(where_node[[1]], "WhereClauseOID", default = NA)
         } else {
            where_id <- NA
         }
         c(derivation_id = derivation_id, id = id, where_id = where_id)
      }) %>%
      distinct()


   # Get where list information
   where_nodes <- get_nodes(doc, path = "//def:WhereClauseDef")
   where_list <- where_nodes %>%
      map_dfr(function(node){
         where_id <- xmlGetAttr(node, "OID", default = NA)
         where_statement <- xmlElementsByTagName(node, "RangeCheck")
         if(length(where_statement) > 0){
            id <- xmlGetAttr(where_statement[[1]], "ItemOID", default = NA)
            operator <- xmlGetAttr(where_statement[[1]], "Comparator", default = NA)
            val <- xmlValue(where_statement[[1]])
         } else {
            id <- operator <- val <- NA
         }
         c(where_id = where_id, id = id, operator = operator, val = val)
      }) %>%
      mutate(var = id %>% str_extract("[:alnum:]*$"),
             where =  case_when(operator == "EQ" ~  paste0(var, " = '", val, "'"),
                                   TRUE ~ paste(var, operator, val))) %>%
      select(-id, -operator, -val, -var)
   value_ids <- value_ids %>%
      full_join(where_list, by = "where_id") %>%
      select(-where_id)


   # Remove duplicate infromation for variables that appear multiple times
   all_data <- full_join(var_info, value_ids, by = "id") %>%
      mutate(remove = str_c("^.*", variable),
             sub_cat = str_remove(id, remove)) %>%
      group_by(variable) %>%
      mutate(cat_test = !all(sub_cat == ""), # T if there are sub categories
             rm_flg = cat_test & (sub_cat == "")) %>%  # T if is a sub cat var, and not a sub-cat
      filter(!rm_flg) %>%
      select(-remove, -sub_cat, -cat_test, -rm_flg, -id)

   # Fill-in missing dataset information
   full_var_ds <- ds_var_ls(doc)
   miss_ds <- all_data %>%
      filter(is.na(dataset)) %>%
      select(-dataset) %>%
      inner_join(full_var_ds, by = "variable")
   all_data %>%
      filter(!is.na(dataset)) %>%
      bind_rows(miss_ds) %>%
      select(dataset, variable, everything())
}


xml_to_code_list <- function(doc){
   cl_nodes <- get_nodes(doc, "//ns:CodeList[ns:CodeListItem]")
   # List of each code group
   code_grps <- cl_nodes %>%
      map(function(node){
         code_id <- xmlGetAttr(node, "OID", default = NA)
         names <- xmlGetAttr(node,  "Name", default = NA)
         dataType <- xmlGetAttr(node, "DataType", default = NA)
         c(code_id = code_id, names = names, dataType = dataType)
      }) %>%
      bind_rows()

   codes <- code_grps %>%
      mutate(codes = code_id %>% map(get_codes, doc)) %>%
      unnest(codes)

   decodes <- get_nodes(doc, "//ns:Decode") %>%
      map_chr(function(node){
         xmlValue(node)
      })

   code_decode <- codes %>%
      mutate(decodes = decodes) %>%
      group_by(code_id) %>%
      mutate(type = "code_decode") %>%
      nest(codes = c(codes, decodes))


   # Permitted Values
   permitted <-  get_nodes(doc, "//ns:CodeList[ns:EnumeratedItem]") %>%
      map(function(node){
         code_id <- xmlGetAttr(node, "OID", default = NA)
         names <- xmlGetAttr(node,  "Name", default = NA)
         dataType <- xmlGetAttr(node, "DataType", default = NA)
         c(code_id = code_id, names = names, dataType = dataType)
      }) %>%
      bind_rows()
   permitted <- permitted %>%
      mutate(codes = code_id %>% map(get_permitted_vals, doc),
             type = "permitted_val")

   # Combinging the code decode with the permitted values
   bind_rows(code_decode, permitted) %>%
      select(-dataType)
}

# Get derivation table
xml_to_derivations <- function(doc){
   # Gets derivartion node
   method_nodes <- get_nodes(doc, path = "//ns:MethodDef")

   derivations <- method_nodes %>%
      map(function(node){
         derivation_id = xmlGetAttr(node, "OID", default = NA)
         derivation = xmlElementsByTagName(node, "Description") %>%
            xmlValue("ns:TranslatedText[@xml:lang = \"en\"]")
         c(derivation_id = derivation_id,
           derivation = derivation)
      }) %>%
      bind_rows()
}
### Helper functions -----------------------------------------------------------
get_nodes <- function(doc, path){
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces)[1] <- "ns"
   item_group <- getNodeSet(doc, path, namespaces)
}


get_codes <- function(id, doc){
   grp <- get_nodes(doc, str_c("//ns:CodeList[@OID=\"",
                               id, "\"]", "/ns:CodeListItem",
                               sep = ""))
   # Get codes
   codes <- grp %>%
      map_chr(function(node){
         code_value = xmlGetAttr(node, "CodedValue", default = NA)
      })

}

get_permitted_vals <- function(id, doc){
   grp <- get_nodes(doc, str_c("//ns:CodeList[@OID=\"",
                               id, "\"]", "/ns:EnumeratedItem", sep = ""))

   # Get values
   vals <- grp %>%
      map_chr(function(node){
         code_value = xmlGetAttr(node, "CodedValue", default = NA)
      })

}

id_to_var <- function(id){
   id %>%
      str_split("\\.") %>%
      map_chr(function(x){
         if(length(x) < 3){
            x[[2]]
         } else {
            x[[3]]
         }
      })
}

ds_var_ls <- function(doc){
   item_grp <- get_nodes(doc, "//ns:ItemGroupDef")
   # Get a list of the datasets and variables in the xml file
   var_ls <- item_grp %>%
      map(function(x){
         dataset <- xmlGetAttr(x, "Domain")
         vars <- xmlElementsByTagName(x, "ItemRef") %>%
            map_chr(function(node){
               xmlGetAttr(node, "ItemOID", default = NA)
            })
         tibble(dataset= dataset, variable = vars)
      }) %>%
      bind_rows() %>%
      mutate(remove = str_c("^", dataset, "\\."),
             variable = str_remove(variable, "^IT\\.") %>%
                str_remove(remove)) %>%
      select(-remove)
   var_ls
}
