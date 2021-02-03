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
