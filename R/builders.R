library(tidyverse)
library(rbenchmark)
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

   var_info %>%
      mutate(label = label) %>%
      distinct(variable, length, label, .keep_all = TRUE) %>%
      group_by(variable) %>%
      mutate(n = n(),
             variable = if_else(n > 1, var_full, variable) %>%
                str_remove(., "^IT\\.")) %>%
      select(-n, -var_full)
}

xml_to_value_spec <- function(doc){
   item_def <- get_nodes(doc, path = "//ns:ItemDef")
   var_info <- item_def %>%
      map(function(item){
         dataset = xmlGetAttr(item, "OID", default = NA)
         variable = xmlGetAttr(item, "Name", default = NA)
         type = xmlGetAttr(item, "DataType", default = NA)
         list(dataset= dataset, variable=variable, type = type)
      }) %>%
      bind_rows() %>%
      mutate(dataset = dataset %>%
                str_extract("(?=\\.).*(?<=\\.)") %>%
                str_remove_all("[:punct:]"))
   # Get namespace for origin and derivation ID
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces)[1] <- "ns"
   # Get code list information
   code_list <- item_def %>%
      map_chr(function(x){
         code_ls_nodes <- getNodeSet(x, "./ns:CodeListRef", namespaces)
         if(length(code_ls_nodes) > 0){
            xmlGetAttr(code_ls_nodes[[1]], "CodeListOID", default = NA)
         } else {
            NA
         }
      })

   origin_list <- item_def %>%
      map_chr(function(x){
         origin_ls_nodes <- getNodeSet(x, "./def:Origin", namespaces)
         if(length(origin_ls_nodes) > 0){
            type <- xmlGetAttr(origin_ls_nodes[[1]], "Type", default = NA)
            if(type == "CRF"){
               page_num <- getNodeSet(origin_ls_nodes[[1]],
                                      "./def:DocumentRef/def:PDFPageRef",
                                      namespaces)%>%
                  .[[1]] %>%
                  xmlGetAttr("PageRefs") %>%
                  str_replace_all("\\s", ", ")
               type<- paste(type, page_num)
            }
            type
         } else {
            NA_character_
         }
      })
   var_info %>%
      mutate(code_id = code_list,
             origin = origin_list)
   # TODO missing where and derivation id

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

   code_grps %>%
      mutate(codes = map(code_id, get_code_decode, doc))


}


### Helper functions -----------------------------------------------------------
get_nodes <- function(doc, path){
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces)[1] <- "ns"
   item_group <- getNodeSet(doc, path, namespaces)
}


get_code_decode <- function(id, doc){
   grp <- get_nodes(doc, str_c("//ns:CodeList[@OID=\"",
                               id, "\"]", "/ns:CodeListItem",
                               sep = ""))
   # Get codes
   codes <- grp %>%
      map_chr(function(node){
         code_value = xmlGetAttr(node, "CodedValue", default = NA)
      })

   # Get decodes
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces)[1] <- "ns"

   decod_node_loc <- str_c("//ns:CodeList[@OID=\"",
                           id, "\"]/ns:CodeListItem[", seq(1:length(codes)),
                           "]", "/ns:Decode", sep = "")


   decodes = decod_node_loc %>%
      map(~getNodeSet(doc, ., namespaces)) %>%
      map_chr(function(node){
         xmlValue(node)
      })
   tibble(codes = codes, decodes = decodes)

}

# TODO add a normalization function in the initializer
