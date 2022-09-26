library(xml2)
library(tidyverse)
path <- "/Users/christinafillmore/Documents/GitHub/metacore/dev/define_examples/defineV21-ADaM.xml"
path <- "/Users/christinafillmore/Documents/GitHub/metacore/inst/extdata/ADaM_define.xml"
# path <- "/Users/christina/Dropbox/Mac/Downloads/adam_specs_whereclauses/define-cwc.xml"
# path <- metacore_example("SDTM_define.xml")
path <- "/Users/christinafillmore/Documents/GitHub/metacore/dev/define_examples/defineV21-SDTM.xml"
path <- "/Users/christinafillmore/Downloads/ARM-for-Define-XML/adam/define2-0-0-example-adam-results.xml"


xml <- read_xml(path)
xml_ns_strip(xml)


var_info <- xml_find_all(xml, "//ItemDef") %>%
   map_dfr(function(node){
      data.frame(
         oid = xml_attr(node,"OID"),
         variable = xml_attr(node, "Name"),
         type = xml_attr(node, "DataType"),
         length = xml_attr(node, "Length") %>% as.integer(),
         format = xml_attr(node, "DisplayFormat"),
         label =  xml_find_first(node, "./Description/TranslatedText") %>%
            xml_text()
      )
   })


possible_values <- xml_find_all(xml, "//ItemRef") %>%
   map_chr(function(node){
         oid = xml_attr(node, "ItemOID")
   })

var_info %>%
   filter(.data$oid %in% possible_values)


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
      origin = if_else(origin == "Collected" & !is.na(page_num), paste0(origin,", page_num = ", page_num), origin)
   ) %>%
   select(-page_num)

# Pull the information from the item reference
derivations <- xml_find_all(xml, "//ItemRef") %>%
   map_dfr(function(node){
      tibble(
         oid = xml_attr(node, "ItemOID"),
         derivation_id_all = xml_attr(node, "MethodOID")
      )
   })


item_info <- left_join(item_def, derivations, by = "oid")

where_to_merge <- xml_find_all(xml, "//def:ValueListDef/ItemRef") %>%
   map_dfr(function(node){
      data.frame(
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
      mutate(item_oid = left,
             derivation_id = paste0("MT", str_remove(left, "IT"), ".", right))
} else{
   where_eqs<- full_join(where_to_merge, where_eqs, by = "where_oid")
}

all_where_eqs <- where_eqs  %>%
   group_by(where_oid) %>%
   mutate(var = str_extract(left, "\\w*$"),
          right = paste0("'", right, "'"),
          test = case_when(test == "EQ" ~ "==",
                           test == "LT" ~ "<",
                           test == "LE" ~ "<=",
                           test == "GT" ~ ">",
                           test == "GE" ~ ">=",
                           test == "NE" ~ "!=",
                           TRUE ~ test),
          eq = case_when( test == "IN" ~ paste(var, "%in%", "c(",
                                               paste(right, collapse = ","),
                                               ")"),
                          test == "NOTIN" ~ paste("!", var, "%in%", "c(",
                                                  paste(right, collapse = ","),
                                                  ")"),
                          TRUE ~ paste(var, test, right, collapse = " & "))
   ) %>%
   select(-left, -var, -test, -right) %>%
   distinct() %>%
   group_by(item_oid, derivation_id) %>%
   mutate(full_eq = str_c(eq, collapse = "||")) %>%
   filter(!is.na(item_oid)) %>%
   ungroup() %>%
   select(item_oid, where = full_eq, derivation_id)


val_spec <- item_info %>%
   left_join(all_where_eqs, by = c("oid" = "item_oid")) %>%
   mutate(derivation_id = case_when(
      origin == "Predecessor" & !is.na(predecessor) ~ predecessor,
      origin == "Assigned" & !is.na(comment_id) ~ comment_id,
      !is.na(derivation_id_all) ~ derivation_id_all,
      TRUE ~ derivation_id)) %>%
   select(-predecessor, -comment_id, -derivation_id_all)


if(define_version >= as.numeric_version("2.1.0")){
   where_to_merge <- xml_find_all(xml, "//def:ValueListDef/ItemRef") %>%
      map_dfr(function(node){
         data.frame(
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

   where_eqs<- full_join(where_to_merge, where_eqs, by = "where_oid")
} else {
   where_eqs <- xml_find_all(xml, "//def:WhereClauseDef[@OID]/RangeCheck") %>%
      map_dfr(function(node){
         tibble(
            where_oid = xml_parent(node) %>% xml_attr("OID"),
            left = xml_attr(node, "ItemOID"),
            test = xml_attr(node, "Comparator"),
            right = xml_find_all(node, "./CheckValue") %>% xml_text()
         )
      }
      ) %>%
      mutate(item_oid = left,
             derivation_id = paste0("MT", str_remove(left, "IT"), ".", right))
}


all_where_eqs <- where_eqs  %>%
   group_by(where_oid) %>%
   mutate(var = str_extract(left, "\\w*$"),
          right = paste0("'", right, "'"),
          test = case_when(test == "EQ" ~ "==",
                           test == "LT" ~ "<",
                           test == "LE" ~ "<=",
                           test == "GT" ~ ">",
                           test == "GE" ~ ">=",
                           test == "NE" ~ "!=",
                           TRUE ~ test),
          eq = case_when( test == "IN" ~ paste(var, "%in%", "c(",
                                               paste(right, collapse = ","),
                                               ")"),
                          test == "NOTIN" ~ paste("!", var, "%in%", "c(",
                                                  paste(right, collapse = ","),
                                                  ")"),
                          TRUE ~ paste(var, test, right, collapse = " & "))
   ) %>%
   select(-left, -var, -test, -right) %>%
   distinct() %>%
   group_by(item_oid, derivation_id) %>%
   mutate(full_eq = str_c(eq, collapse = "||")) %>%
   filter(!is.na(item_oid)) %>%
   ungroup() %>%
   select(item_oid, where = full_eq, derivation_id)


val_spec <- val_spec %>%
   left_join(all_where_eqs, by = c("oid" = "item_oid")) %>%
   mutate(derivation_id = case_when(
      origin == "Predecessor" & !is.na(predecessor) ~ predecessor,
      origin == "Assigned" & !is.na(comment_id) ~ comment_id,
      TRUE ~ derivation_id)) %>%
   select(-predecessor, -comment_id)







map_dfr(xml_find_all(xml, "//MetaDataVersion/ItemGroupDef[contains(@OID, 'IG')]"), function(nn) {
   map_dfr(xml_find_all(nn, "./ItemRef[contains(@ItemOID, 'IT')]"), function(n) {
      data.frame(
         itemoid = xml_attr(n, "ItemOID"),
         domain = xml_attr(n, "ItemOID") %>% str_extract("\\.\\w+\\.") %>% str_remove_all("\\."),
         variable = xml_attr(n, "ItemOID") %>% str_extract("\\.\\w+$") %>% str_remove_all("\\."),
         ordernumber = xml_attr(n, "OrderNumber"),
         mandatory = xml_attr(n, "Mandatory"),
         role = xml_attr(n, "Role")
      )
   })
})




