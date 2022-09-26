library(xml2)
library(tidyverse)
path <- "/Users/christinafillmore/Documents/GitHub/metacore/dev/define_examples/defineV21-ADaM.xml"
# path <- "/Users/christina/Dropbox/Mac/Downloads/adam_specs_whereclauses/define-cwc.xml"
# path <- metacore_example("SDTM_define.xml")
path <- "/Users/christinafillmore/Documents/GitHub/metacore/dev/define_examples/defineV21-SDTM.xml"


xml <- read_xml(path)
xml_ns_strip(xml)


# All values without the where issue
val_spec <- xml_find_all(xml, "//ItemDef") %>%
   map_dfr(function(node){
      data.frame(
         oid = xml_attr(node,"OID"),
         name = xml_attr(node, "Name"),
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
      data.frame(
         where_oid = xml_parent(node) %>% xml_attr("OID"),
         left = xml_attr(node, "ItemOID"),
         test = xml_attr(node, "Comparator"),
         right = xml_find_all(node, "./CheckValue") %>% xml_text()
      )
   }
   ) %>%
   group_by(where_oid) %>%
   mutate(var = str_extract(left, "\\w*$"),
          test = case_when(test == "EQ" ~ "=="), #Update from the other thing
          eq = paste(var, test, right, collapse = " & ")) %>%
   select(-left, -var, -test, -right) %>%
   distinct()


full_eq_where <- full_join(where_to_merge, where_eqs, by = "where_oid") %>%
   group_by(item_oid) %>%
   mutate(full_eq = str_c(eq, collapse = "||")) %>%
   filter(!is.na(oid)) %>%
   ungroup() %>%
   select(item_oid, where = full_eq)

val_spec <- val_spec %>%
   left_join(full_eq_where, by = c("oid" = "item_oid"))



dataset_spec <- map_dfr(xml_find_all(xml, "//ItemGroupDef[contains(@OID, 'IG')]"), function(n) {
   data.frame(
      oid = xml_attr(n, "OID"),
      domain = xml_attr(n, "Domain"),
      name = xml_attr(n, "Name"),
      desc = xml_find_first(n, "./Description") %>% xml_text(),
      purpose = xml_attr(n, "Purpose"),
      struc = xml_attr(n, "Structure")
   )
})





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
