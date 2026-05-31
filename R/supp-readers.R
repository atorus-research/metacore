# For Define-XML
xml_to_supp <- function(doc) {
   # Find all ItemGroupDef nodes that start with SUPP
   xml_all_supp <- xml_find_all(doc, "//ItemGroupDef[starts-with(@Name, 'SUPP')]")

   supp_ds <- map(xml_all_supp, function(node) {
         supp_dataset_name <- xml_attr(node, "Name")

         # Find all QNAM values from the codelist
         qnam_item <- xml_find_first(node, ".//ItemRef[contains(@ItemOID, 'QNAM')]")
         qnam_oid <- xml_attr(qnam_item, "ItemOID")
         qnam_def <- xml_find_first(doc, sprintf("//ItemDef[@OID='%s']", qnam_oid))
         qnam_codelist_oid <- xml_find_first(qnam_def, "./CodeListRef") |> xml_attr("CodeListOID")
         qnam_value <- xml_find_first(doc, sprintf("//CodeList[@OID='%s']", qnam_codelist_oid)) |>
            xml_find_all(".//CodeListItem") |>
            xml_attr("CodedValue")

         # Get IDVAR comment to find identifying variable
         idvar_item <- xml_find_first(node, ".//ItemRef[contains(@ItemOID, 'IDVAR')]")
         idvar_oid <- xml_attr(idvar_item, "ItemOID")
         idvar_def <- xml_find_first(doc, sprintf("//ItemDef[@OID='%s']", idvar_oid))
         idvar_com_oid <- xml_attr(idvar_def, "CommentOID")
         idvar_value <- xml_find_all(doc, sprintf("//def:CommentDef[@OID='%s']/Description/TranslatedText", idvar_com_oid)) |>
            xml_text() |>
            str_extract('(?<==")[A-Z0-9]+')

         # Get QEVAL if it exists
         qeval_item <- xml_find_first(node, ".//ItemRef[contains(@ItemOID, 'QEVAL')]")
         qeval_oid <- xml_attr(qeval_item, "ItemOID")
         qeval_def <- xml_find_first(doc, sprintf("//ItemDef[@OID='%s']", qeval_oid))
         qeval_codelist_oid <- xml_find_first(qeval_def, "./CodeListRef") |> xml_attr("CodeListOID")
         qeval_value <- xml_find_first(doc, sprintf("//CodeList[@OID='%s']", qeval_codelist_oid)) |>
            xml_find_all(".//CodeListItem") |>
            xml_attr("CodedValue")

         # Create rows for each QNAM value
         tibble(
            dataset = supp_dataset_name,
            variable = qnam_value,
            idvar = idvar_value,
            qeval = qeval_value
         )
      }) |>
      purrr::list_rbind(
         ptype = tibble(
            dataset = character(),
            variable = character(),
            idvar = character(),
            qeval = character()
         )
      )

   supp_ds
}
