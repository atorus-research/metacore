library(tidyverse)
library(R4DSXML)
library(rbenchmark)

path <- "C:/Users/cf124952/ATorus/GSK Atorus Open Source Collaboration - Metadata/GSK_SDTM_defines/mid201584/define.xml"

# Read in the file
doc <- xmlTreeParse(path, useInternalNodes = TRUE)


ds_spec <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)

item_def <- R4DSXML:::getItemDef(doc)




function (doc)
{
   namespaces <- R4DSXML:::namespaces(doc)
   ItemDefNode <- getNodeSet(doc, "//ns:ItemDef", namespaces)
   defVersion <- tail(str_split(namespaces[["def"]], "/", n = Inf,
                                simplify = FALSE)[[1]], n = 1)
   ID_OID <- getAttr(Nodeset = ItemDefNode, Attr = "OID")
   ID_Name <- getAttr(Nodeset = ItemDefNode, Attr = "Name")
   ID_DataType <- getAttr(Nodeset = ItemDefNode, Attr = "DataType")
   ID_Length <- as.integer(getAttr(Nodeset = ItemDefNode, Attr = "Length"))
   ID_SignificantDigits <- as.integer(getAttr(Nodeset = ItemDefNode,
                                              Attr = "SignificantDigits"))
   ID_SASFieldName <- getAttr(Nodeset = ItemDefNode, Attr = "SASFieldName")
   if (defVersion == "v2.1") {
      ID_DisplayFormat <- getAttr(Nodeset = ItemDefNode, Attr = "def:DisplayFormat")
   }
   else {
      ID_SASFormatName <- getAttr(Nodeset = ItemDefNode, Attr = "ns:SASFormatName")
   }
   ItemDefNode2 <- getNodeSet(doc, "//ns:ItemDef/ns:Description",
                              namespaces)
   ID_Label <- R4DSXML:::getVal(ItemDefNode2, "ns:TranslatedText[@xml:lang = \"en\"]")
   ID_CodeListOID <- getCodeListRef(ItemDefNode, namespaces)
   originList <- getOrigin(ItemDefNode, namespaces)
   ID_OriginType <- originList[[1]]
   ID_OriginDescription <- originList[[2]]
   ID_ValueListOID <- getValueListRef(ItemDefNode, namespaces)
   if (defVersion == "v2.1") {
      df <- data.frame(ID_OID, ID_Name, ID_Length, ID_SignificantDigits,
                       ID_DataType, ID_Label, ID_SASFieldName, ID_DisplayFormat,
                       ID_CodeListOID, ID_OriginType, ID_OriginDescription,
                       ID_ValueListOID, stringsAsFactors = FALSE)
   }
   else {
      df <- data.frame(ID_OID, ID_Name, ID_Length, ID_SignificantDigits,
                       ID_DataType, ID_Label, ID_SASFieldName, ID_SASFormatName,
                       ID_CodeListOID, ID_OriginType, ID_OriginDescription,
                       ID_ValueListOID, stringsAsFactors = FALSE)
   }
}









control_term <- getCT(path)

ds_meta <- getDLMD(path)

val_meta <- getValMD(path)


var_meta <- getVarMD(path)



### Exported functions ---------------------------------------------------------
# Create the ds_spec
xml_to_ds_spec <- function(doc){
   item_grp <- get_nodes(doc, "//ns:ItemGroupDef")
   ds_name <- item_group %>%
      map_chr(function(x){
         xmlGetAttr(x, "Name", default = NA)
      })
   ds_struct <- item_group %>%
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
   ds_name <- item_group %>%
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
            variable = xmlGetAttr(x, "ItemOID", default = NA) %>%
               str_extract("(?=\\.).*$") %>%
               str_remove("[:punct:]")
            mandatory = xmlGetAttr(x, "Mandatory", default = NA)
            key_seq = xmlGetAttr(x, "KeySequence", default = NA)
            c(variable = variable,
              keep = mandatory == "Yes",
              key_seq = key_seq)
         }) %>%
         bind_rows()

      var_info %>%
         mutate(dataset = nm)
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
             variable = if_else(n > 1, var_full, variable)) %>%
      select(-n, -var_full)
}

### Helper functions -----------------------------------------------------------
get_nodes <- function(doc, path){
   namespaces <- xmlNamespaceDefinitions(doc, simplify = TRUE)
   names(namespaces)[1] <- "ns"
   item_group <- getNodeSet(doc, path, namespaces)
}
