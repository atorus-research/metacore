library(safetyData)
library(dplyr)
library(admiral)
library(metatools)

#Read from XML
doc <- xmlTreeParse(metacore_example("ADaM_define.xml"), useInternalNodes = TRUE)
ds_spec <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)
var_spec <- xml_to_var_spec(doc)
value_spec <- xml_to_value_spec(doc)
code_list <- xml_to_codelist(doc)
derivations <- xml_to_derivations(doc)


adsl_preds <- build_from_derived(metacore, list("dm" = sdtm_dm), predecessor_only = FALSE, keep = TRUE, "ADSL")
# Pulling ct for DISCREAS from the cdisc pilot data
new_ct <- adsl_preds %>%
   derive_disposition_reason(
      dataset_ds = sdtm_ds,
      new_var = DCSREAS,
      reason_var = DSDECOD,
      filter = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
   ) %>%
   pull(DCSREAS) %>%
   unique() %>%
   purrr::discard(is.na) %>%
   tibble(code = ., decode = .)
code_list2 <- code_list %>%
   mutate(codes = if_else(name == "DISCREAS", list(new_ct), codes))

metacore <- metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, code_list2)

save(metacore, file = "inst/extdata/pilot_ADaM.rda")


### Create an SDTM sample file
doc <- xmlTreeParse(metacore_example("SDTM_define.xml"), useInternalNodes = TRUE)
ds_spec <- xml_to_ds_spec(doc)
ds_vars <- xml_to_ds_vars(doc)
var_spec <- xml_to_var_spec(doc)
value_spec <- xml_to_value_spec(doc)
code_list <- xml_to_codelist(doc)
derivations <- xml_to_derivations(doc)
supp <- tibble(dataset = character(), variable = character(), idvar = character(), qeval = character())

datasets <- c("sdtm_suppae", "sdtm_suppdm",
              "sdtm_suppds", "sdtm_supplb")
# i <- datasets[1]
for(i in datasets){
   dat <- get(i)

   ds_vars <- dat %>%
      distinct(dataset= RDOMAIN, variable = QNAM) %>%
      mutate(supp_flag = TRUE) %>%
      bind_rows(ds_vars, .)

   var_spec <- dat %>%
      distinct(variable = QNAM, label = QLABEL) %>%
      mutate(type = "text", length = 20) %>%
      bind_rows(var_spec, . )

   value_spec <- dat %>%
      distinct(dataset= RDOMAIN, variable = QNAM, origin = QORIG) %>%
      bind_rows(value_spec, . )
   supp <- dat %>%
      distinct(dataset= RDOMAIN, variable = QNAM, idvar = IDVAR, qeval = QEVAL) %>%
      bind_rows(supp, .)
}


metacore <- metacore(ds_spec, ds_vars, var_spec, value_spec, derivations, code_list, supp)

save(metacore, file = "inst/extdata/pilot_SDTM.rda")
