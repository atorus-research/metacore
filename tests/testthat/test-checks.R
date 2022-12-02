test_that("Test label Checks", {
   load(metacore_example("pilot_ADaM.rda"))
   man_label <- tibble::tribble(
      ~variable, ~label,  ~n_vars,  ~ls_of_vars,
      "ABLFL"  , "ABLFL"                               ,     1L, "ADADAS.ABLFL",
      "ABLFL"  , "Analysis Baseline Flag"              ,     1L, "ADNPIX.ABLFL" ,
      "ABLFL"  , "Baseline Record Flag"                ,     6L, c("ADLBC.ABLFL", "ADLBCPV.ABLFL", "ADLBH.ABLFL", "ADLBHPV.ABLFL", "ADLBHY.ABLFL", "ADVS.ABLFL"),
      "ANL01FL", "Analysis Flag 01"                    ,     5L, c("ADLBC.ANL01FL", "ADLBCPV.ANL01FL", "ADLBH.ANL01FL", "ADLBHPV.ANL01FL", "ADVS.ANL01FL"),
      "ANL01FL", "Analysis Record Flag 01"             ,     3L, c("ADADAS.ANL01FL", "ADCIBC.ANL01FL", "ADNPIX.ANL01FL"),
      "CHG"    , "Baseline Value"                      ,     1L, "ADADAS.CHG",
      "CHG"    , "Change from Baseline"                ,     6L, c("ADLBC.CHG", "ADLBCPV.CHG", "ADLBH.CHG", "ADLBHPV.CHG", "ADNPIX.CHG", "ADVS.CHG"),
      "ITTFL"  , "Intent-to-Treat Population Flag"     ,     3L, c("ADADAS.ITTFL", "ADCIBC.ITTFL", "ADNPIX.ITTFL"),
      "ITTFL"  , "Intent-To-Treat Population Flag"     ,     1L, "ADSL.ITTFL"
   ) %>%
      dplyr::arrange(variable, label)
   label_df <- check_inconsistent_labels(metacore) %>%
      dplyr::arrange(variable, label)
   expect_equal(label_df, man_label)

   load(metacore_example("pilot_SDTM.rda"))
   expect_message(check_inconsistent_labels(metacore),
                  "No mismatch labels detected")

   expect_error(check_inconsistent_labels("metacore"),
                "Expects a metacore object")
})
test_that("Check formats and types", {
   load(metacore_example("pilot_ADaM.rda"))
   expect_message(check_inconsistent_formats(metacore),
                  "No mismatch formats detected"
   )
   man_types <- tibble::tribble(
      ~variable, ~type, ~n_vars, ~ls_of_vars,
         "AVAL" ,    "float"  ,      6L, c("ADLBC.AVAL", "ADLBCPV.AVAL", "ADLBH.AVAL", "ADLBHPV.AVAL", "ADLBHY.AVAL", "ADVS.AVAL"),
         "AVAL",     "integer",      4L, c("ADADAS.AVAL", "ADCIBC.AVAL", "ADNPIX.AVAL", "ADTTE.AVAL"),
         "BASE",     "float"  ,      6L, c("ADLBC.BASE", "ADLBCPV.BASE", "ADLBH.BASE","ADLBHPV.BASE", "ADLBHY.BASE","ADVS.BASE"),
         "BASE" ,    "integer",      2L, 	c("ADADAS.BASE", "ADNPIX.BASE"),
         "CHG",      "float"  ,      5L, c("ADLBC.CHG", "ADLBCPV.CHG", "ADLBH.CHG", "ADLBHPV.CHG", "ADVS.CHG"),
         "CHG",      "integer",      2L, c("ADADAS.CHG", "ADNPIX.CHG"),
         "PCHG",     "float"  ,      1L, "ADVS.PCHG",
         "PCHG",     "integer",      2L, c("ADADAS.PCHG", "ADNPIX.PCHG"),
   )

   type_df <- check_inconsistent_types(metacore)

   expect_equal(type_df, man_types)
})

