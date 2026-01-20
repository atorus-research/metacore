test_that("Test label Checks", {
  load(metacore_example("pilot_ADaM.rda"))
  man_label <- tibble::tribble(
    ~variable, ~label, ~n_vars, ~ls_of_vars,
    "ANL01FL", "Analysis Flag 01", 1L, "ADADAS.ANL01FL",
    "ANL01FL", "Analysis Record Flag 01", 1L, "ADLBC.ANL01FL",
    "USUBJID", "Unique Subject Identified", 3L, c("ADLBC.USUBJID", "ADTTE.USUBJID", "ADAE.USUBJID"),
    "USUBJID", "Unique Subject Identifier", 2L, c("ADSL.USUBJID", "ADADAS.USUBJID"),
  ) %>%
    dplyr::arrange(variable, label)

  expect_warning(
    label_df <- check_inconsistent_labels(metacore) %>%
      dplyr::arrange(variable, label)
  )

  expect_equal(label_df, man_label)

  load(metacore_example("pilot_SDTM.rda"))

  expect_message(
    check_inconsistent_labels(metacore),
    "No mismatch labels detected"
  )

  expect_error(
    check_inconsistent_labels("metacore"),
    "Expects a metacore object"
  )
})

test_that("Check formats and types", {
  load(metacore_example("pilot_ADaM.rda"))
  expect_message(
    check_inconsistent_formats(metacore),
    "No mismatch formats detected"
  )

  man_types <- tibble::tribble(
    ~variable, ~type, ~n_vars, ~ls_of_vars,
    "AVAL", "float", 1L, "ADADAS.AVAL",
    "AVAL", "integer", 2L, c("ADLBC.AVAL", "ADTTE.AVAL"),
    "BASE", "float", 1L, "ADLBC.BASE",
    "BASE", "integer", 1L, "ADADAS.BASE",
    "CHG", "float", 1L, "ADLBC.CHG",
    "CHG", "integer", 1L, "ADADAS.CHG",
  )

  expect_warning(
    type_df <- check_inconsistent_types(metacore)
  )

  expect_equal(type_df, man_types)
})
