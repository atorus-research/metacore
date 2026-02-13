# where should this function go
empty_df <- function(nms, fill) {
  df <- as.data.frame(matrix(fill, 1, length(nms)))
  names(df) <- nms
  return(df)
}

# both of these functions only work
# when the data def object is loaded
# and i think its checking the wrong thing

test_that("specific words and primitive columns fail when character", {
  dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
    setNames(c(
      "ds_spec",
      "ds_vars",
      "var_spec",
      "value_spec",
      "derivations",
      "codelist",
      "supp"
    ))

  expect_warning(do.call(check_columns, dfs))
})


test_that("NA columns fail", {
  dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = NA)) %>%
    setNames(c(
      "ds_spec",
      "ds_vars",
      "var_spec",
      "value_spec",
      "derivations",
      "codelist",
      "supp"
    ))

  expect_error(do.call(check_columns, dfs))
})


test_that("NA columns fail", {
  dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
    setNames(c(
      "ds_spec",
      "ds_vars",
      "var_spec",
      "value_spec",
      "derivations",
      "codelist",
      "supp"
    ))

  dfs$ds_spec$label <- NA

  expect_warning(do.call(check_columns, dfs))
})

test_that("all_message dataframe contains 6 datasets", {
  expect_equal(all_message() %>%
    distinct(dataset) %>%
    nrow(), 7)
})

test_that("check cross-reference tests", {
  dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
    setNames(c(
      "ds_spec",
      "ds_vars",
      "var_spec",
      "value_spec",
      "derivations",
      "codelist",
      "supp"
    ))

  dfs$ds_vars <- dfs$ds_vars %>%
    mutate(supp_flag = TRUE)
  dfs$var_spec <- dfs$var_spec %>%
    mutate(variable = "B")
  dfs$derivations <- dfs$derivations %>%
    mutate(derivation_id = "C")
  dfs$codelist <- dfs$codelist %>%
    mutate(code_id = "D")
  expect_warning(do.call(metacore, dfs[1:7]))
})

test_that("test for incorrect column names", {
  dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = NA)) %>%
    setNames(c(
      "ds_spec",
      "ds_vars",
      "var_spec",
      "value_spec",
      "derivations",
      "codelist",
      "supp"
    ))

  dfs$codelist <- dfs$codelist %>%
    mutate(codelist2 = "A")
  expect_warning(do.call(metacore, dfs[1:7]))
})

test_that("check object works", {
  load(metacore_example("pilot_ADaM.rda"))
  metacore %>%
    is_metacore() %>%
    expect_equal(TRUE)

  is_metacore("THIS IS NOT A THING") %>%
    expect_equal(FALSE)
})

# Test that is_DatasetMeta works as intended for various class types
load(metacore_example("pilot_ADaM.rda"))
test_that("is_DatasetMeta returns FALSE if a non-DatasetMeta object is supplied", {
  expect_false(is_DatasetMeta(metacore))
})

dataset_meta <- select_dataset(metacore, "ADSL", verbose = "silent")
test_that("is_DatasetMeta returns TRUE if a DatasetMeta object is supplied", {
  expect_true(is_DatasetMeta(dataset_meta))
})

# Test that internal function check_DatasetMeta works as intended for various class types
test_that("check_DatasetMeta throws an error if a non-Metacore object is supplied", {
  expect_error(verify_DatasetMeta("DUMMY"))
})

test_that("is_DatasetMeta throws an error if a non-DatasetMeta object is supplied", {
  expect_error(verify_DatasetMeta(metacore))
})

test_that("is_DatasetMeta returns TRUE if a DatasetMeta object is supplied", {
  expect_true(verify_DatasetMeta(dataset_meta))
})

test_that("check_columns handles multiple datasets including 'supp' correctly", {
  # Create dummy dataframes that conform to all_message specifications
  ds_spec_test <- tribble(
    ~dataset, ~structure, ~label,
    "ADSL", "flat", "Subject-Level Data"
  )
  ds_vars_test <- tribble(
    ~dataset, ~variable, ~key_seq, ~order, ~mandatory, ~core, ~supp_flag,
    "ADSL", "USUBJID", 1, 1, TRUE, "Expected", FALSE
  )
  var_spec_test <- tribble(
    ~variable, ~type, ~length, ~label, ~format, ~common,
    "USUBJID", "Char", 8, "Unique Subject ID", "$8.", TRUE
  )
  supp_test <- tribble(
    ~dataset, ~variable, ~idvar, ~qeval,
    "ADSL", "AGE", "USUBJID", "AGE"
  )

  # Expect no errors or warnings for valid data, including the 'supp' dataset
  expect_silent(check_columns(
    ds_spec = ds_spec_test,
    ds_vars = ds_vars_test,
    var_spec = var_spec_test,
    supp = supp_test
  ))

  # Example: Test for a missing column to ensure `check_structure` works
  ds_spec_bad <- tribble(
    ~dataset, ~structure,
    "ADSL", "flat"
  )
  expect_error(
    check_columns(ds_spec = ds_spec_bad)
  )
})

test_that("check_columns handles multiple datasets excluding 'supp' correctly", {
  # Create dummy dataframes that conform to all_message specifications
  ds_spec_test <- tribble(
    ~dataset, ~structure, ~label,
    "ADSL", "flat", "Subject-Level Data"
  )
  ds_vars_test <- tribble(
    ~dataset, ~variable, ~key_seq, ~order, ~mandatory, ~core, ~supp_flag,
    "ADSL", "USUBJID", 1, 1, TRUE, "Expected", FALSE
  )
  var_spec_test <- tribble(
    ~variable, ~type, ~length, ~label, ~format, ~common,
    "USUBJID", "Char", 8, "Unique Subject ID", "$8.", TRUE
  )

  ds_vars_bad <- tribble(
    ~dataset, ~variable, ~idvar, ~qeval
  )

  # Example: Empty supp dataset, check_columns called manually
  expect_warning(
    check_columns(supp = ds_vars_bad),
    regexp = cli::cli_inform(c(
      "*" = "`dataset` from the `supp` table only contains missing values.",
      "*" = "`variable` from the `supp` table only contains missing values.",
      "*" = "`idvar` from the `supp` table only contains missing values.",
      "*" = "`qeval` from the `supp` table only contains missing values."
    ))
  )

  # Example: Empty supp dataset, not included in check_columns call
  ds_vars_bad <- tribble(
    ~dataset, ~variable, ~idvar, ~qeval
  )
  expect_silent(
    check_columns(
      ds_spec = ds_spec_test,
      ds_vars = ds_vars_test,
      var_spec = var_spec_test
    )
  )
})
