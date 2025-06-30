# where should this function go
empty_df <- function(nms, fill) {
   df <- as.data.frame(matrix(fill,1,length(nms)))
   names(df) <- nms
   return(df)
}

# both of these functions only work
# when the data def object is loaded
# and i think its checking the wrong thing

test_that("specific words and primitive columns fail when character", {

   dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
      setNames(c("ds_spec",
               "ds_vars",
               "var_spec",
               "value_spec",
               "derivations",
               "codelist",
               "supp"))

   expect_warning(do.call(check_columns, dfs))
})


test_that("NA columns fail", {

   dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = NA)) %>%
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "codelist",
                 "supp"))

   expect_error(do.call(check_columns, dfs))
})


test_that("NA columns fail", {

   dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "codelist",
                 "supp"))

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
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "codelist",
                 "supp"))

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
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "codelist",
                 "supp"))

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

dataset_meta <- select_dataset(metacore, "ADSL", quiet = TRUE)
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
