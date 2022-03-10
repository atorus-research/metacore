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
