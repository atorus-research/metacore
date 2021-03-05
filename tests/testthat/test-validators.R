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
               "code_list",
               "changelog"))

   expect_warning(do.call(check_columns, dfs[-7]))
})


test_that("NA columns fail", {

   dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = NA)) %>%
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "code_list",
                 "changelog"))

   expect_error(suppressWarnings(do.call(check_columns, dfs[-7])))
})

