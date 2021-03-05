# where should this function go
empty_df <- function(nms, fill) {
   df <- as.data.frame(matrix(fill,1,length(nms)))
   names(df) <- nms
   return(df)
}

#... why doesnt this work?
test_that("empty columns fail primitive checks", {

   dfs <- purrr::map(col_vars("ds_spec",
                              "ds_vars",
                              "var_spec",
                              "value_spec",
                              "derivations",
                              "codelist",
                              "changelog"),
                     ~ empty_df(.x, fill = ""))

   expect_warning(do.call(check_columns, dfs[-7]))
})


test_that("NA columns fail", {

   dfs <- purrr::map(col_vars("ds_spec",
                              "ds_vars",
                              "var_spec",
                              "value_spec",
                              "derivations",
                              "codelist",
                              "changelog"),
                     ~ empty_df(.x, fill = NA))

   expect_error(suppressWarnings(do.call(check_columns, dfs[-7])))
})

