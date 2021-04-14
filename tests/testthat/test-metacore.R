# where should this function go
empty_df <- function(nms, fill) {
   df <- as.data.frame(matrix(fill,1,length(nms)))
   names(df) <- nms
   return(df)
}

MetaCore_filter()

MetaCore_initialize()

test_that("MetaCore_initialize adds labels to dataframes", {

   private <- list()

   dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
      setNames(c("ds_spec",
                 "ds_vars",
                 "var_spec",
                 "value_spec",
                 "derivations",
                 "codelist",
                 "changelog"))

   MetaCore_initialize(dfs$ds_spec,
                       dfs$ds_vars,
                       dfs$var_spec,
                       dfs$value_spec,
                       dfs$derivations,
                       dfs$codelist)


})

MetaCore_print()

MetaCore_validate()

readonly()

metacore()

select_dataset()
