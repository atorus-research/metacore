# where should this function go
empty_df <- function(nms, fill) {
   df <- as.data.frame(matrix(fill,1,length(nms)))
   names(df) <- nms
   return(df)
}

dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
   setNames(c("ds_spec",
              "ds_vars",
              "var_spec",
              "value_spec",
              "derivations",
              "code_list",
              "changelog"))

# test_that("metacore print function works", {
#    private <- list(.ds_pec = data.frame(dataset = c("a", "b", "c"), structure = 1:3))
#    MetaCore_print(const)
# })
#
# test_that("metacore init function works", {
# })
#
# test_that("metacore filter works", {
# })
#
# test_that("select_dataset works", {
# })
#
# test_that("metacore_filter works", {
#
# })

test_that("readonly function factory", {
   a <- readonly("a")
   expect_equal(class(a), "function")
   expect_equal(attr(a, "name"), "a")
})

test_that("metacore wrapper function works", {
   wrapper <- suppressWarnings(
      metacore(dfs$ds_spec, dfs$ds_vars, dfs$var_spec,
            dfs$value_spec, dfs$derivations, dfs$code_list)
   )

   r6 <- suppressWarnings(
      MetaCore$new(dfs$ds_spec, dfs$ds_vars, dfs$var_spec,
                      dfs$value_spec, dfs$derivations, dfs$code_list)
   )

   expect_equal(wrapper, r6)
})

test_that("save_metacore creates .rda", {
   wrapper <- suppressWarnings(
      metacore(dfs$ds_spec, dfs$ds_vars, dfs$var_spec,
               dfs$value_spec, dfs$derivations, dfs$code_list)
   )
   save_metacore(wrapper)
   expect_true("wrapper.rda" %in% list.files("."))
   file.remove("wrapper.rda")
})

test_that("load_metacore loads .rda", {
   wrapper <- suppressWarnings(
      metacore(dfs$ds_spec, dfs$ds_vars, dfs$var_spec,
               dfs$value_spec, dfs$derivations, dfs$code_list)
   )
   save_metacore(wrapper)
   wrapper <- load_metacore("wrapper.rda")
   expect_equal(class(wrapper), c("Metacore", "R6"))
   file.remove("wrapper.rda")
})
