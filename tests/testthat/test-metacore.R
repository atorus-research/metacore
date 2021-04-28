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
              "codelist",
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
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:6]))

   r6 <- suppressWarnings(
      MetaCore$new(dfs$ds_spec,
                   dfs$ds_vars,
                   dfs$var_spec,
                   dfs$value_spec,
                   dfs$derivations,
                   dfs$codelist)
   )

   expect_equal(wrapper, r6)
})


test_that("save_metacore creates .rds with no file path", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:6]))
   my_temp_dir <- tempdir()
   withr::with_dir(my_temp_dir, save_metacore(wrapper))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("save_metacore replaces file path", {
   wrapper <- suppressWarnings(
      metacore(dfs$ds_spec,
               dfs$ds_vars,
               dfs$var_spec,
               dfs$value_spec,
               dfs$derivations,
               dfs$code_list)
   )

   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.csv"))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("save_metacore uses file path", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:6]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("load_metacore loads .rds", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:6]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   wrapper <- load_metacore(file.path(my_temp_dir, "wrapper.rds"))
   expect_equal(class(wrapper), c("Metacore", "R6"))
   unlink(my_temp_dir)
})

test_that("load metacore fails with no path", {
   expect_error(load_metacore())
})

test_that("load metacore fails with no path and rdas in wd", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:6]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   expect_error(load_metacore())
   unlink(my_temp_dir)
})
