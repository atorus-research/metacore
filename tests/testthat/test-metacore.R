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
              "supp"))
dfs$ds_vars <- dfs$ds_vars %>%
   mutate(supp_flag = FALSE)

# function from the withr package
with_dir <- function (new, code) {
   old <- setwd(dir = new)
   on.exit(setwd(old))
   force(code)
}


test_that("readonly function factory", {
   a <- readonly("a")
   expect_equal(class(a), "function")
   expect_equal(attr(a, "name"), "a")
})

test_that("metacore wrapper function works", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))

   r6 <- suppressWarnings(
      MetaCore$new(dfs$ds_spec,
                   dfs$ds_vars,
                   dfs$var_spec,
                   dfs$value_spec,
                   dfs$derivations,
                   dfs$codelist,
                   dfs$supp)
   )

   expect_equal(wrapper, r6)

   expect_warning(define_to_metacore(metacore_example("ADaM_define.xml")))
   expect_warning(spec_to_metacore(metacore_example("p21_mock.xlsx")))
})


test_that("Can pass metacore NULL df's", {
   wrapper <- suppressWarnings(metacore(dfs$ds_spec, NULL, dfs$var_spec,
                       dfs$value_spec, dfs$derivations, dfs$codelist, dfs$supp))
   dummy <- list(character(), character(), numeric(), numeric(),
                 logical(), character(), logical())
   names(dummy) <- c("dataset", "variable", "key_seq", "order",
                     "keep", "core", "supp_flag")
   dummy <- as_tibble(dummy)
   #Because of the labels the dfs are slightly different so checking
   # the insides match
   expect_equal(names(wrapper$ds_vars), names(dummy))
   expect_equal(map_chr(wrapper$ds_vars, mode),
                map_chr(dummy, mode))
})

test_that("subsetting works", {
   test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   subset <- test %>% select_dataset("DM")
   expect_equal(unique(subset$ds_spec$dataset), "DM")
})

test_that("save_metacore creates .rds with no file path", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))
   my_temp_dir <- tempdir()
   with_dir(my_temp_dir, save_metacore(wrapper))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("save_metacore replaces file path", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.csv"))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("save_metacore uses file path", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   expect_true("wrapper.rds" %in% list.files(my_temp_dir))
   unlink(my_temp_dir)
})

test_that("load_metacore loads .rds", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   wrapper <- load_metacore(file.path(my_temp_dir, "wrapper.rds"))
   expect_equal(class(wrapper), c("Metacore", "R6"))
   unlink(my_temp_dir)
})

test_that("load metacore fails with no path", {
   expect_error(load_metacore())
})

test_that("load metacore fails with no path and rdss in wd", {
   wrapper <- suppressWarnings(do.call(metacore, dfs[1:7]))
   my_temp_dir <- tempdir()
   save_metacore(wrapper, file.path(my_temp_dir, "wrapper.rds"))
   expect_error(
      with_dir(my_temp_dir, load_metacore())
   )
   unlink(my_temp_dir)
})

test_that("pulling out control terminology works", {
   test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   #Testing Errors
   ## Not specific enough
   expect_error(get_control_term(test, QVAL))
   ## Wrong Dataset name
   expect_error(get_control_term(test, QVAL, LB))
   ## Wrong variable name
   expect_error(get_control_term(test, QVA))
   expect_equal(
      get_control_term(test, QVAL, SUPPAE),
      tibble(code = c("N", "Y"), decode = c("No", "Yes"))
   )
   expect_equal(
      get_control_term(test, "QVAL", "SUPPAE"),
      tibble(code = c("N", "Y"), decode = c("No", "Yes"))
   )
   })

test_that("get_keys works", {
   test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
   #Testing Errors
   ## Domain not in ds_vars table
   expect_error(get_keys(test, DS))
   ## Missing dataset name
   expect_error(get_keys(test))
   #Testing Correct Output
   expect_equal(
      get_keys(test, DM),
      tibble(variable = c("STUDYID", "USUBJID"), key_seq = c(1L, 2L)) %>%
         add_labs(variable = "Variable Name",
                  key_seq = "Sequence Key")
   )
})
