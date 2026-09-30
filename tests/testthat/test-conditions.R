empty_df <- function(nms, fill) {
  df <- as.data.frame(matrix(fill, 1, length(nms)))
  names(df) <- nms
  df
}

spec_tables <- function(fill = "A") {
  purrr::map(col_vars(), ~ empty_df(.x, fill = fill)) %>%
    setNames(c("ds_spec", "ds_vars", "var_spec", "value_spec", "derivations", "codelist", "supp"))
}

# Collects every metacore validation warning raised while evaluating `expr`,
# muffling each so the test output stays quiet. An environment gives reference
# semantics without superassignment.
collect_conditions <- function(expr) {
  acc <- new.env(parent = emptyenv())
  acc$conditions <- list()
  withCallingHandlers(
    expr,
    metacore_validation_warning = function(w) {
      acc$conditions[[length(acc$conditions) + 1L]] <- w
      invokeRestart("muffleWarning")
    }
  )
  acc$conditions
}

test_that("an all-missing column raises a classed warning carrying table and column", {
  dfs <- spec_tables()
  dfs$ds_spec$label <- NA
  cnd <- rlang::catch_cnd(
    check_columns(ds_spec = dfs$ds_spec),
    classes = "metacore_validation_warning"
  )
  expect_s3_class(cnd, "metacore_validation_warning_column_all_missing")
  expect_s3_class(cnd, "metacore_validation_warning")
  expect_s3_class(cnd, "metacore_warning")
  expect_identical(cnd$table, "ds_spec")
  expect_identical(cnd$column, "label")
  expect_null(cnd$check)
})

test_that("a failed type check names the check", {
  ds_vars <- spec_tables()$ds_vars
  ds_vars$key_seq <- "not a number"
  cnd <- rlang::catch_cnd(
    check_columns(ds_vars = ds_vars),
    classes = "metacore_validation_warning_column_check_failed"
  )
  expect_identical(cnd$table, "ds_vars")
  expect_identical(cnd$column, "key_seq")
  expect_identical(cnd$check, "is.numeric")
})

test_that("disallowed words carry the offending values", {
  test_df <- data.frame(b = c("SpecificWord", "Other", "Other"))
  cnd <- rlang::catch_cnd(
    check_structure(test_df, "b", check_words("SpecificWord"), TRUE, "test_df"),
    classes = "metacore_validation_warning_disallowed_words"
  )
  expect_identical(cnd$table, "test_df")
  expect_identical(cnd$column, "b")
  expect_identical(cnd$values, "Other")
})

test_that("cross-table checks carry the variables involved", {
  ds_vars <- data.frame(dataset = "DM", variable = "AGE", stringsAsFactors = FALSE)
  var_spec <- data.frame(variable = "SEX", stringsAsFactors = FALSE)
  cnd <- rlang::catch_cnd(
    ds_vars_check(ds_vars, var_spec),
    classes = "metacore_validation_warning_variables_without_var_spec"
  )
  expect_identical(cnd$variables, "AGE")

  value_spec <- data.frame(dataset = "DM", variable = "RACE", stringsAsFactors = FALSE)
  cnds <- collect_conditions(value_check(ds_vars, value_spec))
  expect_length(cnds, 2L)
  expect_s3_class(cnds[[1]], "metacore_validation_warning_variables_without_value_spec")
  expect_identical(cnds[[1]]$variables, "DM.AGE")
  expect_s3_class(cnds[[2]], "metacore_validation_warning_value_spec_without_ds_vars")
  expect_identical(cnds[[2]]$variables, "RACE")
})

test_that("a specific expected warning can be muffled by class and field while others pass", {
  dfs <- spec_tables()
  dfs$ds_spec$label <- NA
  dfs$ds_spec$structure <- NA
  seen <- new.env(parent = emptyenv())
  seen$columns <- character()
  expect_warning(
    withCallingHandlers(
      check_columns(ds_spec = dfs$ds_spec),
      metacore_validation_warning_column_all_missing = function(w) {
        seen$columns <- c(seen$columns, w$column)
        if (identical(w$column, "label")) invokeRestart("muffleWarning")
      }
    ),
    regexp = "`structure` from the `ds_spec` table only contains missing values"
  )
  expect_setequal(seen$columns, c("label", "structure"))
})

test_that("skipped checks say why", {
  empty <- spec_tables()
  cnd <- rlang::catch_cnd(
    metacore(
      ds_spec = empty$ds_spec[0, ], ds_vars = empty$ds_vars[0, ],
      var_spec = empty$var_spec[0, ], value_spec = empty$value_spec[0, ],
      derivations = empty$derivations[0, ], codelist = empty$codelist[0, ],
      supp = empty$supp[0, ], verbose = "warn"
    ),
    classes = "metacore_validation_warning_checks_not_performed"
  )
  expect_identical(cnd$reason, "empty")
})
