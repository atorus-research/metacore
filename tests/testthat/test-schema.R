# base_column_schema() ---------------------------------------------------------

test_that("base_column_schema returns a list with exactly 7 named tables", {
  schema <- base_column_schema()

  expect_type(schema, "list")
  expect_length(schema, 7L)
  expect_named(schema, c(
    ".ds_spec", ".ds_vars", ".var_spec", ".value_spec",
    ".derivations", ".codelist", ".supp"
  ))
})

test_that("base_column_schema tables are zero-row tibbles", {
  schema <- base_column_schema()

  for (nm in names(schema)) {
    expect_true(inherits(schema[[nm]], "tbl_df"), label = nm)
    expect_equal(nrow(schema[[nm]]), 0L, label = nm)
  }
})

test_that("base_column_schema has correct columns for each table", {
  schema <- base_column_schema()

  expect_named(schema$.ds_spec, c("dataset", "structure", "label"))
  expect_named(schema$.ds_vars, c("dataset", "variable", "key_seq", "order", "mandatory", "core", "supp_flag"))
  expect_named(schema$.var_spec, c("variable", "length", "label", "type", "common", "format"))
  expect_named(schema$.value_spec, c("dataset", "variable", "type", "origin", "sig_dig", "code_id", "where", "derivation_id"))
  expect_named(schema$.derivations, c("derivation_id", "derivation"))
  expect_named(schema$.codelist, c("code_id", "name", "type", "codes"))
  expect_named(schema$.supp, c("dataset", "variable", "idvar", "qeval"))
})

test_that("base_column_schema column types are correct", {
  schema <- base_column_schema()

  expect_type(schema$.ds_vars$key_seq, "integer")
  expect_type(schema$.ds_vars$order, "integer")
  expect_type(schema$.ds_vars$mandatory, "logical")
  expect_type(schema$.ds_vars$supp_flag, "logical")
  expect_type(schema$.var_spec$length, "integer")
  expect_type(schema$.var_spec$common, "logical")
  expect_type(schema$.value_spec$sig_dig, "integer")
  expect_type(schema$.codelist$codes, "list")
})


# define_column_schema() -------------------------------------------------------

test_that("define_column_schema returns a list with 10 named tables", {
  schema <- define_column_schema()

  expect_type(schema, "list")
  expect_length(schema, 10L)
  expect_true(all(c(
    ".ds_spec", ".ds_vars", ".var_spec", ".value_spec",
    ".derivations", ".codelist", ".supp",
    ".study_level", ".documents", ".comments"
  ) %in% names(schema)))
})

test_that("define_column_schema tables are zero-row tibbles", {
  schema <- define_column_schema()

  for (nm in names(schema)) {
    expect_true(inherits(schema[[nm]], "tbl_df"), label = nm)
    expect_equal(nrow(schema[[nm]]), 0L, label = nm)
  }
})

test_that("define_column_schema extends ds_spec with define-specific columns", {
  schema <- define_column_schema()
  base <- base_column_schema()

  extra <- setdiff(names(schema$.ds_spec), names(base$.ds_spec))
  expect_equal(sort(extra), sort(c("class", "repeating", "reference", "purpose")))
})

test_that("define_column_schema extends ds_vars with role", {
  schema <- define_column_schema()
  base <- base_column_schema()

  extra <- setdiff(names(schema$.ds_vars), names(base$.ds_vars))
  expect_equal(extra, "role")
})

test_that("define_column_schema extends value_spec with where_label and comment_id", {
  schema <- define_column_schema()
  base <- base_column_schema()

  extra <- setdiff(names(schema$.value_spec), names(base$.value_spec))
  expect_equal(sort(extra), c("comment_id", "where_label"))
})

test_that("define_column_schema extends derivations with method and document columns", {
  schema <- define_column_schema()
  base <- base_column_schema()

  extra <- setdiff(names(schema$.derivations), names(base$.derivations))
  expect_equal(sort(extra), sort(c("method_name", "method_type", "document_id", "pages")))
})

test_that("define_column_schema adds study_level table with correct columns", {
  schema <- define_column_schema()

  expect_named(schema$.study_level, c(
    "study_name", "study_description", "protocol_name",
    "standard_name", "standard_version", "define_version", "language"
  ))
})

test_that("define_column_schema adds documents table with correct columns", {
  schema <- define_column_schema()
  expect_named(schema$.documents, c("document_id", "title", "href"))
})

test_that("define_column_schema adds comments table with correct columns", {
  schema <- define_column_schema()
  expect_named(schema$.comments, c("comment_id", "comment"))
})

test_that("define_column_schema preserves all base columns", {
  schema <- define_column_schema()
  base <- base_column_schema()

  for (tbl in names(base)) {
    base_cols <- names(base[[tbl]])
    define_cols <- names(schema[[tbl]])
    expect_true(all(base_cols %in% define_cols), info = tbl)
  }
})


# base_col_regex() and define_col_regex() --------------------------------------

test_that("base_col_regex returns a named list of character vectors", {
  regex <- base_col_regex()

  expect_type(regex, "list")
  expect_true(all(c(".ds_spec", ".ds_vars", ".var_spec", ".value_spec", ".derivations") %in% names(regex)))
  for (nm in names(regex)) {
    expect_type(regex[[nm]], "character")
    expect_false(is.null(names(regex[[nm]])))
  }
})

test_that("base_col_regex covers required columns for ds_spec", {
  regex <- base_col_regex()
  expect_true(all(c("dataset", "structure", "label") %in% names(regex$.ds_spec)))
})

test_that("define_col_regex extends base with define-specific entries", {
  base_r <- base_col_regex()
  define_r <- define_col_regex()

  # All base entries are present in define
  for (tbl in names(base_r)) {
    base_keys <- names(base_r[[tbl]])
    define_keys <- names(define_r[[tbl]])
    expect_true(all(base_keys %in% define_keys), info = tbl)
  }
})

test_that("define_col_regex adds documents and comments tables", {
  define_r <- define_col_regex()

  expect_true(".documents" %in% names(define_r))
  expect_true(".comments" %in% names(define_r))
  expect_true("document_id" %in% names(define_r$.documents))
  expect_true("comment_id" %in% names(define_r$.comments))
})

test_that("define_col_regex adds define-specific ds_spec columns", {
  define_r <- define_col_regex()
  expect_true(all(c("class", "repeating", "reference", "purpose") %in% names(define_r$.ds_spec)))
})


# define_only_cols() -----------------------------------------------------------

test_that("define_only_cols returns extra ds_spec columns", {
  extra <- define_only_cols("ds_spec")
  expect_equal(sort(extra), sort(c("class", "repeating", "reference", "purpose")))
})

test_that("define_only_cols returns role for ds_vars", {
  expect_equal(define_only_cols("ds_vars"), "role")
})

test_that("define_only_cols returns where_label and comment_id for value_spec", {
  extra <- define_only_cols("value_spec")
  expect_equal(sort(extra), c("comment_id", "where_label"))
})

test_that("define_only_cols returns method and document columns for derivations", {
  extra <- define_only_cols("derivations")
  expect_equal(sort(extra), sort(c("method_name", "method_type", "document_id", "pages")))
})

test_that("define_only_cols returns empty for tables identical in both schemas", {
  expect_equal(define_only_cols("var_spec"), character(0))
  expect_equal(define_only_cols("codelist"), character(0))
  expect_equal(define_only_cols("supp"), character(0))
})

test_that("define_only_cols returns all columns for define-only tables", {
  sl_cols <- define_only_cols("study_level")
  expect_equal(
    sort(sl_cols),
    sort(c(
      "study_name", "study_description", "protocol_name",
      "standard_name", "standard_version", "define_version", "language"
    ))
  )
  expect_equal(sort(define_only_cols("documents")), sort(c("document_id", "title", "href")))
  expect_equal(sort(define_only_cols("comments")), sort(c("comment_id", "comment")))
})

test_that("define_only_cols returns empty character for unknown table", {
  expect_equal(define_only_cols("nonexistent"), character(0))
})


# col_vars() -------------------------------------------------------------------

test_that("col_vars with default schema returns 7 tables (excludes study_level etc.)", {
  cv <- col_vars()

  expect_type(cv, "list")
  expect_length(cv, 7L)
  expect_false("study_level" %in% names(cv))
  expect_false("documents" %in% names(cv))
  expect_false("comments" %in% names(cv))
})

test_that("col_vars includes all 7 expected table names", {
  cv <- col_vars()
  expect_true(all(c(
    ".ds_spec", ".ds_vars", ".var_spec", ".value_spec",
    ".derivations", ".codelist", ".supp"
  ) %in% names(cv)))
})

test_that("col_vars with base schema returns fewer columns per table", {
  cv_define <- col_vars(define_column_schema())
  cv_base <- col_vars(base_column_schema())

  # Base and define both have 7 tables
  expect_equal(length(cv_define), length(cv_base))

  # Define tables have >= as many columns as base tables
  for (tbl in names(cv_base)) {
    expect_gte(length(cv_define[[tbl]]), length(cv_base[[tbl]]), label = tbl)
  }
})

test_that("col_vars returns character vectors per table", {
  cv <- col_vars()
  for (nm in names(cv)) {
    expect_type(cv[[nm]], "character")
  }
})


# fill_cols() ------------------------------------------------------------------

test_that("fill_cols with NULL input returns the schema tibble", {
  schema <- base_column_schema()$.ds_spec
  result <- fill_cols(NULL, schema)

  expect_equal(result, schema)
})

test_that("fill_cols leaves existing columns unchanged", {
  schema <- base_column_schema()$.ds_spec
  data <- tibble::tibble(dataset = "AE", structure = "1PerRow", label = "AE")
  result <- fill_cols(data, schema)

  expect_equal(result$dataset, "AE")
  expect_equal(result$structure, "1PerRow")
  expect_equal(result$label, "AE")
})

test_that("fill_cols back-fills missing columns with NA of the correct type", {
  schema <- define_column_schema()$.ds_spec # has class, repeating, reference, purpose
  data <- tibble::tibble(dataset = "AE", structure = "1PerRow", label = "AE")
  result <- fill_cols(data, schema)

  expect_true("class" %in% names(result))
  expect_true("repeating" %in% names(result))
  expect_true(is.na(result$class[[1]]))
  expect_true(is.na(result$repeating[[1]]))
  expect_type(result$class, "character")
  expect_type(result$repeating, "logical")
})

test_that("fill_cols does not add columns beyond what the schema specifies", {
  schema <- base_column_schema()$.ds_spec
  data <- tibble::tibble(
    dataset = "AE", structure = "1PerRow", label = "AE",
    extra_col = "EXTRA"
  )
  result <- fill_cols(data, schema)

  # extra_col is NOT in schema but should still be present (fill_cols only adds, not removes)
  expect_true("extra_col" %in% names(result))
})

test_that("fill_cols handles an already complete data frame", {
  schema <- base_column_schema()$.ds_spec
  data <- tibble::tibble(dataset = "AE", structure = "1PerRow", label = "AE")
  result <- fill_cols(data, schema)

  expect_equal(names(result), names(schema))
  expect_equal(nrow(result), 1L)
})


# reorder_by_schema() ----------------------------------------------------------

test_that("reorder_by_schema puts columns in schema order", {
  # Deliberately scramble the column order
  scrambled <- tibble::tibble(
    label = "AE",
    dataset = "AE",
    structure = "1PerRow"
  )
  result <- reorder_by_schema(scrambled, "ds_spec")

  expect_equal(names(result), c("dataset", "structure", "label"))
})

test_that("reorder_by_schema drops columns not in the schema", {
  data <- tibble::tibble(dataset = "AE", structure = "1PerRow", label = "AE", extra = "X")
  result <- reorder_by_schema(data, "ds_spec")

  expect_false("extra" %in% names(result))
  expect_equal(names(result), c("dataset", "structure", "label"))
})

test_that("reorder_by_schema returns data unchanged for an unknown table name", {
  data <- tibble::tibble(a = 1, b = 2, c = 3)
  result <- reorder_by_schema(data, "nonexistent_table")

  expect_equal(names(result), c("a", "b", "c"))
})

test_that("reorder_by_schema handles a subset of schema columns", {
  # Only provide 2 of the 3 ds_spec columns
  data <- tibble::tibble(label = "AE", dataset = "AE")
  result <- reorder_by_schema(data, "ds_spec")

  expect_equal(names(result), c("dataset", "label"))
})
