# where should this function go
empty_df <- function(nms, fill) {
  df <- as.data.frame(matrix(fill, 1, length(nms)))
  names(df) <- nms
  return(df)
}

dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
  setNames(c(
    "ds_spec",
    "ds_vars",
    "var_spec",
    "value_spec",
    "derivations",
    "codelist",
    "supp"
  ))
dfs$ds_vars <- dfs$ds_vars %>%
  mutate(supp_flag = FALSE)

# function from the withr package
with_dir <- function(new, code) {
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
    MetaCore$new(
      dfs$ds_spec,
      dfs$ds_vars,
      dfs$var_spec,
      dfs$value_spec,
      dfs$derivations,
      dfs$codelist,
      dfs$supp
    )
  )

  expect_equal(wrapper, r6)

  expect_warning(define_to_metacore(metacore_example("ADaM_define_CDISC_pilot3.xml")))
  expect_warning(spec_to_metacore(metacore_example("p21_mock.xlsx")))
})


test_that("Can pass metacore NULL df's", {
  wrapper <- suppressWarnings(metacore(
    dfs$ds_spec, NULL, dfs$var_spec,
    dfs$value_spec, dfs$derivations, dfs$codelist, dfs$supp
  ))
  dummy <- list(
    character(), character(), numeric(), numeric(),
    logical(), character(), logical()
  )
  names(dummy) <- c(
    "dataset", "variable", "key_seq", "order",
    "mandatory", "core", "supp_flag"
  )
  dummy <- as_tibble(dummy)
  # Because of the labels the dfs are slightly different so checking
  # the insides match
  expect_equal(names(wrapper$ds_vars), names(dummy))
  expect_equal(
    map_chr(wrapper$ds_vars, mode),
    map_chr(dummy, mode)
  )
})

test_that("subsetting works", {
  test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
  subset <- test %>% select_dataset("DM", verbose = "silent")
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

test_that("get_control_term: Missing variable argument", {
  # Test when variable is missing entirely
  expect_error(get_control_term(p21_spec), "must be provided")
})

test_that("get_control_term: Variable not found in value_spec", {
  # Test with non-existent variable
  expect_error(get_control_term(p21_spec, NONEXISTENT), "not found in `value_spec`")
  expect_error(get_control_term(p21_spec, "NONEXISTENT"), "not found in `value_spec`")
})

test_that("get_control_term: Dataset filtering - valid dataset", {
  # Test with both bare and string dataset specification
  result1 <- get_control_term(p21_spec, QVAL, "SUPPAE")
  result2 <- get_control_term(p21_spec, "QVAL", "SUPPAE")

  expect_equal(result1, tibble(code = c("N", "Y"), decode = c("No", "Yes")))
  expect_equal(result2, result1)
})

test_that("get_control_term: Dataset filtering - invalid dataset", {
  # Dataset doesn't exist for this variable
  expect_error(get_control_term(p21_spec, QVAL, "INVALIDDS"), "not found in `value_spec`")
})

test_that("get_control_term: VLM where filter - valid condition", {
  result <- get_control_term(vlm_spec, AVALCAT1, where = "PARAMCD EQ ADURD")
  expect_type(result, "list")
})

test_that("get_control_term: multiple where conditions get unique names when where = `all`", {
  result <- get_control_term(vlm_spec, AVALCAT1, where = "all")
  # Should return a named list with unique names
  expect_true(is.list(result))
  expect_true(length(unique(names(result))) == length(names(result)))
})

test_that("get_control_term: VLM where filter - invalid condition", {
  # Test with non-existent where condition
  expect_error(
    get_control_term(p21_spec, QVAL, "SUPPAE", where = "INVALID_WHERE"),
    "No VLM condition matching"
  )
})

test_that("get_control_term: VLM where filter - multiple code_ids raises error", {
  expect_error(
    get_control_term(vlm_spec, TESTVAR1, "DUMMY", where = "PARAMCD EQ TESTA"),
    "does not resolve to a single\\s+codelist"
  )
})

test_that("get_control_term: No control terminology (all code_ids NA)", {
  expect_message(get_control_term(suppae_spec, STUDYID), "has no controlled terminology")
})

test_that("get_control_term: VLM with single code_id returns dataframe", {
  result <- get_control_term(vlm_spec, TESTVAR2)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
})

test_that("get_control_term: VLM with multiple conditions returns error when where = NULL", {
  expect_error(get_control_term(vlm_spec, AVALCA1N), "does not have a unique\\s+codelist")
})

test_that("get_control_term: VLM with multiple conditions returns named list when where = `all`", {
  result <- get_control_term(vlm_spec, AVALCA1N, where = "all")
  expect_true(is.list(result))
  expect_true(length(result) > 1)
  expect_true(all(sapply(result, is.data.frame)))
})

test_that("get_control_term: Non-VLM with multiple code_ids requires dataset", {
  # Should error without dataset specification
  expect_error(get_control_term(p21_spec, QVAL), "does not have a unique codelist")

  # Should succeed with dataset
  result <- get_control_term(p21_spec, QVAL, "SUPPAE")
  expect_true(is.data.frame(result))
})

test_that("get_control_term: Non-VLM with single code_id returns dataframe", {
  result <- get_control_term(p21_spec, AESEV)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("get_control_term: Invalid codelist structure triggers error", {
  codelist <- tibble(
    code_id = "DUMMYCODEA",
    name = "Bad",
    type = "Codelist",
    codes = list(c("A", "B")) # Not a dataframe!
  )

  bad_spec <- metacore(vlm_spec$ds_spec, vlm_spec$ds_vars, vlm_spec$var_spec, vlm_spec$value_spec,
    vlm_spec$derivations, codelist, vlm_spec$supp,
    verbose = "silent"
  )

  expect_error(get_control_term(bad_spec, TESTVAR2), "Unexpected codelist structure")
})

test_that("get_keys works", {
  test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
  # Testing Errors
  ## Domain not in ds_vars table
  expect_error(get_keys(test, DS))
  ## Missing dataset name
  expect_error(get_keys(test))
  # Testing Correct Output
  expect_equal(
    get_keys(test, DM),
    tibble(variable = c("STUDYID", "USUBJID"), key_seq = c(1L, 2L)) %>%
      add_labs(
        variable = "Variable Name",
        key_seq = "Sequence Key"
      )
  )
})

test_that("spec_to_metacore() is silent when verbose = `silent`", {
  test <- metacore_example("p21_mock.xlsx")

  expect_silent({
    out <- spec_to_metacore(test, verbose = "silent")
  })

  expect_true(inherits(out, "Metacore"))
})

test_that("spec_to_metacore() verbose = `silent` is silent and returns Metacore", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  expect_silent({
    mc_q <- spec_to_metacore(path, verbose = "silent")
    expect_true(inherits(mc_q, "Metacore"))
  })
})

test_that("spec_to_metacore() verbose = `silent` returns invisibly", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  expect_invisible(
    spec <- spec_to_metacore(path, verbose = "silent")
  )
})

test_that("spec_to_metacore() quiet = FALSE returns a Metacore object", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  # We don't assert on printed output here; just on the return type.
  mc_n <- suppressWarnings(spec_to_metacore(path, quiet = FALSE))
  expect_true(inherits(mc_n, "Metacore"))
})

test_that("spec_to_metacore() returns structurally similar objects for quiet TRUE/FALSE", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  mc_q <- suppressWarnings(spec_to_metacore(path, verbose = "silent"))
  mc_n <- suppressWarnings(spec_to_metacore(path, quiet = FALSE))

  expect_true(inherits(mc_q, "Metacore"))
  expect_true(inherits(mc_n, "Metacore"))

  # Basic structural check: same component tables
  expect_identical(names(mc_q$data), names(mc_n$data))
})


test_that("select_dataset() is silent when verbose = `silent`", {
  test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
  subset <- test %>% select_dataset("DM", verbose = "silent")
  expect_silent({
    subset <- test %>% select_dataset("DM", verbose = "silent")
  })
})

test_that("metacore() verbose = `silent` is silent and returns Metacore object", {
  # simplest small valid inputs
  ds_spec <- tibble::tibble(dataset = "AE", structure = "OneRowPerRecord", label = "Adverse Events")
  ds_vars <- tibble::tibble(
    dataset = "AE", variable = "AETERM", keep = TRUE,
    key_seq = 1L, order = 1L, core = "Req", supp_flag = FALSE
  )
  var_spec <- tibble::tibble(
    variable = "AETERM", label = "Reported Term", length = 200L,
    type = "character", common = NA_character_, format = NA_character_
  )
  value_spec <- tibble::tibble(
    dataset = "AE", variable = "AETERM", where = NA_character_,
    type = "character", sig_dig = NA_integer_,
    code_id = NA_character_, origin = "Collected", derivation_id = NA_integer_
  )
  derivations <- tibble::tibble(derivation_id = integer(), derivation = character())
  codelist <- tibble::tibble(code_id = character(), name = character(), type = character(), codes = list())
  supp <- tibble::tibble(dataset = character(), variable = character(), idvar = character(), qeval = character())

  expect_silent({
    mc_q <- metacore(
      ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp,
      verbose = "silent"
    )
    expect_true(inherits(mc_q, "Metacore"))
  })
})

test_that("metacore() verbose = `silent` returns invisibly", {
  ds_spec <- tibble::tibble(dataset = "AE", structure = "Row", label = "AE")
  ds_vars <- tibble::tibble(
    dataset = "AE", variable = "AETERM", keep = TRUE,
    key_seq = 1L, order = 1L, core = "Req", supp_flag = FALSE
  )
  var_spec <- tibble::tibble(
    variable = "AETERM", label = "Term", length = 200L,
    type = "character", common = NA_character_, format = NA_character_
  )
  value_spec <- tibble::tibble(
    dataset = "AE", variable = "AETERM", where = NA_character_,
    type = "character", sig_dig = NA_integer_,
    code_id = NA_character_, origin = "Collected", derivation_id = NA_integer_
  )

  expect_invisible(
    spec <- metacore(
      ds_spec, ds_vars, var_spec, value_spec,
      derivations = tibble::tibble(),
      codelist = tibble::tibble(),
      supp = tibble::tibble(),
      verbose = "silent"
    )
  )
})

test_that("metacore() quiet = FALSE returns a Metacore object", {
  ds_spec <- tibble::tibble(dataset = "AE", structure = "Row", label = "AE")
  ds_vars <- tibble::tibble(
    dataset = "AE", variable = "AETERM", keep = TRUE,
    key_seq = 1L, order = 1L, core = "Req", supp_flag = FALSE
  )
  var_spec <- tibble::tibble(
    variable = "AETERM", label = "Term", length = 200L,
    type = "character", common = NA_character_, format = NA_character_
  )
  value_spec <- tibble::tibble(
    dataset = "AE", variable = "AETERM", where = NA_character_,
    type = "character", sig_dig = NA_integer_,
    code_id = NA_character_, origin = "Collected", derivation_id = NA_integer_
  )

  mc <- suppressWarnings(
    metacore(
      ds_spec, ds_vars, var_spec, value_spec,
      derivations = tibble::tibble(),
      codelist = tibble::tibble(),
      supp = tibble::tibble(),
      quiet = FALSE
    )
  )

  expect_true(inherits(mc, "Metacore"))
})

test_that("metacore() verbose message/silent paths produce similar structure", {
  ds_spec <- tibble::tibble(dataset = "AE", structure = "Row", label = "AE")
  ds_vars <- tibble::tibble(
    dataset = "AE", variable = "AETERM", keep = TRUE,
    key_seq = 1L, order = 1L, core = "Req", supp_flag = FALSE
  )
  var_spec <- tibble::tibble(
    variable = "AETERM", label = "Term", length = 200L,
    type = "character", common = NA_character_, format = NA_character_
  )
  value_spec <- tibble::tibble(
    dataset = "AE", variable = "AETERM", where = NA_character_,
    type = "character", sig_dig = NA_integer_,
    code_id = NA_character_, origin = "Collected", derivation_id = NA_integer_
  )

  mc_q <- suppressWarnings(
    metacore(ds_spec, ds_vars, var_spec, value_spec,
      tibble::tibble(), tibble::tibble(), tibble::tibble(),
      verbose = "silent"
    )
  )

  mc_n <- suppressWarnings(
    metacore(ds_spec, ds_vars, var_spec, value_spec,
      tibble::tibble(), tibble::tibble(), tibble::tibble(),
      verbose = "message"
    )
  )

  expect_identical(names(mc_q$data), names(mc_n$data))
})

test_that("metacore(quiet) deprecation message is output when supplied by the user`", {
  # `Deprecation when quiet = FALSE`
  lifecycle::expect_deprecated(
    specs <- metacore(
      ds_spec = data.frame(
        dataset = "ADSL",
        structure = NA_character_,
        label = "Subject-Level Analysis Dataset"
      ),
      ds_vars = data.frame(
        dataset = "ADSL",
        variable = c("STUDYID", "USUBJID"),
        key_seq = NA_integer_,
        order = NA_integer_,
        keep = NA_character_,
        core = NA_character_,
        supp_flag = NA
      ),
      quiet = FALSE
    )
  )

  # `Deprecation when quiet = TRUE`
  lifecycle::expect_deprecated(
    specs <- metacore(
      ds_spec = data.frame(
        dataset = "ADSL",
        structure = NA_character_,
        label = "Subject-Level Analysis Dataset"
      ),
      ds_vars = data.frame(
        dataset = "ADSL",
        variable = c("STUDYID", "USUBJID"),
        key_seq = NA_integer_,
        order = NA_integer_,
        keep = NA_character_,
        core = NA_character_,
        supp_flag = NA
      ),
      quiet = TRUE
    )
  )
})

test_that("select_dataset(quiet) deprecation message is output when supplied by the user`", {
  spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
  # `Deprecation when quiet = FALSE`
  lifecycle::expect_deprecated(
    select_dataset(spec, "AE", quiet = FALSE, verbose = "silent")
  )

  # `Deprecation when quiet = TRUE`
  lifecycle::expect_deprecated(
    select_dataset(spec, "AE", quiet = TRUE)
  )
})

test_that("select_dataset(simplify = TRUE) returns expected structure", {
  spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
  ae <- select_dataset(spec, "AE", verbose = "silent")
  ae_simple <- select_dataset(spec, "AE", simplify = TRUE, verbose = "silent")

  expected_names <- c(
    "dataset", "variable", "order", "mandatory", "key_seq", "core", "supp_flag",
    "length", "label", "type", "format", "common", "origin", "code_id", "sig_dig",
    "derivation_id", "where", "derivation", "codes", "idvar", "qeval"
  )

  expect_equal(names(ae_simple), expected_names)
  expect_equal(nrow(ae_simple), nrow(ae$ds_vars))
})
