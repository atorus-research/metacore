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
  test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
  subset <- test %>% select_dataset("DM", quiet = TRUE)
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
  # Testing Errors
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

test_that("spec_to_metacore() is silent when quiet = TRUE", {
  test <- metacore_example("p21_mock.xlsx")

  expect_silent({
    out <- spec_to_metacore(test, quiet = TRUE)
  })

  expect_true(inherits(out, "Metacore"))
})

test_that("spec_to_metacore() quiet = TRUE is silent and returns Metacore", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  expect_silent({
    mc_q <- spec_to_metacore(path, quiet = TRUE)
    expect_true(inherits(mc_q, "Metacore"))
  })
})

test_that("spec_to_metacore() quiet = TRUE returns invisibly", {
  path_try <- try(metacore_example("p21_mock.xlsx"), silent = TRUE)
  if (inherits(path_try, "try-error") || path_try == "") {
    skip("p21_mock.xlsx example spec not available")
  }
  path <- path_try

  expect_invisible(
    spec_to_metacore(path, quiet = TRUE)
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

  mc_q <- suppressWarnings(spec_to_metacore(path, quiet = TRUE))
  mc_n <- suppressWarnings(spec_to_metacore(path, quiet = FALSE))

  expect_true(inherits(mc_q, "Metacore"))
  expect_true(inherits(mc_n, "Metacore"))

  # Basic structural check: same component tables
  expect_identical(names(mc_q$data), names(mc_n$data))
})


test_that("select_dataset() is silent when quiet = TRUE", {
  test <- spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE)
  subset <- test %>% select_dataset("DM", quiet = TRUE)
  expect_silent({
    subset <- test %>% select_dataset("DM", quiet = TRUE)
  })
})

test_that("metacore() quiet = TRUE is silent and returns Metacore object", {
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
      quiet = TRUE
    )
    expect_true(inherits(mc_q, "Metacore"))
  })
})

test_that("metacore() quiet = TRUE returns invisibly", {
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
    metacore(
      ds_spec, ds_vars, var_spec, value_spec,
      derivations = tibble::tibble(),
      codelist = tibble::tibble(),
      supp = tibble::tibble(),
      quiet = TRUE
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

test_that("metacore() quiet TRUE/FALSE paths produce similar structure", {
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
      quiet = TRUE
    )
  )

  mc_n <- suppressWarnings(
    metacore(ds_spec, ds_vars, var_spec, value_spec,
      tibble::tibble(), tibble::tibble(), tibble::tibble(),
      quiet = FALSE
    )
  )

  expect_identical(names(mc_q$data), names(mc_n$data))
})
