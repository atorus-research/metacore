test_df <- data.frame(
   a = c("blah", NA, NA),
   b = c("SpecificWord", "Incorrect", "Dummy")
)

test_that("check_structure function works", {
  # error when NA not permissible

  expect_equal(check_structure(test_df, "a", is.character, FALSE, "test_df")$error,
               "`a` from the `test_df` table contains missing values. Actual values are needed.")

  expect_equal(check_structure(test_df, "a", is.logical, TRUE, "test_df")$warning,
               "test_df$a fails is.logical check"
               )

  expect_null(check_structure(test_df, "b", check_words("SpecificWord", "Incorrect", "Dummy"), TRUE, "test_df")$warning)
  expect_warning(check_structure(test_df, "b", check_words("SpecificWord"), TRUE, "test_df"))

})

test_that("check_words creates function", {
   expect_true(is.function(check_words("word")))
})


test_that("add_lab function adds label", {
  a <- add_lab("a", "mylabel")
  expect_equal(attr(a, "label"), "mylabel")
})

test_that("label set to null if not supplied", {
  a <- add_lab("a", character(0))
  expect_null(attr(a, "label"))
})

test_that("add labels adds multiple labels", {
  a <- data.frame(a = "a") %>%
    add_labs("a" = "label_a")
  expect_equal(attr(a$a, "label"), "label_a")
})

test_that("add labels adds NULL to missing labels", {
  a <- data.frame(a = "a", b = "b") %>%
    add_labs("a" = "label_a", "b" = character(0))
  expect_null(attr(a$b, "label"))
})

test_that("metacore example returns file options", {
  expect_equal(sort(metacore_example()),
               sort(c("ADaM_define_CDISC_pilot3.xml", "mock_spec.xlsx", "p21_mock.xlsx", "pilot_ADaM.rda",
                      "pilot_SDTM.rda", "SDTM_define.xml", "SDTM_spec_CDISC_pilot.xlsx")))
})

test_that("with_verbosity suppresses messages when verbose = `silent`", {
   expect_silent(
      with_verbosity({
         message("this should not print")
      }, verbose = "silent")
   )
})

test_that("with_verbosity suppresses warnings when verbose = `silent`", {
   expect_silent(
      with_verbosity({
         warning("this should not print")
      }, verbose = "silent")
   )
})

test_that("with_verbosity suppresses cli output when verbose = `silent`", {
   skip_if_not_installed("cli")

   expect_silent(
      with_verbosity({
         cli::cli_alert_info("cli output should not print")
         cli::cli_rule("Suppressed rule")
         cli::cli_bullets(c("• Bullet should be suppressed"))
      }, verbose = "silent")
   )
})

test_that("with_verbosity does not suppress errors when verbose = `silent`", {
   expect_error(
      with_verbosity({
         stop("This error must propagate")
      }, verbose = "silent"),
      "This error must propagate"
   )
})

test_that("with_verbosity still evaluates side-effect code when verbose = `silent`", {
   env <- new.env(parent = emptyenv())
   env$x <- 0

   with_verbosity({
      env$x <- 99
   }, verbose = "silent")

   expect_equal(env$x, 99)
})

test_that("with_verbosity suppresses messages when verbose = `warn`", {
   expect_silent(
      with_verbosity({
         message("this should not print")
      }, verbose = "warn")
   )
})

test_that("with_verbosity does not suppress warnings when verbose = `warn`", {
   expect_warning(
      with_verbosity({
         warning("this should print")
      }, verbose = "warn")
   )
})

test_that("with_verbosity suppresses cli output when verbose = `warn`", {
   skip_if_not_installed("cli")

   expect_silent(
      with_verbosity({
         cli::cli_alert_info("cli output should not print")
         cli::cli_rule("Suppressed rule")
         cli::cli_bullets(c("• Bullet should be suppressed"))
      }, verbose = "warn")
   )
})

test_that("with_verbosity does not suppress errors when verbose = `warn`", {
   expect_error(
      with_verbosity({
         stop("This error must propagate")
      }, verbose = "warn"),
      "This error must propagate"
   )
})

test_that("with_verbosity still evaluates side-effect code when verbose = `warn`", {
   env <- new.env(parent = emptyenv())
   env$x <- 0

   with_verbosity({
      env$x <- 99
   }, verbose = "warn")

   expect_equal(env$x, 99)
})

test_that("with_verbosity does not suppress messages when verbose = `message`", {
   expect_message(
      with_verbosity({
         message("This message should print")
      }, verbose = "message")
   )
})

test_that("with_verbosity does not suppress warnings when verbose = `message`", {
   expect_warning(
      with_verbosity({
         warning("This warning should print")
      }, verbose = "message")
   )
})

test_that("with_verbosity does not suppress cli output when verbose = `message`", {
   skip_if_not_installed("cli")

   expect_message(
      with_verbosity({
         cli::cli_alert_info("cli output should be printed")
      }, verbose = "message"),
      "cli output should be printed"
   )

   expect_message(
      with_verbosity({
         cli::cli_rule("Horizontal rule should be printed")
      }, verbose = "message"),
      "Horizontal rule should be printed"
   )

   expect_message(
      with_verbosity({
         cli::cli_bullets(c("• Bullet should be printed"))
      }, verbose = "message"),
      "• Bullet should be printed"
   )
})

test_that("with_verbosity does not suppress errors when verbose = `message`", {
   expect_error(
      with_verbosity({
         stop("This error must propagate")
      }, verbose = "message"),
      "This error must propagate"
   )
})

test_that("with_verbosity evaluates expr normally when verbose = `message`", {
   result <- with_verbosity({ 1 + 1 }, verbose = "message")
   expect_equal(result, 2)

   expect_message(
      x <- with_verbosity({
         message("This should print normally")
         123
      }, verbose = "message")
   )

   expect_equal(x, 123)
})

test_that("validate_verbose accepts valid inputs without error", {
   # Test each valid choice
   expect_no_error(validate_verbose("message"))
   expect_no_error(validate_verbose("warn"))
   expect_no_error(validate_verbose("collapse"))
   expect_no_error(validate_verbose("silent"))
})

test_that("validate_verbose allows partial matching for valid inputs", {
   # match.arg defaults to allowing partial matching if unambiguous
   expect_no_error(validate_verbose("mess")) # Should resolve to "message"
   expect_no_error(validate_verbose("war"))  # Should resolve to "warn"
   expect_no_error(validate_verbose("col"))  # Should resolve to "collapse"
   expect_no_error(validate_verbose("sil"))  # Should resolve to "silent"
})

test_that("validate_verbose throws error for invalid string input", {
   # Test with a string that is not one of the choices
   expect_error(
      lifecycle::expect_deprecated(
         spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = "invalid")
      )
   )
   # Test with another invalid string
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = "error")
   )
})

test_that("validate_verbose throws error for incorrect data type inputs", {
   # Numeric input
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = 1)
   )
   # Logical input
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = TRUE)
   )
   # NA_character_ input (match.arg expects a character string)
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = NA_character_)
   )
})

test_that("validate_verbose throws error for vector input", {
   # match.arg expects a single string by default
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = c("message", "warn"))
   )
   # Empty character vector
   expect_error(
      spec_to_metacore(metacore_example("p21_mock.xlsx"), quiet = TRUE, verbose = character(0))
   )
})

test_that("with_verbosity collapses warnings into a single message", {
   # Test single warning
   expect_message(
      res <- with_verbosity({
         warning("This warning should be collapsed")
         1 + 1
      }, verbose = "collapse"),
      regexp = cli_inform("Operation performed with 1 suppressed warning. Set `verbose = \"warn\"` to show.")
   )

   # Test multiple warnings
   expect_message(
      with_verbosity({
         warning("This warning should be collapsed")
         warning("This warning should be collapsed")
         warning("This warning should be collapsed")
         1 + 1
      }, verbose = "collapse"),
      regexp = cli_inform("Operation performed with 3 suppressed warnings. Set `verbose = \"warn\"` to show.")
   )
})
