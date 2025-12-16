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
test_that("quiet_if_true returns expression result when quiet = FALSE", {
   result <- quiet_if_true({ 1 + 1 }, quiet = FALSE)
   expect_equal(result, 2)
})

test_that("quiet_if_true suppresses messages when quiet = TRUE", {
   expect_silent(
      quiet_if_true({
         message("this should not print")
         10
      }, quiet = TRUE)
   )
})

test_that("quiet_if_true suppresses warnings when quiet = TRUE", {
   expect_silent(
      quiet_if_true({
         warning("this should not print")
         5
      }, quiet = TRUE)
   )
})

test_that("quiet_if_true suppresses cli output when quiet = TRUE", {
   skip_if_not_installed("cli")

   expect_silent(
      quiet_if_true({
         cli::cli_alert_info("cli output should not print")
         cli::cli_rule("Suppressed rule")
         cli::cli_bullets(c("• Bullet should be suppressed"))
         42
      }, quiet = TRUE)
   )
})

test_that("quiet_if_true does not suppress errors when quiet = TRUE", {
   expect_error(
      quiet_if_true({
         stop("this error must propagate")
      }, quiet = TRUE),
      "this error must propagate"
   )
})

test_that("quiet_if_true still evaluates side-effect code when quiet = TRUE", {
   env <- new.env(parent = emptyenv())
   env$x <- 0

   quiet_if_true({
      env$x <- 99
   }, quiet = TRUE)

   expect_equal(env$x, 99)
})

test_that("quiet_if_true evaluates expr normally when quiet = FALSE", {
   x <- quiet_if_true({
      message("this should print normally")
      123
   }, quiet = FALSE)

   expect_equal(x, 123)
})
