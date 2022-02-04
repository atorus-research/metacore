test_df <- data.frame(
   a = c("blah", NA),
   b = c("SpecificWord", "Incorrect")
)

test_that("check_structure function works", {
  # error when NA not permissible

  expect_equal(check_structure(test_df, "a", is.character, FALSE, "test_df")$error,
               "a from the test_df table contains missing values. Actual values are needed.")

  expect_equal(check_structure(test_df, "a", is.logical, TRUE, "test_df")$warning,
               "test_df$a fails is.logical check \n"
               )

  expect_null(check_structure(test_df, "b", check_words("SpecificWord", "Incorrect"), TRUE, "test_df")$warning)
  expect_equal(check_structure(test_df, "b", check_words("SpecificWord"), TRUE, "test_df")$warning,
               "The following words in test_df$b are not allowed: \n    Incorrect\n")

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
               sort(c("ADaM_define.xml", "mock_spec.xlsx", "p21_mock.xlsx", "pilot_ADaM.rda",
                      "SDTM_define.xml", "SDTM_spec_CDISC_pilot.xlsx")))
})
