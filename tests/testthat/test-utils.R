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
