test_df <- data.frame(
   a = c("blah", "bloop"), c = c("SpecificWord", "Incorrect")
)

test_that("check_structure function works", {
  expect_true(test_df %>% check_structure(a, is.character))
  expect_false(test_df %>% check_structure(a, is.logical))
  expect_true(test_df %>% check_structure(c, check_words("SpecificWord|Incorrect")))
})

test_that("check_words creates function", {
   expect_type(check_words("SpecificWord"), "language")
})
