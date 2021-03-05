test_df <- data.frame(
   a = c("blah", "bloop"),
   b = c("SpecificWord", "Incorrect")
)

test_that("check_structure function works", {
  expect_true(check_structure(test_df, a, is.character))
  expect_false(check_structure(test_df, a, is.logical))
  expect_true(check_structure(test_df, check_words("SpecificWord|Incorrect")))
})

test_that("check_words creates function", {
   is.function(check_words("word"))
})

make_function <- function(args = pairlist(), body, env = parent.frame())  {
  eval(call("function", args, body), env)
}
