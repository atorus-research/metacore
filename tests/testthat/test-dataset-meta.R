dm_base <- select_dataset(mc_original, "DM", verbose = "silent")
dm_define <- select_dataset(mc_define, "DM", verbose = "silent")

# DatasetMeta print -------------------------------------------------------

test_that("DatasetMeta print snapshot", {
  withr::local_options(cli.num_colors = 0)
  expect_snapshot(print(dm_base))
})

# DatasetMetaDefine print -------------------------------------------------

test_that("DatasetMetaDefine print snapshot", {
  withr::local_options(cli.num_colors = 0)
  expect_snapshot(print(dm_define))
})
