# # where should this function go
# empty_df <- function(nms, fill) {
#    df <- as.data.frame(matrix(fill,1,length(nms)))
#    names(df) <- nms
#    return(df)
# }
#
# dfs <- purrr::map(col_vars(), ~ empty_df(.x, fill = "A")) %>%
#    setNames(c("ds_spec",
#               "ds_vars",
#               "var_spec",
#               "value_spec",
#               "derivations",
#               "code_list",
#               "changelog"))
#
# test_that("metacore init adds labels to column names", {
#    MetaCore_public <- R6Class("Init",
#       public = list(
#          initialize = MetaCore_initialize,
#          print = MetaCore_print,
#          validate =  MetaCore_validate,
#          metacore_filter = MetaCore_filter
#       ))
# })
