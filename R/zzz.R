#' @import R6
#' @importFrom tidyr replace_na nest unnest
#' @importFrom tidyselect everything matches
#' @importFrom dplyr right_join n_distinct row_number case_when n if_else arrange rowwise anti_join inner_join bind_rows distinct mutate full_join left_join group_by summarise filter pull select group_split ungroup
#' @importFrom tibble tibble tribble
#' @importFrom stringr str_replace str_count str_remove_all str_remove str_detect str_subset str_length str_replace_na str_replace_all str_extract str_c str_trim str_split str_starts regex
#' @importFrom rlang !! as_string expr sym prim_name
#' @importFrom purrr keep discard reduce compact map map_chr map_lgl map_dfr map_int map2_lgl map2_chr pmap_chr
#' @importFrom stats var na.omit
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tibble tibble as_tibble
#' @importFrom readxl excel_sheets read_excel

NULL

globalVariables(c("private",
                  "self",
                  "keep",
                  ".",
                  ".data",
                  "code",
                  "code_id",
                  "codes",
                  "core",
                  "dataset",
                  "decode",
                  "derivation_id",
                  "dictionary",
                  "id",
                  "key_seq",
                  "lab",
                  "label",
                  "matches",
                  "spec_type_to_code_list",
                  "type",
                  "variable",
                  "where",
                  "where_new"))
