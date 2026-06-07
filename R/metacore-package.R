#' @keywords internal
#' @family internal
#'
#' @import R6
#'
#' @importFrom tidyr replace_na nest unnest
#'
#' @importFrom tidyselect everything matches
#'
#' @importFrom dplyr all_of any_of right_join n_distinct row_number case_when n if_else arrange
#' @importFrom dplyr rowwise anti_join inner_join bind_rows distinct mutate full_join
#' @importFrom dplyr left_join group_by summarise filter pull select group_split ungroup
#' @importFrom dplyr starts_with if_all
#'
#' @importFrom tibble tibble tribble as_tibble
#'
#' @importFrom stringr str_replace str_count str_remove_all str_remove str_detect
#' @importFrom stringr str_subset str_length str_replace_na str_replace_all str_extract
#' @importFrom stringr str_c str_trim str_split str_starts regex
#'
#' @importFrom rlang !! as_string expr set_names sym prim_name inherits_only
#'
#' @importFrom purrr keep discard reduce compact imap map map_chr map_lgl map_dfr
#' @importFrom purrr map_int map2_lgl map2_chr pmap_chr safely
#'
#' @importFrom stats var na.omit
#'
#' @importFrom readxl excel_sheets read_excel
#'
#' @importFrom cli ansi_collapse cli_abort cli_inform cli_alert_success cli_alert_info
#' @importFrom cli cli_warn cli_bullets cli_div cli_end cli_par cli_rule cli_text
#' @importFrom cli col_red qty
#'
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attr xml_ns_strip
#' @importFrom xml2 xml_text
#'
#' @importFrom lifecycle deprecated deprecate_soft
#'
"_PACKAGE"
