#' Write metacore object to JSON file (export/viewing only)
#'
#' @param metacore A metacore object
#' @param path File path for the JSON output
#' @param pretty Logical indicating whether to use pretty formatting (default TRUE)
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @note JSON export is for version control visibility and human readability only.
#'   The JSON file CANNOT be reliably loaded back into R as a functional metacore object
#'   due to R6 serialization limitations. For saving/loading metacore objects in R,
#'   use save_metacore() and load_metacore() which use RDS format.
#'
#' @examples
#' \dontrun{
#' mc <- spec_to_metacore("specs.xlsx")
#' 
#' # For version control (human-readable, commit to git)
#' write_metadata_json(mc, "specs.json")
#' 
#' # For programmatic use (load in R scripts)
#' save_metacore(mc, "specs.rds")
#' }
write_metadata_json <- function(metacore, path, pretty = TRUE) {
  
  if (!inherits(metacore, "Metacore")) {
    cli::cli_abort(c(
      "{.arg metacore} must be a Metacore object",
      "i" = "Current class: {.cls {class(metacore)}}"
    ))
  }
  
  metacore_list <- list(
    ds_spec = metacore$ds_spec,
    ds_vars = metacore$ds_vars,
    var_spec = metacore$var_spec,
    value_spec = metacore$value_spec,
    derivations = metacore$derivations,
    codelist = metacore$codelist,
    supp = metacore$supp
  )
  
  metacore_list <- metacore_list[!sapply(metacore_list, is.null)]
  
  json_data <- jsonlite::toJSON(
    metacore_list,
    pretty = pretty,
    auto_unbox = FALSE,
    na = "null",
    null = "null",
    digits = NA
  )
  
  writeLines(json_data, con = path)
  
  cli::cli_alert_success("Metacore metadata exported to {.file {path}}")
  cli::cli_alert_warning("JSON is for viewing/version control only, not for loading in R")
  cli::cli_alert_info("For R use: {.run save_metacore(metacore, '{tools::file_path_sans_ext(path)}.rds')}")
  
  invisible(path)
}


#' Convert Excel spec to RDS and optionally JSON
#'
#' @param excel_path Path to Excel specification file
#' @param rds_path Path for RDS output (for R use)
#' @param json_path Optional path for JSON output (for version control/viewing)
#' @param quiet Logical indicating whether to suppress messages (default FALSE)
#'
#' @return A metacore object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create both formats
#' mc <- excel_to_metacore_files(
#'   "specs.xlsx", 
#'   rds_path = "specs.rds",
#'   json_path = "specs.json"
#' )
#' 
#' # Later, load from RDS
#' mc <- load_metacore("specs.rds")
#' }
excel_to_metacore_files <- function(excel_path, rds_path, json_path = NULL, quiet = FALSE) {
  
  # Read from Excel
  mc <- metacore::spec_to_metacore(excel_path, quiet = quiet)
  
  # Save as RDS (for programmatic use)
  metacore::save_metacore(mc, rds_path)
  
  if (!quiet) {
    cli::cli_alert_success("Saved RDS to {.file {rds_path}}")
  }
  
  # Optionally save as JSON (for version control/viewing)
  if (!is.null(json_path)) {
    write_metadata_json(mc, json_path, pretty = TRUE)
  }
  
  invisible(mc)
}