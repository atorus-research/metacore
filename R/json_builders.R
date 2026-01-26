#' Write metacore object to JSON file (for version control/viewing only)
#'
#' @param metacore A metacore object
#' @param path File path for the JSON output
#' @param pretty Logical indicating whether to use pretty formatting (default TRUE)
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @note JSON export is primarily for version control and human readability.
#'   For full fidelity preservation of the metacore object, use save_metacore()
#'   which saves as RDS format.
#'
#' @examples
#' \dontrun{
#' mc <- spec_to_metacore("specs.xlsx")
#' write_metadata_json(mc, "specs.json")
#' }
write_metadata_json <- function(metacore, path, pretty = TRUE) {
  
  # Validate input
  if (!inherits(metacore, "Metacore")) {
    cli::cli_abort(c(
      "{.arg metacore} must be a Metacore object",
      "i" = "Current class: {.cls {class(metacore)}}"
    ))
  }
  
  # Extract all components using active bindings
  metacore_list <- list(
    ds_spec = metacore$ds_spec,
    ds_vars = metacore$ds_vars,
    var_spec = metacore$var_spec,
    value_spec = metacore$value_spec,
    derivations = metacore$derivations,
    codelist = metacore$codelist,
    supp = metacore$supp
  )
  
  # Remove NULL components
  metacore_list <- metacore_list[!sapply(metacore_list, is.null)]
  
  # Convert to JSON
  json_data <- jsonlite::toJSON(
    metacore_list,
    pretty = pretty,
    auto_unbox = FALSE,
    na = "null",
    null = "null",
    digits = NA
  )
  
  # Write to file
  writeLines(json_data, con = path)
  
  cli::cli_alert_success("Metacore object written to {.file {path}}")
  cli::cli_alert_info("For programmatic use, consider {.fn save_metacore} for RDS format")
  
  invisible(path)
}


#' Read metacore object from JSON file and save as RDS
#'
#' @param json_path File path for the JSON input
#' @param rds_path Optional file path for RDS output. If NULL, uses same name as JSON with .rds extension
#' @param quiet Logical indicating whether to suppress messages (default FALSE)
#'
#' @return A metacore object loaded from the RDS file
#' @export
#'
#' @examples
#' \dontrun{
#' mc <- read_metadata_json("specs.json")
#' }
read_metadata_json <- function(json_path, rds_path = NULL, quiet = FALSE) {
  
  # Check file exists
  if (!file.exists(json_path)) {
    cli::cli_abort("File {.file {json_path}} does not exist")
  }
  
  # Determine RDS path
  if (is.null(rds_path)) {
    rds_path <- sub("\\.json$", ".rds", json_path)
    if (rds_path == json_path) {
      rds_path <- paste0(json_path, ".rds")
    }
  }
  
  # Read JSON file
  json_data <- readLines(json_path, warn = FALSE)
  
  # Parse JSON to list
  metacore_list <- jsonlite::fromJSON(
    json_data,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )
  
  # Helper function
  has_data <- function(x) {
    !is.null(x) && is.data.frame(x) && nrow(x) > 0
  }
  
  # Convert to tibbles
  for (component in names(metacore_list)) {
    if (!is.null(metacore_list[[component]]) && is.data.frame(metacore_list[[component]])) {
      metacore_list[[component]] <- tibble::as_tibble(metacore_list[[component]])
    }
  }
  
  # Fix types for each component with CORRECT types
  
  # ds_spec
  if (has_data(metacore_list$ds_spec)) {
    metacore_list$ds_spec <- metacore_list$ds_spec %>%
      dplyr::mutate(
        dataset = as.character(dataset),
        structure = as.character(structure),
        label = as.character(label)
      )
  }
  
  # ds_vars - FIX: order should be numeric, core should be character
  if (has_data(metacore_list$ds_vars)) {
    metacore_list$ds_vars <- metacore_list$ds_vars %>%
      dplyr::mutate(
        dataset = as.character(dataset),
        variable = as.character(variable),
        order = as.numeric(order),  # Changed from integer to numeric
        keep = as.logical(keep),
        key_seq = as.integer(key_seq),
        core = as.character(core),  # Changed from leaving as-is to character
        supp_flag = as.logical(supp_flag)
      )
  }
  
  # var_spec
  if (has_data(metacore_list$var_spec)) {
    metacore_list$var_spec <- metacore_list$var_spec %>%
      dplyr::mutate(
        variable = as.character(variable),
        label = as.character(label),
        length = as.integer(length),
        type = as.character(type),
        common = as.character(common),
        format = as.character(format)
      )
  }
  
  # value_spec
  if (has_data(metacore_list$value_spec)) {
    metacore_list$value_spec <- metacore_list$value_spec %>%
      dplyr::mutate(
        dataset = as.character(dataset),
        variable = as.character(variable),
        where = as.character(where),
        type = as.character(type),
        sig_dig = as.integer(sig_dig),
        code_id = as.character(code_id),
        origin = as.character(origin),
        derivation_id = suppressWarnings(as.integer(derivation_id))
      )
  }
  
  # derivations
  if (has_data(metacore_list$derivations)) {
    metacore_list$derivations <- metacore_list$derivations %>%
      dplyr::mutate(
        derivation_id = suppressWarnings(as.integer(derivation_id)),
        derivation = as.character(derivation)
      )
  }
  
  # codelist
  if (has_data(metacore_list$codelist)) {
    metacore_list$codelist <- metacore_list$codelist %>%
      dplyr::mutate(
        code_id = as.character(code_id),
        name = as.character(name),
        type = as.character(type)
      )
    
    # Convert codes column elements to tibbles
    if ("codes" %in% names(metacore_list$codelist)) {
      metacore_list$codelist$codes <- lapply(metacore_list$codelist$codes, function(x) {
        if (is.data.frame(x)) tibble::as_tibble(x) else x
      })
    }
  }
  
  # supp
  if (has_data(metacore_list$supp)) {
    metacore_list$supp <- metacore_list$supp %>%
      dplyr::mutate(
        dataset = as.character(dataset),
        variable = as.character(variable),
        idvar = as.character(idvar),
        qeval = as.character(qeval)
      )
  }
  
  # Create metacore object
  metacore_obj <- suppressWarnings(
    metacore::metacore(
      ds_spec = metacore_list$ds_spec %||% tibble::tibble(),
      ds_vars = metacore_list$ds_vars %||% tibble::tibble(),
      var_spec = metacore_list$var_spec %||% tibble::tibble(),
      value_spec = metacore_list$value_spec %||% tibble::tibble(),
      derivations = metacore_list$derivations %||% tibble::tibble(),
      codelist = metacore_list$codelist %||% tibble::tibble(),
      supp = metacore_list$supp %||% tibble::tibble(),
      quiet = TRUE
    )
  )
  
  # Save as RDS to preserve R6 object properly
  metacore::save_metacore(metacore_obj, rds_path)
  
  # Load from RDS (this ensures proper R6 reconstruction)
  metacore_final <- metacore::load_metacore(rds_path)
  
  if (!quiet) {
    cli::cli_alert_success("Metacore object loaded from {.file {json_path}}")
    cli::cli_alert_info("Intermediate RDS saved to {.file {rds_path}}")
  }
  
  return(metacore_final)
}

# Helper
`%||%` <- function(x, y) if (is.null(x)) y else x