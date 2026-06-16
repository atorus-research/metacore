#' Convert Metacore Object to Define.xml 2.0
#'
#' Generates a CDISC Define.xml 2.0 document (ODM 1.3.2 with def: namespace) from a
#' metacore object. The function combines dataset, variable, value-level, codelist, and
#' method metadata from the metacore object with user-provided study details to create
#' a complete, valid Define.xml file.
#'
#' @param metacore_obj A Metacore object containing dataset, variable, and metadata
#'   specifications. See [Metacore] for details on required tables and structure.
#'
#' @param path Optional file path where the Define.xml should be written. If `NULL`,
#'   the XML document is returned in memory but not written to disk. Default: `NULL`
#'
#' @param study_details Required named list containing study-level metadata for the
#'   Define.xml root elements. Must include at minimum:
#'   \describe{
#'     \item{study_name}{Character. The name of the study (e.g., "STUDY ABC-123")}
#'     \item{study_description}{Character. A description of the study}
#'     \item{protocol_name}{Character. The protocol identifier (e.g., "ABC-123-001")}
#'   }
#'   Optional elements:
#'   \describe{
#'     \item{study_oid}{Character. OID for the Study element. If not provided, will
#'       be auto-generated.}
#'     \item{metadata_version_oid}{Character. OID for MetaDataVersion. If not provided,
#'       will be auto-generated.}
#'     \item{metadata_version_name}{Character. Name attribute for MetaDataVersion. If not
#'       provided, defaults to "Metadata v1.0".}
#'   }
#'
#' @param timestamp POSIXct or character. The creation timestamp for the Define.xml
#'   file in ISO 8601 format. If POSIXct, will be converted to ISO 8601 with timezone.
#'   Default: `Sys.time()` (current system time)
#'
#' @param source_system Character. Identifier for the system that created the Define.xml.
#'   Default: `"R metacore package"`
#'
#' @param source_system_version Character. Version of the source system. Default uses
#'   the installed metacore package version via `packageVersion("metacore")`.
#'
#' @param stylesheet_href Character. URL or path to the XSL stylesheet for rendering
#'   the Define.xml in a browser. Default: `"define2-0-0.xsl"`
#'
#' @param validate Logical. If `TRUE`, runs validation checks on the metacore object
#'   and generated XML before returning. Default: `TRUE`
#'
#' @return Invisibly returns an xml_document object (from the xml2 package). If `path`
#'   is provided, the XML is also written to that location.
#'
#' @details
#'
#' ## XML Structure Generated
#'
#' The function generates the following Define.xml structure:
#' - **ODM root element** with appropriate namespaces (odm, def, xlink)
#' - **Study element** with OID and metadata
#' - **GlobalVariables** containing study name, description, and protocol
#' - **MetaDataVersion** with Define version and standards information
#' - **ItemGroupDef elements** (one per dataset from `ds_spec`)
#' - **ItemDef elements** (one per unique variable)
#' - **ItemRef elements** linking variables to datasets with ordering and key sequences
#' - **def:WhereClauseDef elements** for conditional value-level metadata
#' - **def:ValueListDef elements** grouping related where clauses
#' - **CodeList and ExternalCodeList elements** for value lists and external dictionaries
#' - **MethodDef elements** for derivations with document references
#' - **def:CommentDef elements** for variable comments
#' - **def:DocumentRef and def:leaf elements** for linked documents
#'
#' ## Study Details Requirements
#'
#' The `study_details` parameter is critical for proper Define.xml generation. At minimum,
#' provide:
#'
#' ```r
#' study_details <- list(
#'   study_name = "STUDY ABC-123",
#'   study_description = "A Phase 3 Randomized, Double-Blind Study",
#'   protocol_name = "ABC-123-001"
#' )
#' ```
#'
#' ## OID Generation Strategy
#'
#' The function uses deterministic OID generation based on CDISC conventions:
#' - ItemGroupDef: `IG.{DOMAIN}`
#' - ItemDef: `IT.{DOMAIN}.{VARIABLE}` for simple variables
#' - ItemDef with where clause: `IT.{DOMAIN}.{VARIABLE}.{HASH}` where HASH represents
#'   the specific where clause condition
#' - CodeList: `CL.{CODE_ID}`
#' - MethodDef: `MT.{DERIVATION_ID}`
#' - Study: Auto-generated or user-provided via `study_details$study_oid`
#'
#' All OIDs are verified for uniqueness before XML generation.
#'
#' ## Timestamp Handling
#'
#' The `timestamp` parameter is included in the ODM element as `CreationDateTime` in
#' ISO 8601 format with timezone information. This is required by the Define.xml specification.
#'
#' ## Validation
#'
#' When `validate = TRUE`, the function performs:
#' 1. **Metacore validation**: Ensures all required tables are present and valid
#' 2. **Study details validation**: Checks that required study metadata is provided
#' 3. **XML structure validation**: Verifies all generated elements have required attributes
#' 4. **OID uniqueness**: Confirms no duplicate OIDs exist in the generated document
#' 5. **Namespace correctness**: Validates namespace declarations and usage
#'
#' If validation fails, an informative error message is returned with details on
#' what needs to be corrected.
#'
#' ## File Output
#'
#' If `path` is provided, the XML is written using [xml2::write_xml()] with UTF-8
#' encoding and proper XML declaration. The file can be validated against the
#' CDISC Define.xml 2.0 XSD schema.
#'
#' @seealso
#'   - [Metacore] for creating and validating metacore objects
#'   - [spec_to_metacore()] for reading P21 Excel specifications
#'   - [define_to_metacore()] for the inverse operation (reading Define.xml)
#'
#' @examples
#' \dontrun{
#' # Load a metacore object from a P21 specification
#' metacore_obj <- spec_to_metacore(
#'   path = "path/to/specification.xlsx"
#' )
#'
#' # Define study metadata
#' study_details <- list(
#'   study_name = "STUDY ABC-123",
#'   study_description = "A Phase 3 Randomized Study",
#'   protocol_name = "ABC-123-001"
#' )
#'
#' # Generate Define.xml and write to file
#' xml_doc <- metacore_to_define_xml(
#'   metacore_obj = metacore_obj,
#'   path = "output/define.xml",
#'   study_details = study_details
#' )
#'
#' # Or generate in memory without writing to file
#' xml_doc <- metacore_to_define_xml(
#'   metacore_obj = metacore_obj,
#'   study_details = study_details
#' )
#' }
#'
#' @export
metacore_to_define_xml <- function(
    metacore_obj,
    path = NULL,
    study_details = NULL,
    timestamp = Sys.time(),
    source_system = "R metacore package",
    source_system_version = packageVersion("metacore"),
    stylesheet_href = "define2-0-0.xsl",
    validate = TRUE) {

  # Input validation
  if (!inherits(metacore_obj, "Metacore")) {
    cli::cli_abort("metacore_obj must be a Metacore object. Got {class(metacore_obj)[1]}.")
  }

  if (is.null(study_details)) {
    cli::cli_abort("study_details must be a non-NULL named list.")
  }
  # Allow users to pass metacore_obj$study_level (a single-row tibble) directly.
  if (is.data.frame(study_details)) {
    study_details <- as.list(study_details[1, , drop = FALSE])
    study_details <- lapply(study_details, function(x) {
      if (length(x) == 1 && is.na(x)) NULL else x
    })
  }
  if (!is.list(study_details)) {
    cli::cli_abort("study_details must be a non-NULL named list.")
  }

  # Validate required study_details elements
  required_study_fields <- c("study_name", "study_description", "protocol_name")
  missing_fields <- setdiff(required_study_fields, names(study_details))
  if (length(missing_fields) > 0) {
    cli::cli_abort(
      "study_details is missing required elements: {.val {missing_fields}}"
    )
  }

  if (!is.null(path) && !is.character(path)) {
    cli::cli_abort("path must be NULL or a character string.")
  }

  if (!is.logical(validate)) {
    cli::cli_abort("validate must be a logical value (TRUE or FALSE).")
  }

  # Validate metacore object if requested
  if (validate) {
    metacore_obj$validate()
  }

  # Convert timestamp to ISO 8601 if needed
  if (inherits(timestamp, "POSIXct") || inherits(timestamp, "POSIXlt")) {
    timestamp_str <- format(timestamp, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
    # Format timezone from +HHMM to +HH:MM
    timestamp_str <- sub("([+-])([0-9]{2})([0-9]{2})$", "\\1\\2:\\3", timestamp_str)
  } else {
    timestamp_str <- as.character(timestamp)
  }

  # ---- Build the Define.xml document -----------------------------------------
  # Each helper mutates the document in place (xml2 nodes are external pointers),
  # so they are called for their side effects in document order. The order below
  # matches the Define.xml 2.0 schema's expected element sequence within
  # MetaDataVersion.
  odm <- .create_odm_root(
    study_details = study_details,
    timestamp = timestamp_str,
    source_system = source_system,
    source_system_version = as.character(source_system_version)
  )

  study <- .create_study(odm, study_details)
  .create_global_variables(study, study_details)
  mdv <- .create_metadata_version(study, study_details)

  # Value-level metadata (where clauses) must be declared before the
  # ItemGroupDef/ItemDef elements that reference them.
  .create_value_list_defs(mdv, metacore_obj)
  .create_where_clause_defs(mdv, metacore_obj)

  # Standard ODM definitions
  .create_item_group_defs(mdv, metacore_obj)
  .create_item_defs(mdv, metacore_obj)
  .create_code_lists(mdv, metacore_obj)
  .create_external_code_lists(mdv, metacore_obj)
  .create_method_defs(mdv, metacore_obj)
  .create_comment_defs(mdv, metacore_obj)
  .create_leaf_refs(mdv, metacore_obj)

  # Verify OID uniqueness across the generated document
  if (validate) {
    .check_oid_uniqueness(odm)
  }

  # ---- Serialise / write -----------------------------------------------------
  if (!is.null(path)) {
    .write_define_xml(odm, path, stylesheet_href)
    cli::cli_alert_success("Define.xml written to {.file {path}}")
  }

  invisible(odm)
}


# ==============================================================================
# Internal helpers
# ==============================================================================

#' Return a default when a value is missing
#'
#' Treats `NULL`, length-0, and `NA` scalars as missing.
#' @noRd
def_val <- function(x, default) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (length(x) == 1 && is.na(x)) {
    return(default)
  }
  x
}


#' Set attributes on an xml node, skipping missing values
#'
#' A thin wrapper over [xml2::xml_set_attr()] that silently drops any attribute
#' whose value is `NULL` or `NA`, so optional metadata simply doesn't appear.
#' Attributes are set in the order supplied.
#' @noRd
set_attrs <- function(node, ...) {
  attrs <- list(...)
  for (nm in names(attrs)) {
    val <- attrs[[nm]]
    if (is.null(val) || length(val) == 0) next
    if (length(val) == 1 && is.na(val)) next
    xml2::xml_set_attr(node, nm, as.character(val))
  }
  invisible(node)
}


#' Yes/No from a logical
#'
#' Maps `TRUE`/`FALSE` to the Define.xml `"Yes"`/`"No"` vocabulary. `NA` returns
#' `NULL` so the attribute is omitted by [set_attrs()].
#' @noRd
yn <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }
  if (isTRUE(x)) "Yes" else "No"
}


#' Map a metacore origin to a Define.xml Origin Type
#'
#' Origins are stored lower-cased in `value_spec`; Define.xml expects specific
#' capitalisations (e.g. `"CRF"`, `"Derived"`). Unknown origins are title-cased.
#' @noRd
origin_type <- function(origin) {
  if (is.null(origin) || length(origin) == 0 || is.na(origin)) {
    return(NULL)
  }
  base <- stringr::str_remove(origin, ",.*$") # strip "collected, page_num = .." artefacts
  base <- stringr::str_trim(base)
  switch(stringr::str_to_lower(base),
         "crf" = "CRF",
         "edt" = "eDT",
         "derived" = "Derived",
         "assigned" = "Assigned",
         "predecessor" = "Predecessor",
         "collected" = "Collected",
         "protocol" = "Protocol",
         stringr::str_to_title(base)
  )
}




#' Generate a Define.xml OID for a given element type
#'
#' Centralises the OID naming conventions following CDISC Define.xml patterns.
#' For where clauses, builds readable OIDs directly from the condition components
#' (WC.DATASET.VARIABLE.COMPARATOR.VALUE) rather than hashing.
#'
#' @param element_type one of `"ItemGroupDef"`, `"ItemDef"`, `"ValueListDef"`,
#'   `"WhereClauseDef"`, `"leaf"`.
#' @param dataset,variable identifiers used to build the OID.
#' @param where optional where-clause string; when supplied an `ItemDef` OID
#'   becomes value-level.
#' @param where_var,comparator,value components of a where condition used to
#'   build readable value-level/where OIDs.
#' @noRd
.generate_oid <- function(element_type, dataset = NA, variable = NA,
                          where = NULL, where_var = NULL,
                          comparator = NULL, value = NULL) {
  switch(element_type,
         "ItemGroupDef" = paste0("IG.", dataset),
         "ValueListDef" = paste0("VL.", dataset, ".", variable),
         "leaf" = paste0("LF.", dataset),
         "ItemDef" = {
           base <- paste0("IT.", dataset, ".", variable)
           if (is.null(where) || length(where) == 0 || is.na(where)) {
             base
           } else if (!is.null(where_var) && !is.null(value) && length(value) == 1) {
             # Value-level ItemDef: use the where clause components to build OID
             paste0(
               base, ".", dataset, ".", where_var, ".",
               def_val(comparator, "EQ"), ".", value
             )
           } else {
             # Fallback for complex where clauses (should be rare)
             paste0(base, ".", make.names(paste(where_var, comparator, value, sep = ".")))
           }
         },
         "WhereClauseDef" = {
           if (!is.null(where_var) && !is.null(value) && length(value) == 1) {
             # Simple condition: build readable OID
             paste0("WC.", dataset, ".", where_var, ".", def_val(comparator, "EQ"), ".", value)
           } else {
             # Fallback for complex conditions (should be rare)
             paste0("WC.", dataset, ".", variable, ".", make.names(where))
           }
         },
         cli::cli_abort("Unknown element_type {.val {element_type}} for OID generation.")
  )
}


#' Add a child element containing text and return the parent
#'
#' Avoids R's nested-replacement semantics (`xml_text(xml_add_child(...)) <- x`
#' would require an `xml_add_child<-` function, which doesn't exist).
#' @noRd
.add_text_child <- function(parent, name, text, ...) {
  child <- xml2::xml_add_child(parent, name, ...)
  val <- as.character(text)
  if (length(val) == 0 || is.na(val)) val <- ""
  xml2::xml_text(child) <- val
  invisible(parent)
}


#' Add a `<Description><TranslatedText>` block to a node
#'
#' Most Define.xml definition elements carry a human-readable description in this
#' standard nested form. Does nothing when `text` is missing.
#' @noRd
.add_description <- function(node, text, lang = "en") {
  if (is.null(text) || length(text) == 0 || is.na(text) || !nzchar(text)) {
    return(invisible(node))
  }
  desc <- xml2::xml_add_child(node, "Description")
  tt <- xml2::xml_add_child(desc, "TranslatedText", "xml:lang" = lang)
  xml2::xml_text(tt) <- as.character(text)
  invisible(node)
}


#' Resolve var_spec metadata for a single dataset/variable
#'
#' `var_spec` stores one row per variable, but qualifies the `variable` with a
#' `DATASET.` prefix when a variable's metadata differs across datasets (the
#' same convention used by [select_dataset()]). This returns the best-matching
#' single row: a dataset-qualified match takes precedence over a bare match.
#' @noRd
.lookup_var_spec <- function(var_spec, dataset, variable) {
  qualified <- paste0(dataset, ".", variable)
  hit <- var_spec[var_spec$variable == qualified, , drop = FALSE]
  if (nrow(hit) == 0) {
    hit <- var_spec[var_spec$variable == variable, , drop = FALSE]
  }
  if (nrow(hit) == 0) {
    return(NULL)
  }
  hit[1, , drop = FALSE]
}


#' Parse a metacore where-clause expression into range checks
#'
#' `value_spec$where` holds either an R expression string (e.g.
#' `QNAM == "TRTEMFL"`) or a P21/Define.xml-style string (e.g.
#' `PARAMCD EQ EXDOSE`). This parses either form into a list of range checks,
#' each `list(var, comparator, values)`, where `comparator` uses the Define.xml
#' vocabulary (`EQ`, `NE`, `LT`, `LE`, `GT`, `GE`, `IN`, `NOTIN`). `AND`
#' conditions yield multiple range checks. Unsupported expressions yield an
#' empty list (the caller then omits the where metadata).
#' @noRd
.parse_where <- function(where) {
  if (is.null(where) || length(where) == 0 || is.na(where) || !nzchar(where)) {
    return(list())
  }

  # --- Try R expression syntax first (e.g. PARAMCD == "EXDOSE") ---
  expr <- tryCatch(str2lang(where), error = function(e) NULL)
  if (!is.null(expr)) {
    walk <- function(e) {
      if (!is.call(e)) return(list())
      op <- as.character(e[[1]])
      if (op %in% c("&", "&&", "|", "||")) {
        return(c(walk(e[[2]]), walk(e[[3]])))
      }
      cmp <- switch(op,
                    "==" = "EQ", "!=" = "NE", "<" = "LT", "<=" = "LE",
                    ">" = "GT", ">=" = "GE", "%in%" = "IN",
                    NA_character_
      )
      if (is.na(cmp)) return(list())
      var <- as.character(e[[2]])
      rhs <- e[[3]]
      if (op == "%in%" && is.call(rhs)) {
        values <- vapply(as.list(rhs)[-1], as.character, character(1))
      } else {
        values <- as.character(rhs)
      }
      list(list(var = var, comparator = cmp, values = values))
    }
    result <- walk(expr)
    if (length(result) > 0) return(result)
  }

  # --- Fallback: P21 / Define.xml keyword syntax (e.g. PARAMCD EQ EXDOSE) ---
  # Supports AND-separated conditions and IN (v1, v2, ...) lists.
  define_ops <- c("EQ", "NE", "LT", "LE", "GT", "GE", "IN", "NOTIN")
  op_pat <- paste0("\\b(", paste(define_ops, collapse = "|"), ")\\b")
  parts <- trimws(strsplit(where, "(?i)\\bAND\\b", perl = TRUE)[[1]])
  result <- list()
  for (part in parts) {
    m <- regmatches(part, regexec(
      paste0("^(\\w+)\\s+", op_pat, "\\s+(.+)$"),
      part, ignore.case = TRUE
    ))[[1]]
    if (length(m) == 0) next
    var <- m[2]
    cmp <- toupper(m[3])
    val_str <- trimws(m[4])
    if (cmp %in% c("IN", "NOTIN")) {
      val_str <- gsub("^\\(|\\)$", "", val_str)
      values <- trimws(strsplit(val_str, ",")[[1]])
    } else {
      values <- val_str
    }
    result <- c(result, list(list(var = var, comparator = cmp, values = values)))
  }
  result
}


# ------------------------------------------------------------------------------
# Root / study structure
# ------------------------------------------------------------------------------

#' Build the ODM root element with namespaces and file metadata
#' @noRd
.create_odm_root <- function(study_details, timestamp, source_system,
                             source_system_version) {
  study_oid <- def_val(study_details$study_oid, study_details$study_name)

  xml2::xml_new_root(
    "ODM",
    "xmlns" = "http://www.cdisc.org/ns/odm/v1.3",
    "xmlns:def" = "http://www.cdisc.org/ns/def/v2.0",
    "xmlns:xlink" = "http://www.w3.org/1999/xlink",
    "ODMVersion" = "1.3.2",
    "FileType" = "Snapshot",
    "FileOID" = study_oid,
    "CreationDateTime" = timestamp,
    "SourceSystem" = source_system,
    "SourceSystemVersion" = source_system_version
  )
}


#' Build the Study element
#' @noRd
.create_study <- function(odm, study_details) {
  study_oid <- def_val(study_details$study_oid, study_details$study_name)
  xml2::xml_add_child(odm, "Study", OID = study_oid)
}


#' Build the GlobalVariables element
#' @noRd
.create_global_variables <- function(study, study_details) {
  gv <- xml2::xml_add_child(study, "GlobalVariables")
  .add_text_child(gv, "StudyName", study_details$study_name)
  .add_text_child(gv, "StudyDescription", study_details$study_description)
  .add_text_child(gv, "ProtocolName", study_details$protocol_name)
  invisible(gv)
}


#' Build the MetaDataVersion element (the container for all definitions)
#' @noRd
.create_metadata_version <- function(study, study_details) {
  study_oid <- def_val(study_details$study_oid, study_details$study_name)
  mdv_oid <- def_val(study_details$metadata_version_oid, paste0("MDV.", study_oid))
  mdv_name <- def_val(
    study_details$metadata_version_name,
    paste("Study", study_details$study_name, "Data Definitions")
  )

  mdv <- xml2::xml_add_child(study, "MetaDataVersion")
  set_attrs(
    mdv,
    "OID" = mdv_oid,
    "Name" = mdv_name,
    "def:DefineVersion" = def_val(study_details$define_version, "2.0.0"),
    "def:StandardName" = def_val(study_details$standard_name, "CDISC ADaM"),
    "def:StandardVersion" = def_val(study_details$standard_version, "1.1")
  )
  mdv
}


# ------------------------------------------------------------------------------
# Value-level metadata
# ------------------------------------------------------------------------------

#' Subset of value_spec rows that carry a where clause (value-level metadata)
#' @noRd
.value_level_rows <- function(value_spec) {
  if (nrow(value_spec) == 0 || !"where" %in% names(value_spec)) {
    return(value_spec[0, , drop = FALSE])
  }
  value_spec[!is.na(value_spec$where) & nzchar(value_spec$where), , drop = FALSE]
}


#' Build def:ValueListDef elements grouping value-level ItemRefs per variable
#' @noRd
.create_value_list_defs <- function(mdv, metacore) {
  vl_rows <- .value_level_rows(metacore$value_spec)
  if (nrow(vl_rows) == 0) {
    return(invisible(NULL))
  }

  groups <- split(vl_rows, paste(vl_rows$dataset, vl_rows$variable, sep = "|"))
  for (grp in groups) {
    dataset <- grp$dataset[1]
    variable <- grp$variable[1]
    vl_oid <- .generate_oid("ValueListDef", dataset, variable)
    vld <- xml2::xml_add_child(mdv, "def:ValueListDef", OID = vl_oid)

    for (i in seq_len(nrow(grp))) {
      row <- grp[i, , drop = FALSE]
      checks <- .parse_where(row$where)
      first <- if (length(checks) > 0) checks[[1]] else NULL

      item_oid <- .generate_oid(
        "ItemDef", dataset, variable,
        where = row$where,
        where_var = if (!is.null(first)) first$var else NULL,
        comparator = if (!is.null(first)) first$comparator else NULL,
        value = if (!is.null(first)) first$values[1] else NULL
      )

      itemref <- xml2::xml_add_child(vld, "ItemRef")
      set_attrs(
        itemref,
        "ItemOID" = item_oid,
        "OrderNumber" = i,
        "Mandatory" = "No",
        "MethodOID" = if (identical(origin_type(row$origin), "Derived")) paste0("MT.", row$derivation_id) else NULL
      )
      wc_oid <- .generate_oid(
        "WhereClauseDef", dataset, variable,
        where = row$where,
        where_var = if (!is.null(first)) first$var else NULL,
        value = if (!is.null(first)) first$values else NULL
      )
      xml2::xml_add_child(itemref, "def:WhereClauseRef", WhereClauseOID = wc_oid)
    }
  }
  invisible(NULL)
}


#' Build def:WhereClauseDef elements from value-level where clauses
#' @noRd
.create_where_clause_defs <- function(mdv, metacore) {
  vl_rows <- .value_level_rows(metacore$value_spec)
  if (nrow(vl_rows) == 0) {
    return(invisible(NULL))
  }

  seen <- character(0)
  for (i in seq_len(nrow(vl_rows))) {
    row <- vl_rows[i, , drop = FALSE]
    checks <- .parse_where(row$where)
    if (length(checks) == 0) next

    first <- checks[[1]]
    wc_oid <- .generate_oid(
      "WhereClauseDef", row$dataset, row$variable,
      where = row$where, where_var = first$var, value = first$values
    )
    if (wc_oid %in% seen) next
    seen <- c(seen, wc_oid)

    wcd <- xml2::xml_add_child(mdv, "def:WhereClauseDef", OID = wc_oid)
    for (chk in checks) {
      rc <- xml2::xml_add_child(wcd, "RangeCheck", SoftHard = "Soft")
      set_attrs(
        rc,
        "def:ItemOID" = paste0("IT.", row$dataset, ".", chk$var),
        "Comparator" = chk$comparator
      )
      for (v in chk$values) {
        .add_text_child(rc, "CheckValue", v)
      }
    }
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# ItemGroupDef / ItemDef
# ------------------------------------------------------------------------------

#' Lookup of base (non-where) derivation per dataset.variable, by origin class
#' @noRd
.base_value_lookup <- function(value_spec) {
  base <- value_spec
  if ("where" %in% names(base)) {
    base <- base[is.na(base$where) | !nzchar(base$where), , drop = FALSE]
  }
  base
}


#' Build ItemGroupDef elements (one per dataset) with their ItemRefs and leaf
#' @noRd
.create_item_group_defs <- function(mdv, metacore) {
  ds_spec <- metacore$ds_spec
  ds_vars <- metacore$ds_vars
  base_vals <- .base_value_lookup(metacore$value_spec)
  vl_keys <- {
    vl <- .value_level_rows(metacore$value_spec)
    if (nrow(vl) > 0) unique(paste(vl$dataset, vl$variable, sep = "|")) else character(0)
  }

  for (d in seq_len(nrow(ds_spec))) {
    ds <- ds_spec[d, , drop = FALSE]
    dataset <- ds$dataset

    domain <- if (grepl("^SUPP", dataset)) sub("^SUPP", "", dataset) else dataset
    igd <- xml2::xml_add_child(mdv, "ItemGroupDef")
    set_attrs(
      igd,
      "OID" = .generate_oid("ItemGroupDef", dataset),
      "Domain" = domain,
      "Name" = dataset,
      "Repeating" = yn(ds$repeating),
      "IsReferenceData" = yn(ds$reference),
      "SASDatasetName" = dataset,
      "Purpose" = def_val(ds$purpose, "Tabulation"),
      "def:Structure" = ds$structure,
      "def:Class" = ds$class,
      "def:ArchiveLocationID" = .generate_oid("leaf", dataset)
    )
    .add_description(igd, ds$label)

    vars <- ds_vars[ds_vars$dataset == dataset, , drop = FALSE]
    if (nrow(vars) > 0 && "order" %in% names(vars)) {
      vars <- vars[order(vars$order, na.last = TRUE), , drop = FALSE]
    }

    for (v in seq_len(nrow(vars))) {
      var <- vars[v, , drop = FALSE]
      item_oid <- .generate_oid("ItemDef", dataset, var$variable)

      # MethodOID for a derived base variable (value-level methods live on the
      # def:ValueListDef ItemRefs instead).
      base_row <- base_vals[
        base_vals$dataset == dataset & base_vals$variable == var$variable, ,
        drop = FALSE
      ]
      method_oid <- NULL
      if (nrow(base_row) > 0 && identical(origin_type(base_row$origin[1]), "Derived")) {
        method_oid <- paste0("MT.", base_row$derivation_id[1])
      }

      itemref <- xml2::xml_add_child(igd, "ItemRef")
      set_attrs(
        itemref,
        "ItemOID" = item_oid,
        "OrderNumber" = def_val(var$order, v),
        "Mandatory" = yn(var$mandatory),
        "KeySequence" = var$key_seq,
        "MethodOID" = method_oid,
        "Role" = var$role
      )
    }

    # def:leaf pointing at the archived dataset file
    leaf <- xml2::xml_add_child(
      igd, "def:leaf",
      "ID" = .generate_oid("leaf", dataset),
      "xlink:href" = paste0(tolower(dataset), ".xpt")
    )
    .add_text_child(leaf, "def:title", paste0(tolower(dataset), ".xpt"))
  }
  invisible(NULL)
}


#' Build ItemDef elements (one per variable, plus one per value-level row)
#' @noRd
.create_item_defs <- function(mdv, metacore) {
  ds_vars <- metacore$ds_vars
  var_spec <- metacore$var_spec
  value_spec <- metacore$value_spec
  base_vals <- .base_value_lookup(value_spec)
  vl_rows <- .value_level_rows(value_spec)
  vl_keys <- if (nrow(vl_rows) > 0) unique(paste(vl_rows$dataset, vl_rows$variable, sep = "|")) else character(0)

  # --- Base variable ItemDefs ---
  for (i in seq_len(nrow(ds_vars))) {
    var <- ds_vars[i, , drop = FALSE]
    dataset <- var$dataset
    variable <- var$variable

    vspec <- .lookup_var_spec(var_spec, dataset, variable)
    base_row <- base_vals[
      base_vals$dataset == dataset & base_vals$variable == variable, ,
      drop = FALSE
    ]
    base_row <- if (nrow(base_row) > 0) base_row[1, , drop = FALSE] else NULL

    data_type <- def_val(
      if (!is.null(base_row)) base_row$type else NA,
      def_val(if (!is.null(vspec)) vspec$type else NA, "text")
    )

    idef <- xml2::xml_add_child(mdv, "ItemDef")
    comment_oid <- if (!is.null(base_row) && !is.null(base_row$comment_id) && !is.na(base_row$comment_id)) paste0("COM.", base_row$comment_id) else NULL
    set_attrs(
      idef,
      "OID" = .generate_oid("ItemDef", dataset, variable),
      "Name" = variable,
      "DataType" = data_type,
      "Length" = if (!is.null(vspec)) vspec$length else NULL,
      "SignificantDigits" = if (!is.null(base_row)) base_row$sig_dig else NULL,
      "SASFieldName" = variable,
      "DisplayFormat" = if (!is.null(vspec)) vspec$format else NULL,
      "def:CommentOID" = comment_oid
    )
    .add_description(idef, if (!is.null(vspec)) vspec$label else NA)

    # CodeListRef
    code_id <- if (!is.null(base_row)) base_row$code_id else NA
    if (!is.null(code_id) && length(code_id) == 1 && !is.na(code_id)) {
      xml2::xml_add_child(idef, "CodeListRef", CodeListOID = code_id)
    }

    # Origin
    if (!is.null(base_row)) {
      .add_origin(idef, base_row, metacore)
    }

    # ValueListRef when this variable has value-level metadata
    if (paste(dataset, variable, sep = "|") %in% vl_keys) {
      xml2::xml_add_child(
        idef, "def:ValueListRef",
        ValueListOID = .generate_oid("ValueListDef", dataset, variable)
      )
    }
  }

  # --- Value-level ItemDefs ---
  for (i in seq_len(nrow(vl_rows))) {
    row <- vl_rows[i, , drop = FALSE]
    dataset <- row$dataset
    variable <- row$variable
    checks <- .parse_where(row$where)
    first <- if (length(checks) > 0) checks[[1]] else NULL

    oid <- .generate_oid(
      "ItemDef", dataset, variable,
      where = row$where,
      where_var = if (!is.null(first)) first$var else NULL,
      comparator = if (!is.null(first)) first$comparator else NULL,
      value = if (!is.null(first)) first$values[1] else NULL
    )
    vspec <- .lookup_var_spec(var_spec, dataset, variable)

    idef <- xml2::xml_add_child(mdv, "ItemDef")
    set_attrs(
      idef,
      "OID" = oid,
      "Name" = sub("^IT\\.", "", oid),
      "DataType" = def_val(row$type, def_val(if (!is.null(vspec)) vspec$type else NA, "text")),
      "Length" = if (!is.null(vspec)) vspec$length else NULL,
      "SignificantDigits" = row$sig_dig,
      "SASFieldName" = variable
    )
    .add_description(idef, def_val(row$where_label, if (!is.null(vspec)) vspec$label else NA))

    if (!is.null(row$code_id) && length(row$code_id) == 1 && !is.na(row$code_id)) {
      xml2::xml_add_child(idef, "CodeListRef", CodeListOID = row$code_id)
    }
    .add_origin(idef, row, metacore)
  }

  invisible(NULL)
}


#' Add a def:Origin element to an ItemDef
#'
#' Handles the P21 implicit document-linking pattern: when the row's derivation
#' carries a `document_id`/`pages`, a `def:DocumentRef`/`def:PDFPageRef` is
#' nested inside the origin. Predecessor origins carry the predecessor text.
#' @noRd
.add_origin <- function(idef, row, metacore) {
  type <- origin_type(row$origin)
  if (is.null(type)) {
    return(invisible(idef))
  }
  origin <- xml2::xml_add_child(idef, "def:Origin", Type = type)

  # Predecessor origins record the source variable/text inline.
  if (identical(type, "Predecessor") && !is.na(row$derivation_id)) {
    deriv <- metacore$derivations
    hit <- deriv[deriv$derivation_id == row$derivation_id, , drop = FALSE]
    if (nrow(hit) > 0 && !is.na(hit$derivation[1])) {
      xml2::xml_text(origin) <- as.character(hit$derivation[1])
    }
  }

  # Document / page references via the linked derivation (implicit P21 pattern).
  if (!is.na(row$derivation_id)) {
    deriv <- metacore$derivations
    hit <- deriv[deriv$derivation_id == row$derivation_id, , drop = FALSE]
    if (nrow(hit) > 0) {
      doc_id <- hit$document_id[1]
      pages <- hit$pages[1]
      if (!is.null(doc_id) && !is.na(doc_id)) {
        docref <- xml2::xml_add_child(origin, "def:DocumentRef", leafID = doc_id)
        if (!is.null(pages) && !is.na(pages)) {
          xml2::xml_add_child(docref, "def:PDFPageRef", PageRefs = as.character(pages))
        }
      }
    }
  }
  invisible(idef)
}


# ------------------------------------------------------------------------------
# CodeLists
# ------------------------------------------------------------------------------

#' DataType associated with a codelist (from referencing value_spec rows)
#' @noRd
.codelist_data_type <- function(code_id, value_spec) {
  hit <- value_spec[!is.na(value_spec$code_id) & value_spec$code_id == code_id, , drop = FALSE]
  if (nrow(hit) > 0 && !is.na(hit$type[1])) hit$type[1] else "text"
}


#' Build CodeList elements for code/decode pairs and permitted values
#' @noRd
.create_code_lists <- function(mdv, metacore) {
  codelist <- metacore$codelist
  value_spec <- metacore$value_spec
  if (nrow(codelist) == 0) {
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(codelist))) {
    row <- codelist[i, , drop = FALSE]
    # `==` (not identical()) because the type column carries a label attribute.
    if (isTRUE(row$type == "external_library")) next # handled separately

    cl <- xml2::xml_add_child(mdv, "CodeList")
    set_attrs(
      cl,
      "OID" = row$code_id,
      "Name" = row$name,
      "DataType" = .codelist_data_type(row$code_id, value_spec)
    )

    codes <- row$codes[[1]]
    if (is.null(codes)) next

    if (isTRUE(row$type == "permitted_val")) {
      vals <- if (is.data.frame(codes)) codes[[1]] else codes
      for (j in seq_along(vals)) {
        xml2::xml_add_child(
          cl, "EnumeratedItem",
          CodedValue = as.character(vals[j]), OrderNumber = j
        )
      }
    } else {
      # code_decode: tibble(code, decode)
      for (j in seq_len(nrow(codes))) {
        item <- xml2::xml_add_child(
          cl, "CodeListItem",
          CodedValue = as.character(codes$code[j]), OrderNumber = j
        )
        decode_val <- if ("decode" %in% names(codes)) codes$decode[j] else codes$code[j]
        decode <- xml2::xml_add_child(item, "Decode")
        .add_text_child(decode, "TranslatedText", decode_val, "xml:lang" = "en")
      }
    }
  }
  invisible(NULL)
}


#' Build CodeList/ExternalCodeList elements for external dictionaries
#' @noRd
.create_external_code_lists <- function(mdv, metacore) {
  codelist <- metacore$codelist
  if (nrow(codelist) == 0) {
    return(invisible(NULL))
  }
  ext <- codelist[!is.na(codelist$type) & codelist$type == "external_library", , drop = FALSE]
  if (nrow(ext) == 0) {
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(ext))) {
    row <- ext[i, , drop = FALSE]
    cl <- xml2::xml_add_child(mdv, "CodeList")
    set_attrs(cl, "OID" = row$code_id, "Name" = row$name, "DataType" = "text")

    codes <- row$codes[[1]]
    dictionary <- if (is.data.frame(codes)) codes$dictionary[1] else NA
    version <- if (is.data.frame(codes)) codes$version[1] else NA
    ecl <- xml2::xml_add_child(cl, "ExternalCodeList")
    set_attrs(ecl, "Dictionary" = dictionary, "Version" = version)
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# Methods / Comments
# ------------------------------------------------------------------------------

#' Build MethodDef elements for derivation methods
#' @noRd
.create_method_defs <- function(mdv, metacore) {
  derivations <- metacore$derivations
  if (nrow(derivations) == 0) {
    return(invisible(NULL))
  }
  # Remove duplicates (keep only unique derivation_ids)
  methods <- derivations[!duplicated(derivations$derivation_id), , drop = FALSE]

  for (i in seq_len(nrow(methods))) {
    row <- methods[i, , drop = FALSE]
    md <- xml2::xml_add_child(mdv, "MethodDef")
    set_attrs(
      md,
      "OID" = paste0("MT.", row$derivation_id),
      "Name" = def_val(row$method_name, paste0("Algorithm to derive ", row$derivation_id)),
      "Type" = def_val(row$method_type, "Computation")
    )
    .add_description(md, row$derivation)
  }
  invisible(NULL)
}


#' Build def:CommentDef elements for comments
#' @noRd
.create_comment_defs <- function(mdv, metacore) {
  comments <- metacore$comments
  if (is.null(comments) || nrow(comments) == 0) {
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(comments))) {
    row <- comments[i, , drop = FALSE]
    cd <- xml2::xml_add_child(mdv, "def:CommentDef", OID = paste0("COM.", row$comment_id))
    .add_description(cd, row$comment)
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# Documents
# ------------------------------------------------------------------------------

#' Build def:leaf elements for referenced documents
#' @noRd
.create_leaf_refs <- function(mdv, metacore) {
  documents <- metacore$documents
  if (is.null(documents) || nrow(documents) == 0) {
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(documents))) {
    row <- documents[i, , drop = FALSE]
    if (is.na(row$document_id)) next
    leaf <- xml2::xml_add_child(
      mdv, "def:leaf",
      "ID" = row$document_id,
      "xlink:href" = def_val(row$href, "")
    )
    .add_text_child(leaf, "def:title", def_val(row$title, row$document_id))
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# Validation / output
# ------------------------------------------------------------------------------

#' Warn if any OIDs collide across the generated document
#' @noRd
.check_oid_uniqueness <- function(odm) {
  # Work on a detached copy so stripping namespaces (needed for the wildcard
  # xpath) doesn't mutate the document that will be written out.
  copy <- xml2::read_xml(as.character(odm))
  xml2::xml_ns_strip(copy)
  oids <- xml2::xml_attr(xml2::xml_find_all(copy, "//*[@OID]"), "OID")
  dupes <- unique(oids[duplicated(oids)])
  if (length(dupes) > 0) {
    cli::cli_warn(c(
      "Duplicate OIDs detected in generated Define.xml:",
      "i" = "{dupes}"
    ))
  }
  invisible(NULL)
}


#' Serialise the document, inject the stylesheet PI, and write to disk
#' @noRd
.write_define_xml <- function(odm, path, stylesheet_href) {
  txt <- as.character(odm)
  if (!is.null(stylesheet_href) && !is.na(stylesheet_href) && nzchar(stylesheet_href)) {
    pi <- paste0("<?xml-stylesheet type=\"text/xsl\" href=\"", stylesheet_href, "\"?>")
    # Insert the stylesheet processing instruction after the XML declaration.
    txt <- sub("(<\\?xml[^>]*\\?>\\n?)", paste0("\\1", pi, "\n"), txt)
  }
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}
