# =============================================================================
# metacore - New Features Demo
# =============================================================================
# Topics covered:
#   1. verbose argument in spec_to_metacore()
#   2. The Metacore object structure
#   3. Why define_fields exists (design rationale)
#   4. Creating a standard vs. define-enabled Metacore object
#   5. set_study_level() for supplying metadata not in the spec
#   6. Writing Define.xml with metacore_to_define_xml()
# =============================================================================

devtools::load_all()

# Path to the bundled P21 mock specification
spec_path <- metacore_example("p21_mock.xlsx")


# =============================================================================
# 1. VERBOSE ARGUMENT
# =============================================================================
# spec_to_metacore() now accepts a `verbose` argument that gives you fine-
# grained control over how noisy the reader is.
#
# Levels:
#   "message"  (default) - messages and warnings behave normally
#   "warn"               - messages are suppressed, warnings still shown
#   "collapse"           - all warnings are collapsed into a single count
#   "silent"             - everything is suppressed
#
# The old `quiet = TRUE` argument still works but is deprecated in favour of
# verbose. It is equivalent to verbose = "silent".

# --- "message" (default) ---
# All messages and warnings are shown. This is what you get by default.
cat("\n--- verbose = 'message' (default) ---\n")
mc_default <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, verbose = "message")

# --- "warn" ---
# Informational messages are suppressed but warnings still surface.
# Useful when running in a CI pipeline where you care about warnings but
# not routine progress messages.
cat("\n--- verbose = 'warn' ---\n")
mc_warn <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, verbose = "warn")

# --- "collapse" ---
# All warnings are summarised into a single count message instead of
# printing each one. Good for keeping console output tidy when you already
# know the spec has minor issues.
cat("\n--- verbose = 'collapse' ---\n")
mc_collapse <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, verbose = "collapse")

# --- "silent" ---
# Nothing is printed. Handy for batch jobs or when embedding in another
# package that manages its own logging.
cat("\n--- verbose = 'silent' ---\n")
mc_silent <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, verbose = "silent")


# =============================================================================
# 2. THE METACORE OBJECT STRUCTURE
# =============================================================================
# A standard Metacore object is a normalised, immutable container for all
# dataset-level metadata. It holds 7 interconnected tables:
#
#   ds_spec      - one row per dataset (name, structure, label)
#   ds_vars      - one row per dataset x variable (order, key, core, mandatory)
#   var_spec     - one row per unique variable (type, length, label, format)
#   value_spec   - one row per dataset x variable value (origin, code list, derivation)
#   derivations  - one row per derivation (id -> text)
#   codelist     - one row per codelist (id -> codes/decode pairs)
#   supp         - one row per supplemental variable (idvar, qeval)
#
# All tables are read-only once built - they cannot be directly modified.

cat("\n--- Standard Metacore object ---\n")
print(mc_default)

# Inspect individual tables
mc_default$ds_spec
mc_default$ds_vars
mc_default$var_spec
mc_default$value_spec
mc_default$derivations
mc_default$codelist
mc_default$supp

# Use select_dataset() to filter the object to a single domain
mc_adsl <- mc_default |> select_dataset("ADSL")
mc_adsl$ds_spec
mc_adsl$ds_vars


# =============================================================================
# 3. WHY define_fields EXISTS - DESIGN RATIONALE
# =============================================================================
# metacore is a single unified schema for clinical metadata. It uses Pinnacle
# 21 (P21) spec templates as its primary input format because that is what most
# ADaM programming teams work with - but metacore is *not* restricted to P21
# format. The spec builder functions (spec_type_to_ds_spec(), etc.) are
# exported and documented precisely so that teams can write custom readers for
# any specification format they use.
#
# Generating a valid Define.xml 2.0 file requires a handful of additional
# fields that P21 specs do carry (class, repeating, purpose on datasets; role
# on variables; method_name / method_type / document_id on derivations) but
# that were not part of the original metacore schema. Rather than adding these
# columns unconditionally and breaking every existing workflow, we made them
# opt-in via define_fields = TRUE.
#
# The extended tables are:
#
#   ds_spec      gains: class, repeating, reference, purpose
#   ds_vars      gains: role
#   value_spec   gains: where_label, comment_id
#   derivations  gains: method_name, method_type, document_id, pages
#
#   Plus three new tables:
#   study_level  - study-wide metadata (name, description, protocol, standard)
#   documents    - linked external documents referenced in methods
#   comments     - variable-level comment text linked via comment_id
#
# Custom readers can populate these fields from whatever source they have -
# they are just additional columns/tables in the same normalised structure.


# =============================================================================
# 4. CREATING A STANDARD VS. DEFINE-ENABLED METACORE OBJECT
# =============================================================================

# --- Standard object (define_fields = FALSE, the default) ---
# Unchanged from existing behaviour. No disruption to existing workflows.
cat("\n--- Standard object (define_fields = FALSE) ---\n")
mc_base <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, verbose = "silent")
print(mc_base)

# The ds_spec table has the original three columns only
mc_base$ds_spec

# --- Define-enabled object (define_fields = TRUE, experimental) ---
# Reads the same spec but also populates the extended columns and the three
# additional tables. The lifecycle badge on this argument is "experimental" -
# the API is stable enough to use, but we may refine it before it becomes
# the default.
cat("\n--- Define-enabled object (define_fields = TRUE) ---\n")
mc_define <- spec_to_metacore(spec_path, where_sep_sheet = FALSE, define_fields = TRUE, verbose = "silent")
print(mc_define)

# The ds_spec table now has four extra columns
mc_define$ds_spec

# ds_vars gains a role column
mc_define$ds_vars

# value_spec gains where_label and comment_id
mc_define$value_spec

# derivations gains method_name, method_type, document_id, pages
mc_define$derivations

# Three new tables are now available
mc_define$study_level
mc_define$documents
mc_define$comments

# Comparing the two side by side:
cat("Base ds_spec columns:  ", paste(names(mc_base$ds_spec), collapse = ", "), "\n")
cat("Define ds_spec columns:", paste(names(mc_define$ds_spec), collapse = ", "), "\n")


# --- Standard metacore object should be the exact same structure as previous version 0.3.0 ---
metacore_0_3_0 <- load_metacore(metacore_example("metacore_0_3_0.rds"))
metacore_0_4_0 <- spec_to_metacore(metacore_example("p21_mock.xlsx"), where_sep_sheet = FALSE, verbose = "silent")

# Names of the tables are identical
identical(names(metacore_0_3_0), names(metacore_0_4_0))

# Tables contain identical information
identical(metacore_0_3_0$ds_spec, metacore_0_4_0$ds_spec)

identical(metacore_0_3_0$ds_vars, metacore_0_4_0$ds_vars)

identical(metacore_0_3_0$var_spec, metacore_0_4_0$var_spec)

identical(metacore_0_3_0$codelist, metacore_0_4_0$codelist)

identical(metacore_0_3_0$derivations, metacore_0_4_0$derivations)

identical(metacore_0_3_0$supp, metacore_0_4_0$supp)

# `where` changed from TRUE default to NA in 0.4.0 so excluded from check
identical(
  metacore_0_3_0$value_spec[, -which(names(metacore_0_3_0$value_spec) == "where")],
  metacore_0_4_0$value_spec[, -which(names(metacore_0_4_0$value_spec) == "where")]
)


# =============================================================================
# 5. set_study_level() - SUPPLYING METADATA NOT IN THE SPEC
# =============================================================================
# Study-level metadata (study name, description, protocol, CDISC standard) is
# required for Define.xml but is often not stored in the spec itself - it may
# live in a separate study setup file or just be known to the programmer.
#
# If spec_to_metacore() cannot find a "Define" or "Study" sheet in the spec,
# the study_level table will be filled with NAs. You can supply (or override)
# this information after construction using set_study_level(). The method
# returns the object invisibly so calls can be chained.

cat("\n--- study_level before set_study_level() ---\n")
mc_define$study_level

# set_study_level() modifies the R6 object in place - no need to reassign.
mc_define$set_study_level(
  study_name        = "CDISCPILOT01",
  study_description = "Safety and Efficacy of the Xanomeline Transdermal Therapeutic System",
  protocol_name     = "CDISCPILOT01",
  standard_name     = "CDISC ADaM",
  standard_version  = "1.1",
  define_version    = "2.0.0",
  language          = "en"
)

cat("\n--- study_level after set_study_level() ---\n")
mc_define$study_level

# You can also update individual fields by omitting the others - omitted
# arguments default to NA, so call set_study_level() with the full set of
# values you want each time.


# =============================================================================
# 6. WRITING DEFINE.XML
# =============================================================================
# metacore_to_define_xml() converts a Metacore object into a valid Define.xml
# 2.0 document. It works with both base and define-enabled objects; the define-
# enabled object will produce richer output (method types, comments, doc refs).
#
# study_details must include at minimum: study_name, study_description,
# protocol_name. It accepts the study_level tibble directly.

output_path <- tempfile(fileext = ".xml")

cat("\n--- Writing Define.xml ---\n")
xml_doc <- metacore_to_define_xml(
  metacore_obj   = mc_define,
  path           = output_path,
  study_details  = mc_define$study_level   # pass the study_level tibble directly
)

cat("Output written to:", output_path, "\n")

# Inspect the XML in memory (xml2 document)
xml_doc

# You can also generate the XML without writing to disk by omitting path:
xml_in_memory <- metacore_to_define_xml(
  metacore_obj  = mc_define,
  study_details = mc_define$study_level
)

# Preview the first ~500 characters of the generated XML
cat(substr(as.character(xml_in_memory), 1, 500), "\n")

# =============================================================================
# SUMMARY
# =============================================================================
# - verbose gives you four levels of console output control in spec_to_metacore()
# - The existing 7-table schema is unchanged when define_fields = FALSE (default)
# - define_fields = TRUE is entirely opt-in and experimental; it extends the
#   existing tables and adds study_level, documents, and comments
# - set_study_level() lets you supply or override study metadata after loading
# - metacore_to_define_xml() generates a Define.xml 2.0 document from any
#   Metacore object, with richer output when define_fields was used
# =============================================================================
