# Expand metacore schema to support Define.xml generation

## Background

The metacore object currently stores 7 tables that capture the core clinical trial metadata needed for downstream data programming tools. However, generating a CDISC Define.xml 2.0 / 2.1 document from a metacore object requires several additional fields that the current schema does not capture.

This issue tracks the schema extensions needed as a prerequisite for a `metacore_to_define_xml()` writer function.

## Proposed schema changes

**Extended columns on existing tables** (opt-in via `define_fields = TRUE`):

| Table | New columns | Source in P21 spec |
|---|---|---|
| `ds_spec` | `class`, `repeating`, `reference`, `purpose` | Datasets sheet |
| `ds_vars` | `role` | Variables sheet |
| `value_spec` | `where_label`, `comment_id` | ValueLevel sheet / Comments sheet |
| `derivations` | `method_name`, `method_type`, `document_id`, `pages` | Methods sheet |

**New tables** (only present when `define_fields = TRUE`):

| Table | Columns | Purpose |
|---|---|---|
| `study_level` | `study_name`, `study_description`, `protocol_name`, `standard_name`, `standard_version`, `define_version`, `language` | ODM root and GlobalVariables elements |
| `documents` | `document_id`, `title`, `href` | `def:leaf` elements for linked PDFs |
| `comments` | `comment_id`, `comment` | `def:CommentDef` elements |

## Design decisions

- All extensions are **opt-in**. The default behaviour of `spec_to_metacore()`, `define_to_metacore()`, and `metacore()` is unchanged (`define_fields = FALSE`), returning the original 7-table schema so existing downstream packages are unaffected.
- When `define_fields = TRUE`, the returned object is of class `MetacoreDefine` (inherits `Metacore`), which adds the three extra tables as first-class active bindings. The base `Metacore` class has no knowledge of them.
- Comments are stored in their own table (linked via `value_spec$comment_id`) rather than mixed into the derivations table, allowing a variable to have both a `MethodDef` and a `CommentDef` independently.
- Document linking is **implicit through methods**: `Variables → derivation_id → derivations$document_id → documents`. No page-reference columns are needed on `value_spec` or `ds_vars`.

## Acceptance criteria

- [ ] `spec_to_metacore(path, define_fields = TRUE)` populates all new columns and tables from a P21 Excel spec
- [ ] `define_to_metacore(path, define_fields = TRUE)` populates all new columns and tables from a Define.xml file
- [ ] `metacore(define_fields = FALSE)` (default) returns an object indistinguishable from the pre-extension schema — no extra columns, no extra tables
- [ ] All existing tests continue to pass
- [ ] New validators cover the `comment_id` foreign key between `value_spec` and `comments`

## Related

This issue is a prerequisite for the Define.xml writer (`metacore_to_define_xml()`), which depends on these fields being populated.
