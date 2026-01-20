globalVariables(c(
  "private",
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
  "where_new",
  "var1",
  "n_lab"
))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Attaching package `metacore`\n\nAs of metacore 0.3.0 the `keep` variable in the `ds_vars` table has been renamed to `mandatory`. Please see release documentation for details.")
}
