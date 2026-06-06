#' Extended Metacore initializer (Define.xml schema: base tables + 3 extra tables)
#' @family Metacore
#' @noRd
MetaCoreDefine_initialize <- function(
      ds_spec, ds_vars, var_spec, value_spec, derivations, codelist, supp, study_level = NULL,
      documents = NULL, comments = NULL, quiet = FALSE, verbose = "message") {

   deprecate_soft(
      when = "0.3.0",
      what = "MetaCore_initialize(quiet)",
      with = "MetaCore_initialize(verbose)"
   )

   .metacore_init_base_tables(private, define_column_schema(), ds_spec, ds_vars, var_spec,
                              value_spec, derivations, codelist, supp)

   full_schema <- define_column_schema()

   private$.study_level <- fill_cols(study_level, full_schema$.study_level) |>
      add_labs(
         study_name = "Study Name", study_description = "Study Description",
         protocol_name = "Protocol Name", standard_name = "Standard",
         standard_version = "Standard Version", define_version = "Define Version",
         language = "Language"
      )

   private$.documents <- fill_cols(documents, full_schema$.documents) |>
      add_labs(document_id = "Document ID", title = "Title", href = "Href")

   private$.comments <- fill_cols(comments, full_schema$.comments) |>
      add_labs(comment_id = "Comment ID", comment = "Comment Text")

   self$validate()

   if (inherits_only(self, c("MetacoreDefine", "Metacore", "R6"))) {
      private$.greet(quiet)
   }
}

#' Extended Metacore validation — checks all tables including Define.xml tables
#' @family Metacore
#' @noRd
MetaCoreDefine_validate <- function() {
   if (var_name_check(private, define_fields = TRUE)) {
      if (nrow(private$.ds_spec) == 0 &
          nrow(private$.ds_vars) == 0 &
          nrow(private$.var_spec) == 0 &
          nrow(private$.value_spec) == 0 &
          nrow(private$.derivations) == 0 &
          nrow(private$.codelist) == 0 &
          nrow(private$.supp) == 0 &
          nrow(private$.study_level) == 0 &
          nrow(private$.documents) == 0 &
          nrow(private$.comments) == 0) {
         cli_warn("Other checks were not performed, because all datasets are empty",
                  call. = FALSE
         )
      } else {
         check_columns(
            private$.ds_spec, private$.ds_vars, private$.var_spec,
            private$.value_spec, private$.derivations, private$.codelist,
            comments = private$.comments
         )
         ds_vars_check(private$.ds_vars, private$.var_spec)
         value_check(private$.ds_vars, private$.value_spec)
         derivation_check(private$.value_spec, private$.derivations)
         codelist_check(private$.value_spec, private$.codelist)
         if (nrow(private$.comments) > 0) {
            comment_check(private$.value_spec, private$.comments)
         }
         if (nrow(private$.supp) > 0) {
            check_columns(supp = private$.supp)
            supp_check(private$.ds_vars, private$.supp)
         }
      }
   } else {
      cli_warn("Other checks were not performed, because column names were incorrect",
               call. = FALSE
      )
   }
}

#' Extended Metacore R6 Class (Define.xml schema: adds study_level, documents, comments)
#' @family Metacore
#' @noRd
MetaCoreDefine <- R6::R6Class(
   "MetacoreDefine",
   inherit = MetaCore,
   public = list(
      initialize = MetaCoreDefine_initialize,
      validate   = MetaCoreDefine_validate
   ),
   private = list(
      .study_level = tibble(),
      .documents   = tibble(),
      .comments    = tibble()
   ),
   active = list(
      study_level = readonly("study_level"),
      documents   = readonly("documents"),
      comments    = readonly("comments")
   )
)
