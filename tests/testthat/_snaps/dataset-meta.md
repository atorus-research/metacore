# DatasetMeta print snapshot

    Code
      print(dm_base)
    Message
      -- Dataset specification object for DM (Demographics) --------------------------
      The dataset contains 25 variables
      Dataset keys: STUDYID and USUBJID
      
      The structure of the specification object is:
      > codelist: character [8 x 4] code_id, name, type, and codes
      > derivations: character [20 x 2] derivation_id and derivation
      > ds_spec: character [1 x 3] dataset, structure, and label
      > ds_vars: character [25 x 7] dataset, variable, key_seq, order, mandatory,
        core, and supp_flag
      > supp: character [0 x 4] dataset, variable, idvar, and qeval
      > value_spec: character [25 x 8] dataset, variable, type, origin, sig_dig,
        code_id, where, and derivation_id
      > var_spec: character [25 x 6] variable, length, label, type, format, and
        common
      
      To inspect the specification object use `View()` in the console.

# DatasetMetaDefine print snapshot

    Code
      print(dm_define)
    Message
      -- Dataset specification object for DM (Demographics) --------------------------
      The dataset contains 31 variables
      Dataset keys: STUDYID and USUBJID
      
      The structure of the specification object is:
      > codelist: character [8 x 4] code_id, name, type, and codes
      > comments: character [3 x 2] comment_id and comment
      > derivations: character [20 x 6] derivation_id, derivation, method_name,
        method_type, document_id, and pages
      > documents: character [0 x 3] document_id, title, and href
      > ds_spec: character [1 x 7] dataset, structure, label, class, repeating,
        reference, and purpose
      > ds_vars: character [31 x 8] dataset, variable, key_seq, order, mandatory,
        core, supp_flag, and role
      > study_level: character [0 x 7] study_name, study_description, protocol_name,
        standard_name, standard_version, define_version, and language
      > supp: character [6 x 4] dataset, variable, idvar, and qeval
      > value_spec: character [31 x 10] dataset, variable, type, origin, sig_dig,
        code_id, where, where_label, derivation_id, and comment_id
      > var_spec: character [31 x 6] variable, length, label, type, common, and
        format
      
      To inspect the specification object use `View()` in the console.

