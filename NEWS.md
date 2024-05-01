# Metacore 0.1.3
- Add `get_keys` function which returns the dataset keys for a given dataset [#102](https://github.com/atorus-research/metacore/issues/102) 
- Fix issues with `select_dataset(simplify = TRUE)` [#97](https://github.com/atorus-research/metacore/issues/97) 

# Metacore 0.1.2
- Update to resolve issues from the dplyr updates

# Metacore 0.1.1
- Add variable cross checks to check consistent use of label, type, and format
- Add `is_metacore()` function to check if it is a metacore object 
- [#64](https://github.com/atorus-research/metacore/issues/64) Resolve issue so define and spec readers work when value levels are empty

# Metacore 0.1.0
- Updates for latest version of {tidyselect}
- Move from XML to xml2 for define.xml processing
- Correct issue with warning on ds_var$core 
- Correct bug with not reading the derivation of predecessor and assigned variables correctly

# Metacore 0.0.6 
- Correct typos
- Limits origin value/make all origins lower case 

# Metacore 0.0.5
- [#47](https://github.com/atorus-research/metacore/issues/47) Improve `yn_to_tf()` to be more robust in parsing yes/no columns

# Metacore 0.0.4
- Adds a supp table to store the `idvar` and `qeval` information 
- Adds `sig_dig`column to the `value_spec` table


# Metacore 0.0.3
- Fixes bugs found in the `get_control_term` function and improves error messages for `get_control_term`
- Improves internal naming consistency by renaming `define_to_MetaCore` to `define_to_metacore`
- Adds quiet option to readers, to allow users to optionally ignore the reader warnings 
- Creates new example metacore object that is an .rda so much faster to read in 
- Fixes issue with select dataset when variables are duplicated 


# Metacore 0.0.1
This fixes the following issues:

- [#16](https://github.com/atorus-research/metacore/issues/16) the metacore function now accepts any empty datasets and creates an empty dataset with the correct column names and types
- [#10](https://github.com/atorus-research/metacore/issues/10) yn function checks for logicals and returns them
- [#11](https://github.com/atorus-research/metacore/issues/11) updated function description to make this clearer
- [#12](https://github.com/atorus-research/metacore/issues/12) updated regex so to "[F|f]ormat" so it can accept lower case
- [#14](https://github.com/atorus-research/metacore/issues/14) added supp_flag to ds_vars (on a side note we did a really good job with this it was super easy to change and only required a few edits)
- [#15](https://github.com/atorus-research/metacore/issues/15) modified create_tbl so if there are two potential matches in the same dataset and one is an exact match it uses that

Additionally, it adds the `get_control_term` function to get pull out the control term for a given variable. 
