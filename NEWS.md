# Metacore 0.0.1

This fixes the following issues:

- #16 the metacore function now accepts any empty datasets and creates an empty dataset with the correct column names and types
- #10 yn function checks for logicals and returns them
- #11 updated function description to make this clearer
- #12 updated regex so to [F|f]ormat so it can accept lower case
- #14 added supp_flag to ds_vars (on a side note we did a really good job with this it was super easy to change and only required a few edits)
- #15 modified create =tbl so if there are two potential matches in the same dataset and one is an exact match it uses that

