# Define specifications to be used in tests
p21_spec <- spec_to_metacore(metacore_example("p21_mock.xlsx"), verbose = "silent")
suppae_spec <- select_dataset(p21_spec, "SUPPAE", verbose = "silent")

vlm_spec <- spec_to_metacore("vlm_test_spec.xlsx", where_sep_sheet = FALSE, verbose = "silent")
