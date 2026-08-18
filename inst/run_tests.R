#!/usr/bin/env Rscript
# tests/run_tests.R
#
# Standalone test runner for the LED validation test suite.
# Does NOT require an R package structure (no DESCRIPTION / devtools needed).
#
# Usage:
#   Rscript tests/run_tests.R                        # console only
#   Rscript tests/run_tests.R --junit                # console + one XML per test file
#   Rscript tests/run_tests.R --filter "addv-spine"  # run tests matching a pattern
#
# With --junit, one XML file is written per test script to tests/test-results/.
# File names mirror the test script name, e.g.:
#   tests/testthat/test-addv-spine.R  ->  tests/test-results/test-addv-spine.xml
#
# The script:
#   1. Sets PYTHONPATH so led_runner.py can find the Mercury executor.
#   2. Loads .env if present (for DOMINO_PROJECT_NAME etc.).
#   3. Sources setup.R + helper-*.R once, then runs each test-*.R file.

# ── Parse arguments ────────────────────────────────────────────────────────────
args        <- commandArgs(trailingOnly = TRUE)
use_junit   <- "--junit"  %in% args
filter_arg  <- {
  idx <- match("--filter", args)
  if (!is.na(idx) && idx < length(args)) args[idx + 1L] else NULL
}

# ── Environment setup ──────────────────────────────────────────────────────────
script_path <- normalizePath(
  sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]),
  mustWork = FALSE
)
repo_root <- if (!is.na(script_path)) dirname(dirname(script_path)) else getwd()

env_file <- file.path(repo_root, ".env")
if (file.exists(env_file)) {
  env_lines <- readLines(env_file, warn = FALSE)
  env_lines <- env_lines[grepl("^[A-Z_]+=", env_lines)]
  for (line in env_lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2L) {
      key <- trimws(parts[1])
      val <- trimws(paste(parts[-1], collapse = "="))
      Sys.setenv(.envvar = val, .names = key)
    }
  }
}

# Ensure Mercury executor is on PYTHONPATH
mercury_path <- Sys.getenv("MERCURY_EXECUTOR_PATH",
                            unset = "/mnt/imported/code/mercury_executor")
existing_pythonpath <- Sys.getenv("PYTHONPATH", unset = "")
new_pythonpath <- if (nzchar(existing_pythonpath)) {
  paste(mercury_path, existing_pythonpath, sep = ":")
} else {
  mercury_path
}
Sys.setenv(PYTHONPATH = new_pythonpath)

# ── Discover test files ────────────────────────────────────────────────────────
test_dir   <- file.path(repo_root, "tests", "testthat")
all_files  <- list.files(test_dir, pattern = "^test.*\\.[Rr]$", full.names = TRUE)

if (!is.null(filter_arg)) {
  all_files <- all_files[grepl(filter_arg, basename(all_files), fixed = FALSE)]
}

if (length(all_files) == 0L) {
  cat("No test files matched filter:", filter_arg, "\n")
  quit(status = 0L)
}

# ── Source setup and helpers once into a shared environment ───────────────────
test_env <- new.env(parent = globalenv())
setwd(test_dir)

setup_file <- file.path(test_dir, "setup.R")
if (file.exists(setup_file)) source(setup_file, local = test_env)

helper_files <- list.files(test_dir, pattern = "^helper.*\\.[Rr]$", full.names = TRUE)
for (hf in helper_files) source(hf, local = test_env)

# ── Prepare output directory for JUnit XMLs ───────────────────────────────────
if (use_junit) {
  results_dir <- file.path(repo_root, "tests", "test-results")
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  cat("JUnit XMLs will be written to:", results_dir, "\n\n")
}

# ── Run each test file ────────────────────────────────────────────────────────
total_pass <- 0L
total_fail <- 0L
total_skip <- 0L

for (test_file in all_files) {
  stem <- sub("\\.[Rr]$", "", basename(test_file))

  if (use_junit) {
    xml_path <- file.path(results_dir, paste0(stem, ".xml"))
    reporter <- testthat::MultiReporter$new(list(
      testthat::ProgressReporter$new(),
      testthat::JunitReporter$new(file = xml_path)
    ))
  } else {
    reporter <- testthat::ProgressReporter$new()
  }

  result <- testthat::test_file(
    path            = test_file,
    reporter        = reporter,
    env             = test_env,
    stop_on_failure = FALSE
  )

  df         <- as.data.frame(result)
  total_pass <- total_pass + sum(df$passed,  na.rm = TRUE)
  total_fail <- total_fail + sum(df$failed,  na.rm = TRUE) + sum(df$error, na.rm = TRUE)
  total_skip <- total_skip + sum(df$skipped, na.rm = TRUE)
}

# ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf("\nResults: %d passed, %d failed/errored, %d skipped\n",
            total_pass, total_fail, total_skip))

if (total_fail > 0L) quit(status = 1L)
