#!/usr/bin/env Rscript
# Base-R structural checks only: never sources an analysis or opens scientific data.
root <- normalizePath(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), mustWork = TRUE)
catalog <- read.csv(file.path(root, "metadata/workflow_stages.csv"), stringsAsFactors = FALSE)
files <- c(list.files(file.path(root, "code"), "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
           list.files(file.path(root, "config"), "\\.[Rr]$", full.names = TRUE),
           file.path(root, "config.R"))
for (f in files) parse(f)
stopifnot(all(file.exists(file.path(root, catalog$script))), !anyDuplicated(catalog$script),
          all(nzchar(catalog$inputs)), all(nzchar(catalog$outputs)))
cat("PASS: parsed", length(files), "R files;", nrow(catalog), "stage entries with input/output descriptions.\n")
cat("WARNING: this is structural validation; no scientific data or final results validated.\n")
