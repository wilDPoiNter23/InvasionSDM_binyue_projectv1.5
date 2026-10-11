#!/usr/bin/env Rscript
# Preflight/list are cheap. Running analysis requires an explicit --run flag.
args <- commandArgs(trailingOnly = TRUE)
project <- normalizePath(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), mustWork = TRUE)
catalog <- read.csv(file.path(project, "metadata", "workflow_stages.csv"), stringsAsFactors = FALSE)
if (!length(args) || args[1] == "--list") { print(catalog[, c("stage", "script", "execution_status")], row.names = FALSE); quit(status = 0) }
if (!args[1] %in% c("--check", "--run") || length(args) < 2) stop("Usage: Rscript code/run_stage.R --list | --check <script> | --run <script> [batch_id]")
script <- args[2]
if (!script %in% catalog$script) stop("Select a script from --list")
if (!file.exists(file.path(project, script))) stop("Missing catalogued script")
invisible(parse(file.path(project, script)))
Sys.setenv(INVASIONSDM_PROJECT_ROOT = project)
source(file.path(project, "config", "bootstrap.R"))
if (args[1] == "--check") { cat("Parse/config OK. External inputs, scientific provenance, packages and workspace NOT validated.\n"); quit(status = 0) }
if (grepl("fit_and_project_batch", script)) stop("Batch script needs commandArgs: use Slurm template or direct Rscript after bootstrap via R_PROFILE_USER; see HPC README")
if (grepl("BLOCKER", catalog$execution_status[match(script, catalog$script)])) stop("Stage blocked: see docs/REPRODUCIBILITY.md")
source(file.path(project, script), chdir = FALSE)
