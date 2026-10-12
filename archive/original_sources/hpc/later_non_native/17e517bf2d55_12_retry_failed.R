# Original source: 12_retry_failed.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/12_retry_failed.R
# Original source SHA-256: 17e517bf2d55632cbf2955212bc506370388555d6e6be79e00c3030e6fcf8b42
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 12_retry_failed.R — PHASE 16: retry ONLY failed taxa (one SLURM task each)
# Usage: Rscript 12_retry_failed.R [--submit | --list]
#  --list    : print failed manifest row indices
#  --submit  : launch a SLURM array over only the failed indices
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

retry_csv <- file.path(DIR_PROVENANCE, "failed_taxa_retry_list.csv")
if (!file.exists(retry_csv)) { cat("No failed taxa on record.\n"); quit(save = "no", status = 0) }
r <- read.csv(retry_csv, stringsAsFactors = FALSE)
cat("Failed taxa:\n"); print(r)

mode <- if (length(commandArgs(trailingOnly = TRUE))) commandArgs(trailingOnly = TRUE)[1] else "list"
if (mode == "--submit") {
  stopifnot(nrow(r) > 0)
  idx <- paste(r$manifest_row_index, collapse = ",")
  job_file <- tempfile(fileext = ".sh", tmpdir = DIR_LOGS)
  writeLines(c(
    "#!/bin/bash",
    "#SBATCH --job-name=nn_retry",
    sprintf("#SBATCH --array=%s%%10", idx),
    "#SBATCH --cpus-per-task=1",
    "#SBATCH --mem=4G",
    "#SBATCH --partition=thcp1",
    sprintf("#SBATCH --output=%s/non_native_retry_%%A_%%a.out", DIR_LOGS),
    sprintf("#SBATCH --error=%s/non_native_retry_%%A_%%a.err", DIR_LOGS),
    "#SBATCH --time=02:00:00",
    "source ~/.bashrc",
    "module add loginnode",
    "export R_LIBS=\"<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu\"",
    sprintf("Rscript %s \"$SLURM_ARRAY_TASK_ID\"", file.path(DIR_SCRIPTS, "09_mask_one_taxon.R")),
    "exit $?"), job_file)
  system2("sbatch", job_file)
  cat("Submitted retry job:", job_file, "\n")
}
