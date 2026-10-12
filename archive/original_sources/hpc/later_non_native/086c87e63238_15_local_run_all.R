# Original source: 15_local_run_all.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/15_local_run_all.R
# Original source SHA-256: 086c87e632385350ffddf778c8abbba71734ec50cb21a1be0d57b44c69fffb5a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 15_local_run_all.R — LOCAL FALLBACK runner for the full 813 masking
# Reason: cluster submit quota (AssocGrpSubmitJobsLimit) blocked the SLURM array
# on 2026-08-15; cluster in maintenance (most nodes drained).
# This runs the SAME per-taxon script (09_mask_one_taxon.R) via Rscript child
# processes on the login node with 8 parallel workers. Resumable: taxa whose
# per-taxon QC exists with QC_pass=TRUE are skipped.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")

n_workers <- 8L

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)

done <- function(tx) {
  f <- file.path(PER_TAXON_QC_DIR, paste0(taxon_safe(tx), "__qc.csv"))
  if (!file.exists(f)) return(FALSE)
  d <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d)) return(FALSE)
  isTRUE(d$QC_pass[1])
}

todo <- which(!vapply(mani$accepted_taxon, done, logical(1)))
cat("taxa to process:", length(todo), "/ 813\n")

run_one <- function(idx) {
  tx <- mani$accepted_taxon[idx]
  logf <- file.path(DIR_LOGS, paste0("local_", idx, "_", taxon_safe(tx), ".log"))
  st <- system2("Rscript",
                c(file.path(DIR_SCRIPTS, "09_mask_one_taxon.R"), as.character(idx)),
                stdout = logf, stderr = logf)
  cat(sprintf("[%d/%d] %s -> exit %d\n", match(idx, todo), length(todo), tx, st))
  st
}

st <- Sys.time()
res <- parallel::mclapply(todo, run_one, mc.cores = n_workers,
                          mc.preschedule = FALSE)
res <- unlist(res)
cat("finished. elapsed:", format(difftime(Sys.time(), st)), "\n")
cat("exit-code table:\n"); print(table(res, useNA = "ifany"))
cat("failed:", sum(res != 0), "\n")
