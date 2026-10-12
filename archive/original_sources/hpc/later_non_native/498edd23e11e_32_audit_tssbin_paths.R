# Original source: 32_audit_tssbin_paths.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/32_audit_tssbin_paths.R
# Original source SHA-256: 498edd23e11ef70fc78b1286d47171f86b21aee6a750e71c369a8efcebae0952
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 32_audit_tssbin_paths.R — HARD INTEGRITY CHECK: how many of the 813 frozen
# manifest paths point to binary TSS-thresholded (_TSSbin) files instead of the
# continuous suitability ensemble, and do the continuous siblings exist?
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

m <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
paths <- m$final_projection_path
paths <- ifelse(grepl("^/", paths) & file.exists(paths), paths,
                file.path("<WORK_ROOT>", paths))
m$path <- paths
m$is_tssbin <- grepl("TSSbin", basename(paths))
cat("TSSbin paths:", sum(m$is_tssbin), "/ 813\n")

# continuous sibling: remove _TSSbin from file name
m$sibling <- NA_character_
m$sibling_exists <- FALSE
idx <- which(m$is_tssbin)
for (i in idx) {
  sib <- gsub("_TSSbin", "", m$path[i])
  for (ext in c(".grd", ".tif")) {
    cand <- sub("\\.grd$", ext, sib)
    if (file.exists(cand)) { m$sibling[i] <- cand; m$sibling_exists[i] <- TRUE; break }
  }
}
cat("continuous siblings found:", sum(m$sibling_exists), "/", length(idx), "\n")

# verify siblings are continuous and 0-1000 for a sample of 8, plus minmax stats
samp <- which(m$sibling_exists)[seq(1, sum(m$sibling_exists), length.out = 8)]
cat("\nsampled sibling ranges:\n")
for (i in samp) {
  mm <- minmax(rast(m$sibling[i]))
  cat(sprintf("  %-40s min=%.1f max=%.1f\n", m$accepted_taxon[i], mm[1], mm[2]))
}
# sibling complete-case count of distinct values for first 3 (binary vs continuous)
cat("\ndistinct values check (first 3 siblings):\n")
for (i in samp[1:3]) {
  v <- values(rast(m$sibling[i]), mat = FALSE, na.rm = TRUE)
  u <- unique(v)
  cat(sprintf("  %-40s n_unique=%d\n", m$accepted_taxon[i], length(u)))
}

out <- m[, c("accepted_taxon", "source_generation", "product_type", "path",
             "is_tssbin", "sibling", "sibling_exists")]
write.csv(out, file.path(DIR_TABLES, "tssbin_manifest_audit_813.csv"), row.names = FALSE)
cat("\nwritten tssbin_manifest_audit_813.csv\nDONE\n")
