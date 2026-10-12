# Original source: 18_manifest_freeze_QC.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/18_manifest_freeze_QC.R
# Original source SHA-256: 4dbfaec811abc5542276e6fe629d6f25e1a1851f95c53f5c6284f2504d5cb390
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 18_manifest_freeze_QC.R — final authoritative freeze validation of the 813 manifest
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

mani_path <- file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv")
mani <- read.csv(mani_path, stringsAsFactors = FALSE)
qc_all <- read.csv(file.path(DIR_SPP_QC, "non_native_projection_QC_813.csv"), stringsAsFactors = FALSE)

cat("rows:", nrow(mani), "\n")
cat("unique taxa:", length(unique(mani$accepted_taxon)), "\n")
cat("duplicates:", sum(duplicated(mani$accepted_taxon)), "\n")
cat("rostratum in main manifest:", "Solanum rostratum" %in% mani$accepted_taxon, "\n")
cat("global-equal set:", paste(sort(mani$accepted_taxon[mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL"]), collapse = "; "), "\n")

approved_ge <- c("Citrus aurantium", "Citrus limon", "Oenothera glazioviana")
ge_ok <- setequal(mani$accepted_taxon[mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL"], approved_ge)

# per-taxon integrity: path exists, readable, SHA256 matches, QC PASS
out <- data.frame()
for (i in seq_len(nrow(mani))) {
  p <- mani$output_non_native_projection[i]
  tx <- mani$accepted_taxon[i]
  exists_ok <- file.exists(p) && file.info(p)$size > 0
  sha <- if (exists_ok) sha256_of(p) else NA_character_
  sha_match <- !is.na(sha) && !is.na(mani$output_SHA256[i]) &&
               tolower(sha) == tolower(mani$output_SHA256[i])
  r <- if (exists_ok) tryCatch(rast(p), error = function(e) NULL) else NULL
  readable <- !is.null(r)
  qc_pass <- isTRUE(qc_all$QC_pass[match(tx, qc_all$accepted_taxon)])
  out <- rbind(out, data.frame(accepted_taxon = tx,
    path_exists = exists_ok, readable = readable, SHA256_match = sha_match,
    manifest_QC_pass = qc_pass,
    treatment = mani$non_native_treatment[i], stringsAsFactors = FALSE))
  if (i %% 100 == 0) cat(sprintf("[%d/813] %s\n", i, tx))
}
write.csv(out, file.path(DIR_SPP_QC, "non_native_manifest_813_final_freeze_QC.csv"), row.names = FALSE)

summ <- data.frame(
  manifest_path = mani_path,
  rows = nrow(mani),
  unique_taxa = length(unique(mani$accepted_taxon)),
  duplicates = sum(duplicated(mani$accepted_taxon)),
  paths_exist = sum(out$path_exists),
  readable = sum(out$readable),
  SHA256_complete_and_match = sum(out$SHA256_match),
  QC_PASS = sum(out$manifest_QC_pass),
  global_equal_cases = sum(mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL"),
  global_equal_set_approved = ge_ok,
  rostratum_absent = !("Solanum rostratum" %in% mani$accepted_taxon),
  final_manifest_SHA256 = sha256_of(mani_path),
  FREEZE_PASS = nrow(mani) == 813 &&
                length(unique(mani$accepted_taxon)) == 813 &&
                sum(duplicated(mani$accepted_taxon)) == 0 &&
                sum(out$path_exists) == 813 &&
                sum(out$readable) == 813 &&
                sum(out$SHA256_match) == 813 &&
                sum(out$manifest_QC_pass) == 813 &&
                ge_ok &&
                !("Solanum rostratum" %in% mani$accepted_taxon),
  stringsAsFactors = FALSE)
write.csv(summ, file.path(DIR_SPP_QC, "non_native_manifest_813_final_freeze_summary.csv"), row.names = FALSE)
print(summ)
cat("DONE\n")
