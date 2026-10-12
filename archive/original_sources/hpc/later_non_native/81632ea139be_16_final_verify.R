# Original source: 16_final_verify.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/16_final_verify.R
# Original source SHA-256: 81632ea139be035c5e1c146c5bd4240e0efa313142a57b7e8047083cd88f18ef
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 16_final_verify.R — final deliverable verification (run LAST)
# Checks that every canonical deliverable exists and is the country-level version;
# re-runs 06/07 if a non-country-level gpkg was detected.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

ok <- function(msg, cond) cat(sprintf("%-6s %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg))

# 1. gpkg must be the country-level layer
v <- tryCatch(vect(NATIVE_GPKG), error = function(e) NULL)
if (!is.null(v) && !("mapping_source" %in% names(v))) {
  cat("gpkg is NOT country-level -> rebuilding via 06/07\n")
  system2("Rscript", c(file.path(DIR_SCRIPTS, "06_native_polygons.R"),
                       "--country-level"))
  system2("Rscript", c(file.path(DIR_SCRIPTS, "07_native_QC.R")))
}
v <- vect(NATIVE_GPKG)
ok("native_ranges_813.gpkg country-level", "mapping_source" %in% names(v))
ok("gpkg multi-feature: 19720 features / 810 taxa",
   nrow(v) == 19720 && length(unique(v$accepted_taxon)) == 810)

mani <- read.csv(file.path(DIR_SPP_MANIFESTS, "non_native_projection_manifest_813_v1.csv"),
                 stringsAsFactors = FALSE)
ok("813 manifest exists with 813 rows", nrow(mani) == 813)
ok("manifest has 813 COMPLETE products incl. 3 global-equal",
   sum(mani$product_status == "COMPLETE") == 813 &&
   sum(mani$non_native_treatment == "NO_NATIVE_ORIGIN_GLOBAL_EQUAL") == 3)
n_tif <- length(list.files(DIR_SPP_RASTERS, pattern = "__non_native_suitability\\.tif$"))
ok("814 non-native rasters on disk (813 + rostratum add-on)", n_tif == 814)

qc <- read.csv(file.path(DIR_SPP_QC, "non_native_projection_QC_813.csv"),
               stringsAsFactors = FALSE)
ok("species QC 813 rows", nrow(qc) == 813)
ok("all 813 processed QC_pass", sum(qc$QC_pass, na.rm = TRUE) == 813)
ok("all values_preserved", all(qc$values_preserved, na.rm = TRUE))
ok("all geometry preserved", all(qc$geometry_match, na.rm = TRUE))

ok("aggregate raster exists", file.exists(AGG_OUT_RASTER))
ok("aggregate QC exists", file.exists(file.path(DIR_AGG_QC, "non_native_aggregate_QC.csv")))
ok("comparison table exists", file.exists(file.path(DIR_TABLES, "global_vs_non_native_aggregate_comparison.csv")))
ok("figures exist", length(list.files(DIR_FIGURES, pattern = "\\.pdf$")) >= 2)
ok("README exists", file.exists(file.path(DIR_PROVENANCE, "..", "00_README", "README_non_native_potential.md")))
ok("provenance exists", file.exists(file.path(DIR_PROVENANCE, "non_native_product_provenance.md")))

# frozen assets untouched (hash of manifest v3 should still match the frozen record)
frozen_sha <- sha256_of(FROZEN_MANIFEST_V3)
cat("frozen manifest v3 SHA256 (reference only):", frozen_sha, "\n")
cat("\nFinal verification complete.\n")
