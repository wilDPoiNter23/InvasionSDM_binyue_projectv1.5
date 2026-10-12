# Original source: repair_pilot_preflight_v2.r
# Original source location: ${WORK_ROOT}/repair_pilot_preflight_v2.r
# Original source SHA-256: 3a3ec6403a08d71dac5839e593f85fd567029ad7481c4b78c2d3a87ecd9ffae7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_pilot_preflight_v2.r — Lightweight ln0 pre-checks BEFORE SLURM submission
# Validates: data existence, CC counts, predictor NA patterns
# Does NOT: format BIOMOD data, validate CV folds (done in SLURM job)

suppressMessages({
  library(terra)
})

PILOT_SELECTION <- "audit/five_species_negative_TSS_repair_selection_v2.csv"
STACK_O  <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
STACK_P  <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OCC_BASE <- "data/species_occ"
AUDIT_DIR <- "audit"

pilot_df <- read.csv(PILOT_SELECTION, stringsAsFactors = FALSE)
cat("=== Repair Pilot v2 Preflight ===\n")
cat(sprintf("Species: %s\n", paste(pilot_df$species, collapse=", ")))
cat(sprintf("Categories: %s\n", paste(pilot_df$category, collapse=", ")))

cat("\n--- Checking data existence ---\n")
all_ok <- TRUE

for (i in 1:nrow(pilot_df)) {
  sp <- pilot_df$species[i]
  sp_dir <- gsub(" ", "_", sp)
  occ_dir <- file.path(OCC_BASE, sp_dir)
  if (!dir.exists(occ_dir)) {
    cat(sprintf("FAIL: Occurrence dir missing: %s\n", occ_dir))
    all_ok <- FALSE
    next
  }
  csvs <- list.files(occ_dir, pattern = "occurrences\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) {
    cat(sprintf("FAIL: No occurrence CSV for %s\n", sp))
    all_ok <- FALSE
    next
  }
  occ <- read.csv(csvs[1], stringsAsFactors = FALSE)
  ok_lat <- "decimalLatitude" %in% names(occ)
  ok_lon <- "decimalLongitude" %in% names(occ)
  if (!ok_lat || !ok_lon) {
    cat(sprintf("FAIL: %s CSV missing lat/lon columns\n", sp))
    all_ok <- FALSE
    next
  }
  cat(sprintf("OK: %s — %d rows, lat/lon present\n", sp, nrow(occ)))
}

cat("\n--- Checking predictor stacks ---\n")
if (!file.exists(STACK_O)) { cat(sprintf("FAIL: O stack missing: %s\n", STACK_O)); all_ok <- FALSE } else cat(sprintf("OK: O stack exists (%s)\n", STACK_O))
if (!file.exists(STACK_P)) { cat(sprintf("FAIL: P stack missing: %s\n", STACK_P)); all_ok <- FALSE } else cat(sprintf("OK: P stack exists (%s)\n", STACK_P))

cat("\n--- CC cell retention (34-layer primary stack) ---\n")
stack_p <- rast(STACK_P)
bio1 <- stack_p[["wc2.1_2.5m_bio_1"]]

pf_results <- data.frame()
for (i in 1:nrow(pilot_df)) {
  sp <- pilot_df$species[i]; cat_str <- pilot_df$category[i]
  sp_dir <- gsub(" ", "_", sp)
  csvs <- list.files(file.path(OCC_BASE, sp_dir), pattern = "occurrences\\.csv$",
                      full.names = TRUE, ignore.case = TRUE)
  occ <- read.csv(csvs[1], stringsAsFactors = FALSE)
  n_raw <- nrow(occ)
  lat <- occ[["decimalLatitude"]]; lon <- occ[["decimalLongitude"]]
  valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
  lat_c <- lat[valid]; lon_c <- lon[valid]
  xy <- cbind(lon_c, lat_c)
  is_land <- !is.na(extract(bio1, xy)[, 1])
  xy_land <- xy[is_land, , drop = FALSE]
  cells <- cellFromXY(bio1, xy_land)
  n_unique <- length(unique(cells))
  cell_xy <- xyFromCell(bio1, as.integer(names(table(cells))))
  ext_p <- extract(stack_p, cell_xy)
  n_cc <- sum(complete.cases(ext_p[, -1, drop = FALSE]))
  pct <- round(n_cc / n_unique * 100, 1)

  # Also check O stack NA at same cells
  stack_o <- rast(STACK_O)
  ext_o <- extract(stack_o, cell_xy)
  n_cc_o <- sum(complete.cases(ext_o[, -1, drop = FALSE]))

  status <- if (n_cc >= 30) "PASS" else "FAIL (<30 CC)"
  cat(sprintf("%-25s raw=%d unique=%d CC_P=%d(%.0f%%) CC_O=%d  [%s]\n",
    sp, n_raw, n_unique, n_cc, pct, n_cc_o, status))
  if (n_cc < 30) all_ok <- FALSE

  pf_results <- rbind(pf_results, data.frame(
    species = sp, category = cat_str, raw = n_raw, valid = sum(valid),
    unique_land = n_unique, CC_34layer = n_cc, pct_CC = pct, CC_40layer = n_cc_o,
    min_CC_met = n_cc >= 30, stringsAsFactors = FALSE
  ))
}

write.csv(pf_results, file.path(AUDIT_DIR, "five_species_negative_TSS_preflight_v2.csv"),
          row.names = FALSE)

cat(sprintf("\n=== Preflight %s ===\n", if (all_ok) "PASSED" else "FAILED"))
cat(sprintf("Saved: %s\n", file.path(AUDIT_DIR, "five_species_negative_TSS_preflight_v2.csv")))

if (!all_ok) {
  cat("\n*** DO NOT SUBMIT — preflight failed ***\n")
  quit(status = 1)
} else {
  cat("\nReady for SLURM submission.\n")
}
