# Original source: audit_13sp_retention.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/audit_13sp_retention.r
# Original source SHA-256: dac4512eecf9cda678b0b2867b3a070392f27c1ea28cf03a9fb6c5609f28f814
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# audit_13sp_retention.r — 13-species occurrence retention audit
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages(library(terra))

species_13 <- c(
  "Adiantum latifolium", "Ageratum houstonianum", "Ailanthus altissima",
  "Cenchrus echinatus", "Diodia teres", "Geranium dissectum",
  "Glebionis coronaria", "Hibiscus trionum", "Tamarix aphylla",
  "Veronica arvensis", "Urochloa eminii", "Musa ornata", "Vicia eriocarpa"
)

base_dir <- "<WORK_ROOT>"
fs_orig <- rast("<HPC_INPUT_ROOT>/climate/full_stack.tif")
fs_rep <- rast(file.path(base_dir, "data/predictors/full_stack_terrestrial_repaired_v1.tif"))

results <- data.frame()

for (sp in species_13) {
  cat(sprintf("\n=== %s ===\n", sp))
  
  fmtd_path <- file.path(base_dir, "data/species_formatted", paste0(sp, "_formatted.rds"))
  if (!file.exists(fmtd_path)) {
    cat("  Formatted data NOT FOUND\n")
    results <- rbind(results, data.frame(species=sp, raw_records=NA, valid_coords=NA, terrestrial=NA,
      unique_cells=NA, orig_40band_CC=NA, repaired_35band_CC=NA, HYCOM_recovered=NA,
      HYCOM_loss_confirmed=NA, remaining_NA_sources="NO_FORMATTED_DATA", stringsAsFactors=FALSE))
    next
  }
  
  fmtd <- readRDS(fmtd_path)
  coord_tab <- fmtd@coord
  n_raw <- nrow(coord_tab)
  
  has_lat <- !is.na(coord_tab[,2]) & coord_tab[,2] >= -90 & coord_tab[,2] <= 90
  has_lon <- !is.na(coord_tab[,1]) & coord_tab[,1] >= -180 & coord_tab[,1] <= 180
  valid <- has_lat & has_lon
  n_valid <- sum(valid)
  
  if (n_valid == 0) {
    cat("  No valid coordinates\n")
    results <- rbind(results, data.frame(species=sp, raw_records=n_raw, valid_coords=n_valid, terrestrial=0,
      unique_cells=0, orig_40band_CC=0, repaired_35band_CC=0, HYCOM_recovered=0,
      HYCOM_loss_confirmed=FALSE, remaining_NA_sources="NO_VALID_COORDS", stringsAsFactors=FALSE))
    next
  }
  
  xy <- as.matrix(coord_tab[valid, 1:2])
  
  # Terrestrial check
  bio1_ext <- extract(fs_orig[["wc2.1_2.5m_bio_1"]], xy)[[2]]
  is_land <- !is.na(bio1_ext)
  n_land <- sum(is_land)
  
  if (n_land == 0) {
    cat("  No terrestrial records\n")
    results <- rbind(results, data.frame(species=sp, raw_records=n_raw, valid_coords=n_valid, terrestrial=0,
      unique_cells=0, orig_40band_CC=0, repaired_35band_CC=0, HYCOM_recovered=0,
      HYCOM_loss_confirmed=FALSE, remaining_NA_sources="NO_TERRESTRIAL", stringsAsFactors=FALSE))
    next
  }
  
  xy_land <- xy[is_land, , drop=FALSE]
  cells <- cellFromXY(fs_orig[[1]], xy_land)
  n_unique_cell <- length(unique(cells))
  
  # Complete cases — drop ID column from extract
  ext_orig <- extract(fs_orig, xy_land)
  ext_rep <- extract(fs_rep, xy_land)
  
  ext_orig_data <- ext_orig[, -1, drop=FALSE]
  ext_rep_data <- ext_rep[, -1, drop=FALSE]
  
  n_orig_cc <- sum(complete.cases(ext_orig_data))
  n_rep_cc <- sum(complete.cases(ext_rep_data))
  hycom_recovered <- n_rep_cc - n_orig_cc
  
  # Remaining NA sources (from non-complete rows in repaired stack)
  na_src <- "none"
  if (n_rep_cc < n_land) {
    na_rows <- !complete.cases(ext_rep_data)
    na_by_layer <- colSums(is.na(ext_rep_data[na_rows, , drop=FALSE]))
    top_na <- sort(na_by_layer, decreasing=TRUE)
    na_src <- paste(names(top_na[top_na > 0])[1:3], collapse="; ")
  }
  
  cat(sprintf("  Raw=%d Valid=%d Land=%d Unique=%d OrigCC=%d RepCC=%d Recovered=%d\n",
    n_raw, n_valid, n_land, n_unique_cell, n_orig_cc, n_rep_cc, hycom_recovered))
  
  results <- rbind(results, data.frame(
    species=sp, raw_records=n_raw, valid_coords=n_valid, terrestrial=n_land,
    unique_cells=n_unique_cell, orig_40band_CC=n_orig_cc, repaired_35band_CC=n_rep_cc,
    HYCOM_recovered=hycom_recovered, HYCOM_loss_confirmed=hycom_recovered > 0,
    remaining_NA_sources=na_src, stringsAsFactors=FALSE))
}

write.csv(results, file.path(base_dir, "audit/13_species_occurrence_retention_terrestrial_stack.csv"), row.names=FALSE, quote=FALSE)

cat("\n=== DONE ===\n")
cat(sprintf("HYCOM-induced loss: %d species\n", sum(results$HYCOM_recovered > 0, na.rm=TRUE)))
cat(sprintf("Total HYCOM recovered: %.0f cells\n", sum(results$HYCOM_recovered, na.rm=TRUE)))
