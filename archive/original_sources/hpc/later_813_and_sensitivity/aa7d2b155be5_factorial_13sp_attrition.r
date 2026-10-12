# Original source: factorial_13sp_attrition.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/factorial_13sp_attrition.r
# Original source SHA-256: aa7d2b155be5c9880dc36501b5c9a58c24db19dbda666527018a3027c9d2adc8
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# factorial_13sp_attrition.r — Proper factorial audit from raw occurrence CSVs
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages(library(terra))

# ── Species list with raw CSV info ──
species_13 <- data.frame(
  sp_name = c("Adiantum latifolium","Ageratum houstonianum","Ailanthus altissima",
    "Cenchrus echinatus","Diodia teres","Geranium dissectum",
    "Glebionis coronaria","Hibiscus trionum","Tamarix aphylla",
    "Veronica arvensis","Urochloa eminii","Musa ornata","Vicia eriocarpa"),
  sp_dir = c("Adiantum_latifolium","Ageratum_houstonianum","Ailanthus_altissima",
    "Cenchrus_echinatus","Diodia_teres","Geranium_dissectum",
    "Glebionis_coronaria","Hibiscus_trionum","Tamarix_aphylla",
    "Veronica_arvensis","Urochloa_eminii","Musa_ornata","Vicia_eriocarpa"),
  csv_name = c("Adiantum_latifolium_occurrences.csv","Ageratum_houstonianum_occurrences.csv",
    "Ailanthus_altissima_occurrences.csv","Cenchrus_echinatus_occurrences.csv",
    "Diodia_teres_occurrences.csv","Geranium_dissectum_occurrences.csv",
    "Glebionis_coronaria_occurrences.csv","Hibiscus_trionum_occurrences.csv",
    "Tamarix_aphylla_occurrences.csv","Veronica_arvensis_occurrences.csv",
    "Urochloa_eminii_occurrences.csv","Musa_ornata_occurrences.csv",
    "Vicia_eriocarpa_occurrences.csv"),
  stringsAsFactors = FALSE
)

base_dir <- "<WORK_ROOT>"
occ_base <- file.path(base_dir, "data/species_occ")
fs <- rast("<HPC_INPUT_ROOT>/climate/full_stack.tif")
bio1 <- fs[["wc2.1_2.5m_bio_1"]]

# ── Predictor set definitions ──
all_layers <- names(fs)
clay_layer <- "clay_mean_2024_2_5min_aligned"
hycom_layers <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")

# P0: all 40
keep_P0 <- rep(TRUE, length(all_layers))
# P1: remove clay only
keep_P1 <- !all_layers %in% clay_layer
# P2: remove HYCOM only
keep_P2 <- !all_layers %in% hycom_layers
# P3: remove clay + HYCOM
keep_P3 <- !all_layers %in% c(clay_layer, hycom_layers)
# P4: use repaired stack
fs_rep <- rast(file.path(base_dir, "data/predictors/full_stack_terrestrial_repaired_v1.tif"))
rep_layers <- names(fs_rep)

# ── Output files ──
attrition_csv <- file.path(base_dir, "audit/13_species_factorial_predictor_attrition.csv")
rowlevel_csv <- file.path(base_dir, "audit/13_species_raw_presence_row_level_attrition.csv")
attrition_fh <- file(attrition_csv, "w")
rowlevel_fh <- file(rowlevel_csv, "w")

cat("species,raw_true_presence_rows,valid_coordinate_rows,terrestrial_rows,unique_terrestrial_cells,P0_original_retained,P1_remove_clay_retained,P2_remove_HYCOM_retained,P3_remove_clay_HYCOM_retained,P4_final_terrestrial_retained,clay_effect_given_HYCOM,clay_effect_without_HYCOM,HYCOM_effect_without_clay,HYCOM_effect_with_clay,HYCOM_effect_with_clay_identifiable,final_stack_change,remaining_loss_under_P4,remaining_NA_layers,diagnostic_status\n",
  file=attrition_fh)
cat("species,cell_id,lon,lat,clay_NA,salinity_mean_NA,salinity_range_NA,temperature_mean_NA,temperature_range_NA,any_HYCOM_NA,both_clay_and_HYCOM_NA,any_other_NA,P0_retained,P1_retained,P2_retained,P3_retained,P4_retained\n",
  file=rowlevel_fh)

# ── Process each species ──
for (i in 1:nrow(species_13)) {
  sp <- species_13$sp_name[i]
  cat(sprintf("\n=== %s ===\n", sp))
  
  csv_path <- file.path(occ_base, species_13$sp_dir[i], species_13$csv_name[i])
  
  # Validate source
  if (!file.exists(csv_path)) {
    cat(sprintf("%s,CSV_NOT_FOUND,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,SOURCE_MISSING\n", sp, rep(NA,15)), file=attrition_fh)
    next
  }
  
  # Read raw CSV
  occ <- read.csv(csv_path, stringsAsFactors=FALSE)
  n_raw <- nrow(occ)
  
  # Validate columns
  lat_col <- if("decimalLatitude" %in% names(occ)) "decimalLatitude" else grep("lat", names(occ), ignore.case=TRUE, value=TRUE)[1]
  lon_col <- if("decimalLongitude" %in% names(occ)) "decimalLongitude" else grep("lon", names(occ), ignore.case=TRUE, value=TRUE)[1]
  
  if (is.na(lat_col) || is.na(lon_col)) {
    cat(sprintf("%s,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,NO_COORD_COLUMNS\n", sp, n_raw, rep(NA,14)), file=attrition_fh)
    next
  }
  
  lat <- as.numeric(occ[[lat_col]])
  lon <- as.numeric(occ[[lon_col]])
  
  # All rows are true presences (occurrence-only CSV)
  n_pres <- n_raw
  
  # Valid coordinates
  valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
  n_valid <- sum(valid)
  if (n_valid == 0) {
    cat(sprintf("%s,%d,%d,0,0,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,NO_VALID_COORDS\n", sp, n_pres, n_valid, rep(NA,12)), file=attrition_fh)
    next
  }
  
  lat_v <- lat[valid]; lon_v <- lon[valid]
  
  # Terrestrial check
  xy <- cbind(lon_v, lat_v)
  bio1_ext <- extract(bio1, xy)[,1]
  is_land <- !is.na(bio1_ext)
  n_land <- sum(is_land)
  if (n_land == 0) {
    cat(sprintf("%s,%d,%d,%d,0,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,NO_TERRESTRIAL\n", sp, n_pres, n_valid, n_land, rep(NA,12)), file=attrition_fh)
    next
  }
  
  xy_land <- xy[is_land, , drop=FALSE]
  
  # Unique 2.5 arc-min cells
  cells <- cellFromXY(bio1, xy_land)
  cell_tab <- table(cells)
  unique_cells <- as.integer(names(cell_tab))
  n_unique <- length(unique_cells)
  
  cat(sprintf("  Raw=%d Valid=%d Land=%d UniqueCells=%d\n", n_pres, n_valid, n_land, n_unique))
  
  # Extract all 40 predictor layers ONCE at unique cells
  cell_xy <- xyFromCell(bio1, unique_cells)
  ext_all <- extract(fs, cell_xy)
  ext_data <- ext_all[, -1, drop=FALSE]  # drop ID
  
  # ── Calculate P0-P4 retention using column indices, not logical vectors ──
  ext_cols <- colnames(ext_data)
  idx_P0 <- 1:ncol(ext_data)                                    # all
  idx_P1 <- which(!ext_cols %in% clay_layer)                    # remove clay
  idx_P2 <- which(!ext_cols %in% hycom_layers)                  # remove HYCOM
  idx_P3 <- which(!ext_cols %in% c(clay_layer, hycom_layers))   # remove both
  
  n_P0 <- sum(complete.cases(ext_data[, idx_P0, drop=FALSE]))
  n_P1 <- sum(complete.cases(ext_data[, idx_P1, drop=FALSE]))
  n_P2 <- sum(complete.cases(ext_data[, idx_P2, drop=FALSE]))
  n_P3 <- sum(complete.cases(ext_data[, idx_P3, drop=FALSE]))
  
  # P4: extract from repaired stack at same cells
  ext_rep <- extract(fs_rep, cell_xy)
  ext_rep_data <- ext_rep[, -1, drop=FALSE]
  n_P4 <- sum(complete.cases(ext_rep_data))
  
  # ── Contrasts ──
  clay_effect_given_HYCOM  <- n_P1 - n_P0
  clay_effect_without_HYCOM <- n_P3 - n_P2
  HYCOM_effect_without_clay <- n_P3 - n_P1
  HYCOM_effect_with_clay    <- n_P2 - n_P0
  HYCOM_identifiable <- n_P0 == 0  # FALSE if P0 is zero (structural clay failure)
  final_stack_change <- n_P4 - n_P3
  remaining_loss <- n_unique - n_P4
  diag_status <- if(n_P0==0 && n_P2==0) "CLAY_STRUCTURAL_ZERO" else if(HYCOM_effect_without_clay>0) "HYCOM_EFFECT_DETECTED" else "HYCOM_EFFECT_ZERO"
  
  # ── Remaining NA layers ──
  na_src <- "none"
  if (remaining_loss > 0) {
    na_rows <- !complete.cases(ext_rep_data)
    na_l <- sort(colSums(is.na(ext_rep_data[na_rows, , drop=FALSE])), decreasing=TRUE)
    na_src <- paste(names(na_l[na_l > 0])[1:3], collapse="; ")
  }
  
  # ── Write attrition row ──
  cat(sprintf("%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%d,%d,%s,%s\n",
    sp, n_pres, n_valid, n_land, n_unique,
    n_P0, n_P1, n_P2, n_P3, n_P4,
    clay_effect_given_HYCOM, clay_effect_without_HYCOM,
    HYCOM_effect_without_clay, HYCOM_effect_with_clay,
    if(HYCOM_identifiable) "yes" else "no_structural_zero",
    final_stack_change, remaining_loss, na_src, diag_status),
    file=attrition_fh)
  
  # ── Row-level flags (unique cells) ──
  clay_NA_flag <- is.na(ext_data[[clay_layer]])
  sal_m_NA <- is.na(ext_data[["salinity_mean"]])
  sal_r_NA <- is.na(ext_data[["salinity_range"]])
  tmp_m_NA <- is.na(ext_data[["temperature_mean"]])
  tmp_r_NA <- is.na(ext_data[["temperature_range"]])
  any_HYCOM_NA <- sal_m_NA | sal_r_NA | tmp_m_NA | tmp_r_NA
  both_NA <- clay_NA_flag & any_HYCOM_NA
  any_other_NA <- rowSums(is.na(ext_data[, !colnames(ext_data) %in% c(clay_layer, hycom_layers), drop=FALSE])) > 0
  
  for (j in seq_along(unique_cells)) {
    cat(sprintf("%s,%d,%.4f,%.4f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
      sp, unique_cells[j], cell_xy[j,1], cell_xy[j,2],
      clay_NA_flag[j], sal_m_NA[j], sal_r_NA[j], tmp_m_NA[j], tmp_r_NA[j],
      any_HYCOM_NA[j], both_NA[j], any_other_NA[j],
      !anyNA(ext_data[j, idx_P0, drop=FALSE]), !anyNA(ext_data[j, idx_P1, drop=FALSE]),
      !anyNA(ext_data[j, idx_P2, drop=FALSE]), !anyNA(ext_data[j, idx_P3, drop=FALSE]),
      complete.cases(ext_rep_data[j, , drop=FALSE])),
      file=rowlevel_fh)
  }
}

close(attrition_fh)
close(rowlevel_fh)
cat("\n=== DONE ===\n")
cat(sprintf("Attrition: %s\n", attrition_csv))
cat(sprintf("Row-level: %s\n", rowlevel_csv))
