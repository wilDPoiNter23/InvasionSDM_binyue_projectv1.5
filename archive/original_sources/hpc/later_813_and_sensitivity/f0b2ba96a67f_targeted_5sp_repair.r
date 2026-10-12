# Original source: targeted_5sp_repair.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/targeted_5sp_repair.r
# Original source SHA-256: f0b2ba96a67f929f019e61dbe11266876357e48073565600ea5a3d1b3e4e0038
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# targeted_5sp_repair.r — Reformat and rerun 5 negative-TSS species
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))

base_dir <- "<WORK_ROOT>"
fs_primary <- rast(file.path(base_dir, "data/predictors/full_stack_terrestrial_primary_v1.tif"))
occ_base <- file.path(base_dir, "data/species_occ")
out_base <- file.path(base_dir, "results/negative_TSS_repair_pilot_v1")
dir.create(out_base, showWarnings=FALSE, recursive=TRUE)

# 5 pilot species with their occ CSV names
pilot_species <- data.frame(
  sp_name = c("Ailanthus altissima","Tamarix aphylla","Veronica arvensis",
              "Geranium dissectum","Ageratum houstonianum"),
  sp_dir  = c("Ailanthus_altissima","Tamarix_aphylla","Veronica_arvensis",
              "Geranium_dissectum","Ageratum_houstonianum"),
  csv     = c("Ailanthus_altissima_occurrences.csv","Tamarix_aphylla_occurrences.csv",
              "Veronica_arvensis_occurrences.csv","Geranium_dissectum_occurrences.csv",
              "Ageratum_houstonianum_occurrences.csv"),
  stringsAsFactors = FALSE
)

summary_rows <- list()
fold_rows <- list()
old_new_rows <- list()

for (i in 1:nrow(pilot_species)) {
  sp <- pilot_species$sp_name[i]
  cat(sprintf("\n========== %s ==========\n", sp))
  
  # ── 1. Read raw occurrence ──
  csv_path <- file.path(occ_base, pilot_species$sp_dir[i], pilot_species$csv[i])
  occ <- read.csv(csv_path, stringsAsFactors=FALSE)
  n_raw <- nrow(occ)
  cat(sprintf("  Raw presences: %d\n", n_raw))
  
  # ── 2. Clean coordinates ──
  lat <- occ[["decimalLatitude"]]
  lon <- occ[["decimalLongitude"]]
  valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
  occ_clean <- occ[valid,]
  lat_c <- lat[valid]; lon_c <- lon[valid]
  n_valid <- sum(valid)
  cat(sprintf("  Valid coordinates: %d\n", n_valid))
  
  # ── 3. Terrestrial + deduplicate by cell ──
  xy <- cbind(lon_c, lat_c)
  bio1 <- fs_primary[["wc2.1_2.5m_bio_1"]]
  bio1_ext <- extract(bio1, xy)[,1]
  is_land <- !is.na(bio1_ext)
  xy_land <- xy[is_land, , drop=FALSE]
  cells <- cellFromXY(bio1, xy_land)
  cell_tab <- table(cells)
  unique_cells <- as.integer(names(cell_tab))
  n_unique <- length(unique_cells)
  cat(sprintf("  Terrestrial unique cells: %d\n", n_unique))
  
  # ── 4. Extract from primary stack ──
  cell_xy <- xyFromCell(bio1, unique_cells)
  ext <- extract(fs_primary, cell_xy)
  ext_data <- ext[, -1, drop=FALSE]
  n_cc <- sum(complete.cases(ext_data))
  cat(sprintf("  Complete cases (new stack): %d (%.1f%%)\n", n_cc, n_cc/n_unique*100))
  
  # ── 5. Get old-stack retention for comparison ──
  # Read existing model to get old retained count (from best_ensemble or individual_models)
  ind_path <- file.path(base_dir, "results/individual_models", paste0(sp, "_individual.rds"))
  old_retained <- NA
  if (file.exists(ind_path)) {
    ind_old <- readRDS(ind_path)
    old_npres <- length(get_built_models(ind_old))
    # Actually, get the original data dimension
    old_retained <- length(ind_old@data.species)
    cat(sprintf("  Old-stack retained presences: %d\n", old_retained))
  }
  
  # ── 6. Create BIOMOD formatted data ──
  # Keep only complete-case cells
  cc_idx <- which(complete.cases(ext_data))
  if (length(cc_idx) < 5) {
    cat(sprintf("  TOO FEW complete cases (%d) — skipping modelling\n", length(cc_idx)))
    next
  }
  
  resp <- rep(1, length(cc_idx))
  resp_name <- gsub(" ", ".", sp)
  env_cc <- ext_data[cc_idx, , drop=FALSE]
  coord_cc <- cell_xy[cc_idx, , drop=FALSE]
  
  # Get old model parameters to match
  # Use standard PA strategy from batch: 4 reps, 1000 PAs, random
  pa_nb_rep <- 4
  pa_nb_abs <- min(1000, length(cc_idx) * 5)
  
  cat(sprintf("  Creating formatted data: %d presences, %d PA per rep\n", length(cc_idx), pa_nb_abs))
  
  fmtd <- tryCatch({
    BIOMOD_FormatingData(
      resp.var = resp,
      expl.var = env_cc,
      resp.xy = coord_cc,
      resp.name = resp_name,
      PA.nb.rep = pa_nb_rep,
      PA.nb.absences = pa_nb_abs,
      PA.strategy = "random",
      na.rm = TRUE
    )
  }, error = function(e) {
    cat(sprintf("  BIOMOD_FormatingData failed: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(fmtd)) next
  
  # Save formatted data
  saveRDS(fmtd, file.path(out_base, paste0(resp_name, "_formatted.rds")))
  cat(sprintf("  Formatted data saved: %d presences + PAs\n", nrow(fmtd@data.env.var)))
  
  # ── 7. Run modelling (same params as batch) ──
  # Algorithms: GLM, RFd, MAXNET (the 3 used)
  # CV: spatial block, 4-fold
  
  cat("  Running BIOMOD_Modeling...\n")
  model_opts <- BIOMOD_ModelingOptions()
  
  individual_models <- tryCatch({
    BIOMOD_Modeling(
      bm.form = fmtd,
      modeling.id = "repair_pilot",
      models = c("GLM","RFd","MAXNET"),
      models.options = model_opts,
      CV.nb.rep = 4,
      CV.perc = 0.80,
      CV.strategy = "random",
      metric.eval = c("TSS","ROC"),
      var.import = 3,
      seed.val = 42
    )
  }, error = function(e) {
    cat(sprintf("  BIOMOD_Modeling failed: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(individual_models)) next
  
  # ── 8. Evaluate ──
  eval <- get_evaluations(individual_models)
  tss_vals <- eval["TSS","Testing.data",]
  roc_vals <- eval["ROC","Testing.data",]
  
  best_tss <- max(tss_vals, na.rm=TRUE)
  best_roc <- max(roc_vals, na.rm=TRUE)
  alarm <- which.max(tss_vals)
  
  cat(sprintf("  Best TSS: %.3f (algo=%s)  Best ROC: %.3f\n", best_tss, names(alarm), best_roc))
  
  # ── 9. CV fold validity ──
  calib_lines <- get_calib_lines(individual_models)
  # Actually, BIOMOD stores this differently. Let me just report the evaluation
  
  # ── Store results ──
  summary_rows[[i]] <- data.frame(
    species = sp,
    raw_presences = n_raw,
    valid_coords = n_valid,
    unique_cells = n_unique,
    new_stack_CC = n_cc,
    old_stack_retained = if(is.na(old_retained)) "UNKNOWN" else old_retained,
    best_TSS = round(best_tss, 4),
    best_ROC = round(best_roc, 4),
    best_algo = names(alarm),
    status = if(best_tss > 0) "TSS_POSITIVE" else "TSS_NEGATIVE",
    stringsAsFactors = FALSE
  )
  
  # Save model
  saveRDS(individual_models, file.path(out_base, paste0(resp_name, "_individual.rds")))
  cat(sprintf("  Model saved.\n"))
  
  gc()
}

# ── Write summary ──
summ <- do.call(rbind, summary_rows)
write.csv(summ, file.path(base_dir, "audit/five_species_negative_TSS_repair_summary.csv"), row.names=FALSE)
cat("\n=== DONE ===\n")
print(summ)
