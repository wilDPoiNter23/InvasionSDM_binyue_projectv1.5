# Original source: repair_targeted_modeling_v1.r
# Original source location: ${WORK_ROOT}/repair_targeted_modeling_v1.r
# Original source SHA-256: 2fcd381353832f08282aea0efc619271249faeb86bc271ee1208776843feff1e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_targeted_modeling_v1.r — Targeted repair of 22 negative-TSS/collapse species
#   34-layer primary terrestrial stack only
#   CV: bm_CrossValidation (block, k=3, perc=0.8) BEFORE modeling
#   CV.user.table frozen, passed into BIOMOD_Modeling
#   Compare against known prior TSS from original pipeline evaluations

suppressMessages({
  library(biomod2)
  library(terra)
  library(tidyr)
  library(dplyr)
})

# ── Config ──────────────────────────────────────────────────────────────────
ARGS <- commandArgs(trailingOnly = TRUE)
if (length(ARGS) < 1) stop("Usage: repair_pilot_modeling_v2.r <species_name>")
SP_NAME   <- ARGS[1]

STACK_P  <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OCC_BASE <- "<WORK_ROOT>/data/species_occ"
OUT_DIR  <- "<WORK_ROOT>/results/targeted_negative_TSS_repair_v1"
AUDIT_DIR <- "<WORK_ROOT>/audit"
TMP_DIR  <- file.path(OUT_DIR, "tmp")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TMP_DIR, recursive = TRUE, showWarnings = FALSE)

# CV strategy: BIOMOD2 block (spatial partitioning), k=3 blocks, 80% calib, 1 rep
# Frozen via bm_CrossValidation() BEFORE modeling, passed as CV.user.table
SEED_PA <- 42L
SEED_CV <- 42L
PA_NB   <- 1000L
PA_REP  <- 1L
CV_K    <- 3L
CV_REP  <- 1L
CV_PERC <- 0.8
MODELS  <- c("GLM", "RF", "MAXNET")  # RFd→RF: RFd sampsize bug in BIOMOD2 v4.2.6.1

SP_DIR <- gsub(" ", "_", SP_NAME)

# ── Read queue metadata ─────────────────────────────────────────────────────
queue_df <- read.csv(file.path(AUDIT_DIR, "remaining_targeted_negative_TSS_repair_queue.csv"),
                     stringsAsFactors = FALSE)
sp_row  <- queue_df[queue_df$species == SP_NAME, ]
if (nrow(sp_row) == 0) {
  # Fallback: try pilot selection CSV
  pilot_df <- read.csv(file.path(AUDIT_DIR, "five_species_negative_TSS_repair_selection_v2.csv"),
                       stringsAsFactors = FALSE)
  sp_row <- pilot_df[pilot_df$species == SP_NAME, ]
}
if (nrow(sp_row) == 0) stop("Species not in any queue: ", SP_NAME)

log_file <- file.path(OUT_DIR, paste0(SP_DIR, "_modeling_v2.log"))
log_con  <- file(log_file, open = "wt")
log_msg  <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n", file = log_con, append = TRUE)
  cat(line, "\n")
}
sink(log_con, append = TRUE, split = FALSE, type = "output")
sink(log_con, append = TRUE, split = FALSE, type = "message")

log_msg(paste0("═══════════════════════════════════════════"))
log_msg(paste0("Repair Targeted v1: ", SP_NAME))
log_msg(paste0("Category: ", sp_row$category))
# CV fallback: block CV fails when random PAs leave a spatial fold with val_PA=0
SPATIAL_CV_FAIL_SPECIES <- c("Cinnamomum camphora", "Croton setiger")
CV_STRATEGY <- if (SP_NAME %in% SPATIAL_CV_FAIL_SPECIES) "kfold" else "block"
log_msg(paste0("CV strategy: ", CV_STRATEGY, if(CV_STRATEGY=="random") " (spatial block fallback)" else ""))
log_msg(paste0("═══════════════════════════════════════════"))

# ── Load occurrences ────────────────────────────────────────────────────────
occ_csvs <- list.files(file.path(OCC_BASE, SP_DIR), pattern = "occurrences\\.csv$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(occ_csvs) == 0) stop("No occurrence CSV for ", SP_NAME)
occ <- read.csv(occ_csvs[1], stringsAsFactors = FALSE)
log_msg(paste0("Raw occurrences: ", nrow(occ)))

lat <- occ[["decimalLatitude"]]
lon <- occ[["decimalLongitude"]]
valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
log_msg(paste0("Valid coords: ", sum(valid), "/", length(valid)))

xy_all <- cbind(lon[valid], lat[valid])

# ── Identify frozen cell set (CC in BOTH stacks) ────────────────────────────
# Use P stack bio1 for land mask and cell lookup
stack_p <- terra::rast(STACK_P)
bio1 <- stack_p[["wc2.1_2.5m_bio_1"]]
is_land <- !is.na(terra::extract(bio1, xy_all)[, 1])
xy_land <- xy_all[is_land, , drop = FALSE]
log_msg(paste0("Land cells from raw: ", sum(is_land)))

cells_land <- terra::cellFromXY(bio1, xy_land)
cell_counts <- table(cells_land)
n_unique <- length(cell_counts)
cell_xy <- terra::xyFromCell(bio1, as.integer(names(cell_counts)))
log_msg(paste0("Unique land cells: ", n_unique))

# Extract from 34-layer primary stack
ext_p <- terra::extract(stack_p, cell_xy)
cc_p  <- complete.cases(ext_p[, -1, drop = FALSE])
n_cc_p <- sum(cc_p)

log_msg(paste0("CC Branch P (34-layer): ", n_cc_p, "/", n_unique,
               " (", round(n_cc_p/n_unique*100, 1), "%)"))

# ── Frozen cell set = CC in Branch P ────────────────────────────────────────
frozen_cells <- cell_xy[cc_p, , drop = FALSE]
n_frozen <- nrow(frozen_cells)
log_msg(paste0("Frozen presence cells (CC in P): ", n_frozen))

if (n_frozen < 10) {
  log_msg("FATAL: <10 frozen cells, cannot model")
  quit(status = 1)
}

resp_var <- rep(1L, n_frozen)

# ── Helper: validate a CV table before modeling ──────────────────────────────
# cv_table from bm_CrossValidation(): cols 1..(nb.rep*k) are CV folds,
#   the last col (if do.full.models=TRUE) is all-TRUE for full model.
#   We only validate the actual CV fold columns.
# NOTE: In BIOMOD.formated.data.PA, PAs are stored as NA in data.species
#   (not as 0). PA.table flags ALL rows as TRUE (presences + PAs alike).
#   Presence rows: data.species == 1; PA rows: is.na(data.species).
validate_cv_table <- function(cv_table, fd, branch_label, n_folds) {
  all_resp <- fd@data.species
  is_pres <- !is.na(all_resp) & all_resp == 1
  is_PA   <- is.na(all_resp)  # PAs are NA in data.species for PA objects

  pf <- data.frame()
  check_cols <- seq_len(min(n_folds, ncol(cv_table)))
  for (j in check_cols) {
    cal_mask <- cv_table[, j]                     # TRUE = calibration
    val_mask <- !cv_table[, j]                    # TRUE = validation
    cal_pres <- sum(cal_mask & is_pres, na.rm = TRUE)
    cal_PA   <- sum(cal_mask & is_PA, na.rm = TRUE)
    val_pres <- sum(val_mask & is_pres, na.rm = TRUE)
    val_PA   <- sum(val_mask & is_PA, na.rm = TRUE)
    ok <- cal_pres >= 3 && val_pres >= 1 && cal_PA >= 10 && val_PA >= 1
    pf <- rbind(pf, data.frame(
      branch = branch_label, fold = j,
      cal_pres = cal_pres, cal_PA = cal_PA,
      val_pres = val_pres, val_PA = val_PA,
      ok = ok, stringsAsFactors = FALSE
    ))
  }
  pf
}

# ── Model one branch with pre-generated CV ──────────────────────────────────
model_branch <- function(stack, branch_label) {
  log_msg(paste0("────── ", branch_label, " ──────"))
  n_lyr <- terra::nlyr(stack)
  log_msg(paste0("  Layers: ", n_lyr))

  # 1. BIOMOD_FormatingData with frozen PA seed
  log_msg("  1/4 Formatting data...")
  fd <- tryCatch({
    set.seed(SEED_PA)
    BIOMOD_FormatingData(
      resp.var    = resp_var,
      expl.var    = stack,
      resp.xy     = frozen_cells,
      resp.name   = SP_NAME,
      PA.nb.rep   = PA_REP,
      PA.nb.absences = PA_NB,
      PA.strategy = "random",
      na.rm       = TRUE
    )
  }, error = function(e) {
    log_msg(paste0("  FORMATING ERROR: ", e$message))
    return(NULL)
  })
  if (is.null(fd)) {
    return(list(formatted = NULL, cv_validation = NULL, models = NULL, error = "formatting_failed"))
  }
  fd_rds <- file.path(OUT_DIR, paste0(SP_DIR, "_formatted_", branch_label, ".rds"))
  saveRDS(fd, fd_rds)
  log_msg(paste0("  Formatted data saved: ", basename(fd_rds)))

  # Basic sanity: count presences and PAs
  # In BIOMOD.formated.data.PA: presences = data.species==1, PAs = is.na(data.species)
  resp <- fd@data.species
  n_pres <- sum(!is.na(resp) & resp == 1)
  n_PA   <- sum(is.na(resp))
  log_msg(sprintf("  Sanity: pres=%d, PA=%d, total=%d", n_pres, n_PA, length(resp)))
  if (n_pres < 5) {
    log_msg("  FATAL: <5 presences after formatting")
    return(list(formatted = fd, cv_validation = NULL, models = NULL, error = "too_few_presences"))
  }

  # 2. Pre-generate CV folds with bm_CrossValidation()
  log_msg(sprintf("  2/4 Generating CV folds (%s, k=%d, perc=%.1f)...",
                  CV_STRATEGY, CV_K, CV_PERC))
  cv_table <- tryCatch({
    set.seed(SEED_CV)
    bm_CrossValidation(
      bm.format     = fd,
      strategy      = CV_STRATEGY,
      k             = CV_K,
      nb.rep        = CV_REP,
      perc          = CV_PERC,
      do.full.models = TRUE
    )
  }, error = function(e) {
    log_msg(paste0("  bm_CrossValidation ERROR: ", e$message))
    return(NULL)
  })
  if (is.null(cv_table)) {
    return(list(formatted = fd, cv_validation = NULL, models = NULL, error = "cv_generation_failed"))
  }
  log_msg(sprintf("  CV table: %d rows x %d cols (%d folds + full model)",
                  nrow(cv_table), ncol(cv_table), CV_REP * CV_K))

  # 3. Validate CV folds BEFORE modeling (only the fold columns, not full-model)
  log_msg("  3/4 Validating CV folds...")
  cv_val <- validate_cv_table(cv_table, fd, branch_label, n_folds = CV_REP * CV_K)
  cv_file <- file.path(AUDIT_DIR, paste0(SP_DIR, "_cv_preflight_", branch_label, ".csv"))
  write.csv(cv_val, cv_file, row.names = FALSE)
  log_msg(paste0("  CV preflight saved: ", basename(cv_file)))
  cat(sprintf("\n--- CV Preflight: %s ---\n", branch_label))
  print(cv_val, row.names = FALSE)

  all_ok <- all(cv_val$ok, na.rm = TRUE) && nrow(cv_val) > 0
  if (!all_ok) {
    log_msg("  CV PREFLIGHT FAILED: insufficient data in one or more folds")
    return(list(formatted = fd, cv_validation = cv_val, models = NULL, error = "cv_preflight_failed"))
  }
  log_msg("  CV PREFLIGHT PASSED — all folds have adequate cal/val splits")

  # 4. BIOMOD_Modeling with the PRE-GENERATED CV table
  log_msg("  4/4 Running BIOMOD_Modeling (CV.user.table frozen)...")
  model_opts <- tryCatch({
    bm_ModelingOptions(data.type = "binary", models = MODELS, strategy = "default")
  }, error = function(e) {
    log_msg(paste0("  ModelingOptions error: ", e$message))
    return(NULL)
  })
  if (is.null(model_opts)) {
    return(list(formatted = fd, cv_validation = cv_val, models = NULL, error = "modeling_options_failed"))
  }

  t_start <- Sys.time()
  ind_models <- tryCatch({
    BIOMOD_Modeling(
      bm.format     = fd,
      modeling.id   = paste0(SP_DIR, "_", branch_label),
      models        = MODELS,
      bm.options    = model_opts,
      CV.user.table = cv_table,
      CV.perc       = CV_PERC,
      OPT.strategy  = "bigboss",
      var.import    = 2,
      metric.eval   = c("TSS", "ROC"),
      do.full.models = TRUE,
      seed.val      = SEED_CV,
      nb.cpu        = 2
    )
  }, error = function(e) {
    log_msg(paste0("  MODELING ERROR: ", e$message))
    return(NULL)
  })
  t_elapsed <- difftime(Sys.time(), t_start, units = "mins")
  log_msg(paste0("  Modeling completed in ", round(t_elapsed, 1), " min"))

  if (is.null(ind_models)) {
    return(list(formatted = fd, cv_validation = cv_val, models = NULL, error = "modeling_failed"))
  }

  mod_rds <- file.path(OUT_DIR, paste0(SP_DIR, "_models_", branch_label, ".rds"))
  saveRDS(ind_models, mod_rds)
  log_msg(paste0("  Models saved: ", basename(mod_rds)))

  # ── Evaluation summary ────────────────────────────────────────────────────
  evals <- get_evaluations(ind_models)
  tss_sub <- evals[evals$metric.eval == "TSS" & !is.na(evals$validation), ]
  roc_sub <- evals[evals$metric.eval == "ROC" & !is.na(evals$validation), ]
  tss_best <- if (nrow(tss_sub) > 0) max(tss_sub$validation, na.rm = TRUE) else NA_real_
  roc_best <- if (nrow(roc_sub) > 0) max(roc_sub$validation, na.rm = TRUE) else NA_real_
  tss_med  <- if (nrow(tss_sub) > 0) median(tss_sub$validation, na.rm = TRUE) else NA_real_

  log_msg(sprintf("  RESULTS: best_TSS=%.3f  median_TSS=%.3f  best_ROC=%.3f",
                  tss_best, tss_med, roc_best))

  list(formatted = fd, cv_validation = cv_val, models = ind_models, error = NULL,
       best_TSS = tss_best, median_TSS = tss_med, best_ROC = roc_best,
       all_folds_ok = all_ok)
}

# ── Execute ─────────────────────────────────────────────────────────────────
results <- list()

# ── Execute: Branch P (34-layer primary) for ALL species ────────────────────
# Branch O is skipped: the 40-layer stack has structural NA (clay + HYCOM)
# that cannot produce a valid model. We compare against KNOWN prior TSS
# from the original pipeline's bestEvaluation CSVs.

log_msg("========== Branch P: 34-layer Primary Stack ==========")
res_p <- model_branch(stack_p, "BranchP")
results$BranchP <- res_p

# ── Comparison against prior TSS ────────────────────────────────────────────
log_msg("")
log_msg("══════════ COMPARISON ══════════")
prior_tss <- if ("prior_TSS" %in% names(sp_row)) sp_row$prior_TSS else
             if ("historical_selected_model_TSS" %in% names(sp_row)) sp_row$historical_selected_model_TSS else "UNKNOWN"
sp_cat <- if ("category" %in% names(sp_row)) sp_row$category else
          if ("primary_failure_category" %in% names(sp_row)) sp_row$primary_failure_category else "UNKNOWN"

tss_p <- if (!is.null(res_p$best_TSS)) res_p$best_TSS else NA_real_
roc_p <- if (!is.null(res_p$best_ROC)) res_p$best_ROC else NA_real_

log_msg(sprintf("  Prior TSS (original 40-layer pipeline): %s", prior_tss))
log_msg(sprintf("  Branch P TSS (34-layer repair):         %.3f", tss_p))
log_msg(sprintf("  Branch P ROC:                           %.3f", roc_p))
log_msg(sprintf("  Repair status:                          %s",
          if (is.null(res_p$models)) res_p$error else "modeled"))

# Write comparison CSV
comp <- data.frame(
  species = SP_NAME,
  category = sp_cat,
  prior_TSS = prior_tss,
  BranchP_maxTSS = tss_p,
  BranchP_maxROC = roc_p,
  BranchP_medianTSS = if (!is.null(res_p$median_TSS)) res_p$median_TSS else NA_real_,
  TSS_repair_delta = if (is.numeric(tss_p) && is.numeric(suppressWarnings(as.numeric(prior_tss))))
                     tss_p - as.numeric(prior_tss) else NA_real_,
  BranchP_status = if (is.null(res_p$models)) res_p$error else "modeled",
  all_CV_folds_ok = if (!is.null(res_p$all_folds_ok)) res_p$all_folds_ok else FALSE,
  n_frozen_cells = n_frozen,
  n_CC_34layer = n_cc_p,
  pct_retained = round(n_cc_p / n_unique * 100, 1),
  stringsAsFactors = FALSE
)
write.csv(comp, file.path(OUT_DIR, paste0(SP_DIR, "_comparison.csv")), row.names = FALSE)

# ── Cleanup ─────────────────────────────────────────────────────────────────
gc()
terra::tmpFiles(remove = TRUE)

log_msg("══════════ DONE ══════════")
sink(type = "output")
sink(type = "message")
close(log_con)

cat(paste0("Completed: ", SP_NAME, "\n"))
