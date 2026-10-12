# Original source: repair_single_species_v2.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/repair_single_species_v2.r
# Original source SHA-256: 28a26e67b3be6a6831d2c3ba1b77bb4d9ddb0937530cad819a936f732f93b5f8
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_single_species_v2.r — Targeted negative-TSS repair pilot
# Branch O: 40-layer original stack (diagnostic, reproduce failure)
# Branch P: 34-layer primary terrestrial stack (repair)
# Both branches: same PA design, same CV folds, frozen seed
# Preflight: CV fold validation before fitting

suppressMessages({
  library(biomod2)
  library(terra)
})

# ── Config ──────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: repair_single_species_v2.r <species_key>")

PILOT_SELECTION <- "audit/five_species_negative_TSS_repair_selection_v2.csv"
STACK_O  <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
STACK_P  <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OCC_BASE <- "data/species_occ"
OUT_DIR  <- "results/repair_pilot_v2"
LOG_DIR  <- file.path(OUT_DIR, "logs")
AUDIT_DIR <- "audit"

SEED_PA <- 42L
SEED_CV <- 42L
PA_NB <- 1000L
PA_REP <- 1L
CV_K <- 3L
CV_PERC <- 0.8
MODELS <- c("GLM", "RFd", "MAXNET")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Load pilot metadata ─────────────────────────────────────────────────────
pilot_df <- read.csv(PILOT_SELECTION, stringsAsFactors = FALSE)
sp_key    <- args[1]
is_control <- grepl("^control", sp_key, ignore.case = TRUE)

# Match by species or category keyword
if (sp_key %in% pilot_df$species) {
  sp_row <- pilot_df[pilot_df$species == sp_key,]
} else if (sp_key %in% pilot_df$category) {
  sp_row <- pilot_df[pilot_df$category == sp_key,]
} else {
  stop("Unknown species key: ", sp_key, " (valid: ", paste(pilot_df$species, collapse=", "),
       " or categories: ", paste(pilot_df$category, collapse=", "), ")")
}
SP_NAME <- sp_row$species
SP_DIR  <- gsub(" ", "_", SP_NAME)
SP_CAT  <- sp_row$category

log_con <- file(file.path(LOG_DIR, paste0(SP_DIR, "_repair_v2.log")), open = "wt")
log_msg <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n", file = log_con, append = TRUE)
  cat(line, "\n")
}
sink(log_con, append = TRUE, split = FALSE, type = "output")
sink(log_con, append = TRUE, split = FALSE, type = "message")

log_msg(paste0("=== Repair Pilot v2: ", SP_NAME, " ==="))
log_msg(paste0("Category: ", SP_CAT))

# ── Locate occurrence file ──────────────────────────────────────────────────
occ_csvs <- list.files(file.path(OCC_BASE, SP_DIR), pattern = "occurrences\\.csv$",
                       full.names = TRUE, ignore.case = TRUE)
if (length(occ_csvs) == 0) stop("No occurrence CSV found for ", SP_NAME)
occ_raw <- read.csv(occ_csvs[1], stringsAsFactors = FALSE)
log_msg(paste0("Raw occurrences: ", nrow(occ_raw)))

# ── Filter valid coordinates ────────────────────────────────────────────────
lat <- occ_raw[["decimalLatitude"]]
lon <- occ_raw[["decimalLongitude"]]
valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
lat_c <- lat[valid]; lon_c <- lon[valid]
xy_all <- cbind(lon_c, lat_c)
log_msg(paste0("Valid coordinates: ", sum(valid), " / ", length(valid)))

# ── Get unique grid cells on land (use wc2.1_2.5m_bio_1 from P stack) ──────
stack_p <- rast(STACK_P)
bio1 <- stack_p[["wc2.1_2.5m_bio_1"]]
is_land <- !is.na(extract(bio1, xy_all)[, 1])
xy_land <- xy_all[is_land, , drop = FALSE]
cells_land <- cellFromXY(bio1, xy_land)
cell_counts <- table(cells_land)
n_unique <- length(cell_counts)
cell_xy <- xyFromCell(bio1, as.integer(names(cell_counts)))
log_msg(paste0("Unique land cells: ", n_unique))

# ── Extract values from BOTH stacks ─────────────────────────────────────────
stack_o <- rast(STACK_O)
ext_o <- extract(stack_o, cell_xy)
ext_p <- extract(stack_p, cell_xy)

cc_o <- complete.cases(ext_o[, -1, drop = FALSE])
cc_p <- complete.cases(ext_p[, -1, drop = FALSE])
n_cc_o <- sum(cc_o)
n_cc_p <- sum(cc_p)
log_msg(paste0("CC Branch O (40-layer): ", n_cc_o, " / ", n_unique, " (",
               round(n_cc_o / n_unique * 100, 1), "%)"))
log_msg(paste0("CC Branch P (34-layer): ", n_cc_p, " / ", n_unique, " (",
               round(n_cc_p / n_unique * 100, 1), "%)"))

if (n_cc_p < 10) stop("Not enough complete cases in Branch P for modeling")

# ── Build resp_var (binary response) ────────────────────────────────────────
# Use Branch P CC mask for both branches (frozen cell set)
frozen_cells <- cell_xy[cc_p, , drop = FALSE]
frozen_vals_p <- ext_p[cc_p, -1, drop = FALSE]
frozen_vals_o <- ext_o[cc_p, -1, drop = FALSE]

resp_var <- rep(1L, nrow(frozen_cells))
log_msg(paste0("Frozen presences (CC in P): ", length(resp_var)))

# ── Helper: preflight CV ────────────────────────────────────────────────────
preflight_cv <- function(formatted_data, branch_label, n_pres) {
  # Access the internal BIOMOD calibration/validation masks
  # BIOMOD stores: formatted_data@calib.lines  -> list of calibration indices per run
  calib <- formatted_data@calib.lines
  if (is.null(calib) || length(calib) == 0) {
    log_msg(paste0("PREFLIGHT ", branch_label, ": NO calib.lines — CV not yet assigned!"))
    return(data.frame(branch=branch_label, fold=NA, cal_pres=NA, cal_PA=NA,
                       val_pres=NA, val_PA=NA, ok=FALSE))
  }

  # data.species: 1 for presence, 0 for PA, NA otherwise
  all_resp <- formatted_data@data.species
  n_total <- length(all_resp)
  n_PA <- sum(all_resp == 0, na.rm = TRUE)

  preflight <- data.frame()
  for (j in seq_along(calib)) {
    cal_idx <- calib[[j]]
    # Logical mask: TRUE in calibration set, AND known response
    cal_mask <- (seq_len(n_total) %in% cal_idx) & !is.na(all_resp)
    val_mask <- !(seq_len(n_total) %in% cal_idx) & !is.na(all_resp)

    cal_pres <- sum(all_resp[cal_mask] == 1, na.rm = TRUE)
    cal_PA   <- sum(all_resp[cal_mask] == 0, na.rm = TRUE)
    val_pres <- sum(all_resp[val_mask] == 1, na.rm = TRUE)
    val_PA   <- sum(all_resp[val_mask] == 0, na.rm = TRUE)

    ok <- cal_pres >= 3 && val_pres >= 1 && cal_PA >= 10 && val_PA >= 1

    preflight <- rbind(preflight, data.frame(
      branch = branch_label, fold = j,
      cal_pres = cal_pres, cal_PA = cal_PA,
      val_pres = val_pres, val_PA = val_PA,
      ok = ok, stringsAsFactors = FALSE
    ))
  }
  preflight
}

# ── Branch O: 40-layer original stack ───────────────────────────────────────
do_branch <- function(stack, vals, branch_label, formatted_path) {
  log_msg(paste0("--- ", branch_label, " ---"))

  # Build raster from template
  r_env <- rast(stack[[1]], nlyrs = nlyr(stack))
  values(r_env) <- NA
  # Assign values only at frozen cells
  cell_ids <- cellFromXY(r_env[[1]], frozen_cells)
  for (ly in 1:nlyr(stack)) {
    r_env[[ly]][cell_ids] <- vals[, ly]
    names(r_env)[ly] <- names(stack)[ly]
  }
  names(r_env) <- names(stack)

  # Remove layers that are all-NA (clay, etc.)
  na_per_layer <- global(r_env, function(x) sum(is.na(x)))$sum
  keep <- na_per_layer < ncell(r_env)  # has at least some non-NA
  if (sum(keep) < nlyr(r_env)) {
    log_msg(paste0("Dropping all-NA layers: ", paste(names(r_env)[!keep], collapse=", ")))
    r_env <- r_env[[which(keep)]]
  }
  log_msg(paste0("Effective layers: ", nlyr(r_env)))

  # BIOMOD_FormatingData
  fd <- tryCatch({
    BIOMOD_FormatingData(
      resp.var = resp_var,
      expl.var = r_env,
      resp.xy = frozen_cells,
      resp.name = SP_NAME,
      PA.nb.rep = PA_REP,
      PA.nb.absences = PA_NB,
      PA.strategy = "random",
      na.rm = TRUE
    )
  }, error = function(e) {
    log_msg(paste0(branch_label, " FORMATING ERROR: ", e$message))
    return(NULL)
  })
  if (is.null(fd)) return(NULL)
  saveRDS(fd, file = formatted_path)
  log_msg(paste0("Formatted data saved: ", formatted_path))

  # ── Preflight ─────────────────────────────────────────────────────────────
  pf <- preflight_cv(fd, branch_label, length(resp_var))
  pf_file <- file.path(AUDIT_DIR, paste0(SP_DIR, "_preflight_", branch_label, ".csv"))
  write.csv(pf, pf_file, row.names = FALSE)
  log_msg(paste0("Preflight saved: ", pf_file))

  all_ok <- all(pf$ok, na.rm = TRUE)
  if (!all_ok) {
    log_msg(paste0("PREFLIGHT FAILED for ", branch_label, ":"))
    print(pf)
    log_msg("Skipping modeling for this branch.")
    return(list(formatted = fd, preflight = pf, models = NULL, error = "preflight_failed"))
  }
  log_msg(paste0("PREFLIGHT PASSED for ", branch_label))

  # ── Modeling ──────────────────────────────────────────────────────────────
  model_opts <- bm_ModelingOptions(
    data.type = "binary",
    models = MODELS,
    strategy = "default"
  )

  individual_models <- tryCatch({
    BIOMOD_Modeling(
      bm.format = fd,
      modeling.id = paste0(SP_NAME, "_", branch_label),
      models = MODELS,
      bm.options = model_opts,
      CV.strategy = "block",
      CV.nb.rep = 1L,
      CV.k = CV_K,
      CV.perc = CV_PERC,
      OPT.strategy = "bigboss",
      var.import = 2,
      metric.eval = c("TSS", "ROC"),
      do.full.models = TRUE,
      seed.val = SEED_CV,
      nb.cpu = 2
    )
  }, error = function(e) {
    log_msg(paste0(branch_label, " MODELING ERROR: ", e$message))
    return(NULL)
  })

  list(formatted = fd, preflight = pf, models = individual_models,
       error = if (is.null(individual_models)) "modeling_failed" else NULL)
}

# ── Run both branches ───────────────────────────────────────────────────────
if (!is_control) {
  form_path_o <- file.path(OUT_DIR, paste0(SP_DIR, "_formatted_BranchO.rds"))
  form_path_p <- file.path(OUT_DIR, paste0(SP_DIR, "_formatted_BranchP.rds"))

  log_msg("========== Branch O (40-layer diagnostic) ==========")
  res_o <- do_branch(stack_o, frozen_vals_o, "BranchO", form_path_o)

  log_msg("========== Branch P (34-layer primary) ==========")
  res_p <- do_branch(stack_p, frozen_vals_p, "BranchP", form_path_p)

  # ── Compare ───────────────────────────────────────────────────────────────
  log_msg("========== COMPARISON ==========")
  for (br in c("BranchO", "BranchP")) {
    res <- if (br == "BranchO") res_o else res_p
    if (!is.null(res$models)) {
      evals <- get_evaluations(res$models)
      tss_sub <- evals[evals$metric.eval == "TSS" & !is.na(evals$validation), ]
      roc_sub <- evals[evals$metric.eval == "ROC" & !is.na(evals$validation), ]
      tss_max <- if (nrow(tss_sub) > 0) max(tss_sub$validation, na.rm = TRUE) else NA
      roc_max <- if (nrow(roc_sub) > 0) max(roc_sub$validation, na.rm = TRUE) else NA
      log_msg(sprintf("%s: best TSS=%.3f  best ROC=%.3f", br, tss_max, roc_max))
    } else {
      log_msg(paste0(br, ": No models (", res$error, ")"))
    }
  }
} else {
  # Control species: Branch P only (verification of stability)
  form_path_p <- file.path(OUT_DIR, paste0(SP_DIR, "_formatted_BranchP.rds"))
  log_msg("========== Control: Branch P only (34-layer primary) ==========")
  res_p <- do_branch(stack_p, frozen_vals_p, "BranchP", form_path_p)
  if (!is.null(res_p$models)) {
    evals <- get_evaluations(res_p$models)
    tss_sub <- evals[evals$metric.eval == "TSS" & !is.na(evals$validation), ]
    roc_sub <- evals[evals$metric.eval == "ROC" & !is.na(evals$validation), ]
    log_msg(sprintf("Control %s: best TSS=%.3f  best ROC=%.3f",
              SP_NAME,
              max(tss_sub$validation, na.rm = TRUE),
              max(roc_sub$validation, na.rm = TRUE)))
  }
}

log_msg("========== DONE ==========")
sink(type = "output")
sink(type = "message")
close(log_con)
