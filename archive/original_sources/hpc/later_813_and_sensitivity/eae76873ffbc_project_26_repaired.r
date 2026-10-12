# Original source: project_26_repaired.r
# Original source location: ${WORK_ROOT}/project_26_repaired.r
# Original source SHA-256: eae76873ffbc4cfa1d22738e7459eacf5717c951f5e89548afb36cd21f39d067
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# project_26_repaired.r — Ensemble + project 26 repaired species using original protocol
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: project_26_repaired.r <species_name>")
SP_NAME <- args[1]

STACK_P <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
MODEL_DIR <- "<WORK_ROOT>/results/targeted_negative_TSS_repair_multifold_v2"
OUT_DIR <- "<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v3"
AUDIT_DIR <- "<WORK_ROOT>/audit"
SEED <- 42L
MAXNET_EXCLUDE <- c("Dracaena trifasciata","Harrisia tortuosa","Melinis minutiflora","Pachira aquatica","Pluchea carolinensis","Urochloa dictyoneura","Vicia eriocarpa")
SP_DIR <- gsub(" ","_",SP_NAME)
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_DIR,"_projection.log"))
log_con <- file(log_file, open="wt")
log_msg <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); line <- paste0("[",ts,"] ",msg); cat(line,"\n",file=log_con,append=TRUE); cat(line,"\n") }
sink(log_con,append=TRUE,split=FALSE,type="output"); sink(log_con,append=TRUE,split=FALSE,type="message")

log_msg(paste0("=== Projection: ",SP_NAME," ==="))

# ── Read frozen model selection CSV ──
SELECTION_CSV <- file.path(AUDIT_DIR, "publication_projection_model_selection_final.csv")
if (!file.exists(SELECTION_CSV)) stop("Frozen selection CSV not found: ", SELECTION_CSV)
selection <- read.csv(SELECTION_CSV, stringsAsFactors = FALSE)

# Filter to this species
selected_models <- selection$full.name[
  selection$species == SP_NAME &
  selection$final_selected %in% TRUE
]
selected_models <- unique(selected_models)

if (length(selected_models) == 0) stop("No frozen qualified model for: ", SP_NAME)

log_msg(sprintf("Frozen selected models: %d", length(selected_models)))
for (m in selected_models) log_msg(sprintf("  %s", m))

# Load multifold model
mod <- readRDS(file.path(MODEL_DIR, paste0(SP_DIR,"_models_multifold_v2.rds")))

# Verify all selected models exist in the built models
built_models <- get_built_models(mod)
if (!all(selected_models %in% built_models)) {
  missing <- setdiff(selected_models, built_models)
  stop("Frozen model list contains unavailable models: ", paste(missing, collapse = "; "))
}
log_msg(sprintf("All %d selected models verified in built_models", length(selected_models)))

# Forbidden MAXNET check for 7 affected species
if (SP_NAME %in% MAXNET_EXCLUDE && any(grepl("MAXNET", selected_models))) {
  stop("Forbidden MAXNET model selected for: ", SP_NAME)
}
log_msg(sprintf("MAXNET check: %s", if(SP_NAME %in% MAXNET_EXCLUDE) "excluded (verified clean)" else "allowed"))

# ── Determine product type and build ──
PRODUCT_TYPE <- if (length(selected_models) >= 2) "ENSEMBLE_EMMEAN_EMMEDIAN" else "SINGLE_QUALIFIED_MODEL"
log_msg(sprintf("Product type: %s (%d models)", PRODUCT_TYPE, length(selected_models)))

if (PRODUCT_TYPE == "ENSEMBLE_EMMEAN_EMMEDIAN") {
  log_msg("Building ensemble (EMmean + EMmedian) from frozen selected models...")
  ens <- tryCatch({
    BIOMOD_EnsembleModeling(
      bm.mod = mod,
      models.chosen = selected_models,
      em.by = "all",
      em.algo = c("EMmean", "EMmedian"),
      metric.select = c("TSS"),
      metric.select.thresh = c(0.0),
      metric.eval = c("TSS", "ROC"),
      var.import = 3,
      EMci.alpha = 0.05,
      EMwmean.decay = "proportional"
    )
  }, error = function(e) { log_msg(paste0("Ensemble ERROR: ", e$message)); NULL })
  if (is.null(ens)) stop("Ensemble build failed")
  saveRDS(ens, file.path(OUT_DIR, paste0(SP_DIR, "_ensemble.rds")))
  log_msg("Ensemble saved")
  proj_model <- ens
} else {
  log_msg("Single qualified model — projecting individual model directly")
  proj_model <- mod
}

# ── Project ──
log_msg("Loading predictor stack...")
stack <- rast(STACK_P)
log_msg(sprintf("Stack: %d layers, %d x %d", nlyr(stack), nrow(stack), ncol(stack)))

if (PRODUCT_TYPE == "ENSEMBLE_EMMEAN_EMMEDIAN") {
  # Project individual models first
  log_msg("Projecting individual selected models...")
  proj_ind <- tryCatch({
    BIOMOD_Projection(
      bm.mod = mod,
      new.env = stack,
      proj.name = paste0(SP_DIR, "_multifold_v3"),
      models.chosen = selected_models,
      binary.meth = NULL,
      clamping.mask = TRUE,
      build.clamping.mask = TRUE,
      output.format = ".grd",
      do.stack = FALSE
    )
  }, error = function(e) { log_msg(paste0("Individual projection ERROR: ", e$message)); NULL })
  if (is.null(proj_ind)) stop("Individual projection failed")
  log_msg("Individual projections completed")
  
  # Build ensemble forecast
  log_msg("Building ensemble forecast...")
  ef <- tryCatch({
    BIOMOD_EnsembleForecasting(
      bm.em = ens,
      bm.proj = proj_ind,
      proj.name = paste0(SP_DIR, "_multifold_v3"),
      models.chosen = "all",
      metric.binary = "TSS",
      metric.filter = "TSS"
    )
  }, error = function(e) { log_msg(paste0("EnsembleForecasting ERROR: ", e$message)); NULL })
  if (is.null(ef)) log_msg("EnsembleForecasting failed — individual projections available")
  else log_msg("Ensemble forecast completed")
} else {
  # Single model projection
  log_msg("Projecting single qualified model...")
  proj_ind <- tryCatch({
    BIOMOD_Projection(
      bm.mod = mod,
      new.env = stack,
      proj.name = paste0(SP_DIR, "_multifold_v3"),
      models.chosen = selected_models,
      binary.meth = NULL,
      clamping.mask = TRUE,
      build.clamping.mask = TRUE,
      output.format = ".grd",
      do.stack = FALSE
    )
  }, error = function(e) { log_msg(paste0("Projection ERROR: ", e$message)); NULL })
  if (is.null(proj_ind)) stop("Projection failed")
  log_msg("Single model projection completed")
}

# ── QC ──
proj_dir <- file.path(OUT_DIR, paste0(SP_DIR, "_multifold_v3"))
if (dir.exists(proj_dir)) {
  rasters <- list.files(proj_dir, pattern = "\\.grd$", full.names = TRUE)
  if (length(rasters) > 0) {
    r <- rast(rasters[1])
    n_non_na <- sum(!is.na(values(r)))
    rng <- range(values(r), na.rm = TRUE)
    log_msg(sprintf("QC: %d non-NA cells, range=[%.4f, %.4f], all_zero=%s",
      n_non_na, rng[1], rng[2], n_non_na == 0 || all(values(r)[!is.na(values(r))] == 0)))
  }
}

log_msg(sprintf("=== DONE: %s (%s) ===", SP_NAME, PRODUCT_TYPE))
sink(type="output"); sink(type="message"); close(log_con)
