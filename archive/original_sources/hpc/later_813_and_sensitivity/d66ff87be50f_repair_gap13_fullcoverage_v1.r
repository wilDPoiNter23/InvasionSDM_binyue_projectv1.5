# Original source: repair_gap13_fullcoverage_v1.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/repair_gap13_fullcoverage_v1.r
# Original source SHA-256: d66ff87be50fe8034a916775937bcfa1c0853bbd71ae85628c16c3ea42ccb07c
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_gap13_fullcoverage_v1.r — Refit models for 13 repair species
# Usage: Rscript repair_gap13_fullcoverage_v1.r <species_name>
# Uses: 35-layer stack (34 terrestrial + VIIRS, no HYCOM, no clay)
# CV: 4-fold user-defined random CV, seed=42, CV.do.full.models=FALSE
# Algorithms: GLM, RF, MAXNET (MAXNET excluded if technical failure)

suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: repair_gap13_fullcoverage_v1.r <species_name>")
SP_NAME <- args[1]

BASE       <- "<WORK_ROOT>"
STACK_PATH <- file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
MODEL_DIR  <- file.path(BASE, "models")
OUT_DIR    <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1")
AUDIT_DIR  <- file.path(BASE, "publication_release_v1/audits")

SP_DOT  <- gsub(" ", ".", SP_NAME)
SP_USC  <- gsub(" ", "_", SP_NAME)
SEED    <- 42L
N_FOLDS <- 4L

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(AUDIT_DIR, recursive=TRUE, showWarnings=FALSE)

# ── Logging ──
log_file <- file.path(OUT_DIR, paste0(SP_USC, "_repair.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n", file=log_con, append=TRUE)
  cat(line, "\n")
}
sink(log_con, append=TRUE, split=FALSE, type="output")
sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Repair: ", SP_NAME, " ==="))

# ── Disk-backed settings ──
task_tmp <- file.path(BASE, "temp", paste0("repair_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# ── Load original formal data ──
setwd(file.path(MODEL_DIR, SP_DOT))
mf <- list.files(".", pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
if (length(mf) == 0) stop("No .models.out found")

m_hist <- get(load(mf[1]))
fd_hist <- get_formal_data(m_hist)

# Original coordinates and PA
orig_coord  <- fd_hist@coord
orig_pa_cols <- setdiff(colnames(fd_hist@data.species), SP_DOT)
n_pa_runs <- length(orig_pa_cols)
log_msg(sprintf("Original: %d presences, %d PA runs, %d hist predictors",
  nrow(orig_coord), n_pa_runs, length(m_hist@expl.var.names)))

# ── Determine repair predictors ──
stack35 <- rast(STACK_PATH)
hist_preds <- m_hist@expl.var.names
hycom_preds <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")

# Remove HYCOM predictors for repair
repair_preds <- setdiff(hist_preds, hycom_preds)
repair_preds <- intersect(repair_preds, names(stack35))  # ensure all available on 35-stack

log_msg(sprintf("Repair predictors (%d): %s", length(repair_preds), paste(repair_preds, collapse=", ")))

if (length(repair_preds) < 2) stop("Too few repair predictors")

# Subset stack
repair_stack <- stack35[[repair_preds]]
log_msg(sprintf("Repair stack: %d layers", nlyr(repair_stack)))

# ── Create new BIOMOD_FormatingData ──
log_msg("Creating new BIOMOD_FormatingData...")

# Extract response data (presence/absence for first PA run)
resp_name <- SP_DOT
resp_data <- as.numeric(fd_hist@data.species[[resp_name]])
# resp_data should be 0/1 where 1=presence

biomod_data <- tryCatch({
  BIOMOD_FormatingData(
    resp.var   = resp_data,
    expl.var   = repair_stack,
    resp.xy    = orig_coord,
    resp.name  = SP_NAME,
    PA.nb.rep  = n_pa_runs,
    PA.strategy = "random",
    PA.nb.absences = sum(resp_data == 0) %/% n_pa_runs,  # approximate
    na.rm = TRUE
  )
}, error = function(e) {
  log_msg(paste0("BIOMOD_FormatingData ERROR: ", conditionMessage(e)))
  return(NULL)
})

if (is.null(biomod_data)) stop("FormatingData failed")

# Apply the same PA table structure
# For random strategy, BIOMOD generates new PAs. But we want reproducible results.
# We use the seed for reproducibility.
log_msg(sprintf("Formated data: %d sites, %d PA runs", nrow(biomod_data@data.species), n_pa_runs))

# ── Cross-validation design ──
# 4-fold random CV
set.seed(SEED)
n_sites <- nrow(biomod_data@data.species)
fold_ids <- sample(rep(1:N_FOLDS, length.out = n_sites))

cv_table <- data.frame(matrix(0, nrow = n_sites, ncol = N_FOLDS))
colnames(cv_table) <- paste0("RUN", 1:N_FOLDS)
for (k in 1:N_FOLDS) {
  cv_table[fold_ids == k, k] <- FALSE  # held-out
  cv_table[fold_ids != k, k] <- TRUE   # training
}

log_msg(sprintf("CV: %d-fold random, seed=%d", N_FOLDS, SEED))

# ── BIOMOD_ModelingOptions ──
log_msg("Setting BIOMOD_ModelingOptions...")
model_opts <- BIOMOD_ModelingOptions(
  GLM = list(type = "quadratic", interaction.level = 0, test = "AIC"),
  RF = list(do.classif = FALSE, ntree = 500),
  MAXNET = list(path_to_maxent.jar = "<HPC_HOME>/software/maxent/maxent.jar",
                memory = TRUE, maximumiterations = 200)
)

# ── BIOMOD_Modeling ──
log_msg("Running BIOMOD_Modeling (GLM, RF, MAXNET)...")
algorithms <- c("GLM", "RF", "MAXNET")
MAXNET_EXCLUDED <- FALSE

# Try full 3-algorithm modeling first
rc <- system.time({
  mod_repair <- tryCatch({
    BIOMOD_Modeling(
      bm.form = biomod_data,
      models = algorithms,
      models.options = model_opts,
      CV.strategy = "user.defined",
      CV.user.table = cv_table,
      CV.do.full.models = FALSE,
      nb.cpu = 1,
      seed.val = SEED,
      metric.eval = c("TSS", "ROC"),
      var.import = 0,
      weights = NULL,
      do.progress = FALSE
    )
  }, error = function(e) {
    log_msg(paste0("BIOMOD_Modeling ERROR (all 3 algos): ", conditionMessage(e)))
    return(NULL)
  })
})

if (!is.null(mod_repair)) {
  log_msg(sprintf("Modeling complete: %.1f min, %d models built",
    rc[3]/60, length(get_built_models(mod_repair))))
  
  # Check MAXNET completeness
  built <- get_built_models(mod_repair)
  maxnet_built <- grep("MAXNET", built, value=TRUE)
  log_msg(sprintf("MAXNET models: %d/%d", length(maxnet_built), N_FOLDS))
  
  if (length(maxnet_built) < N_FOLDS) {
    MAXNET_EXCLUDED <- TRUE
    log_msg("MAXNET_EXCLUDED_TECHNICAL_INCOMPLETENESS: fewer models than folds")
    
    # Retry without MAXNET
    log_msg("Retrying without MAXNET...")
    mod_repair <- tryCatch({
      BIOMOD_Modeling(
        bm.form = biomod_data,
        models = c("GLM", "RF"),
        models.options = model_opts,
        CV.strategy = "user.defined",
        CV.user.table = cv_table,
        CV.do.full.models = FALSE,
        nb.cpu = 1,
        seed.val = SEED,
        metric.eval = c("TSS", "ROC"),
        var.import = 0,
        do.progress = FALSE
      )
    }, error = function(e) {
      log_msg(paste0("BIOMOD_Modeling ERROR (GLM+RF): ", conditionMessage(e)))
      return(NULL)
    })
  }
}

if (is.null(mod_repair)) stop("All modeling attempts failed")

# ── Save model ──
model_rds <- file.path(OUT_DIR, paste0(SP_USC, "_repair_models.rds"))
saveRDS(mod_repair, model_rds)
log_msg(paste0("Model saved: ", model_rds))

# ── Evaluation summary ──
ev <- mod_repair@models.evaluation@val
built_models <- unique(ev$full.name)

eval_summary <- data.frame(
  species = SP_NAME,
  model_name = character(0),
  algorithm = character(0),
  fold = character(0),
  TSS = numeric(0),
  ROC = numeric(0),
  stringsAsFactors = FALSE
)

for (mn in built_models) {
  sub <- ev[ev$full.name == mn,]
  tss_v <- suppressWarnings(max(sub$validation[sub$metric.eval == "TSS"], na.rm=TRUE))
  roc_v <- suppressWarnings(max(sub$validation[sub$metric.eval == "ROC"], na.rm=TRUE))
  
  # Parse algorithm and fold
  algo <- sub$algo[1]
  run  <- sub$run[1]
  
  eval_summary <- rbind(eval_summary, data.frame(
    species = SP_NAME,
    model_name = mn,
    algorithm = algo,
    fold = run,
    TSS = if(is.finite(tss_v)) tss_v else NA,
    ROC = if(is.finite(roc_v)) roc_v else NA,
    stringsAsFactors = FALSE
  ))
}

# Algorithm-level summary
for (algo in unique(eval_summary$algorithm)) {
  sub <- eval_summary[eval_summary$algorithm == algo & !is.na(eval_summary$TSS),]
  if (nrow(sub) > 0) {
    log_msg(sprintf("  %s: %d folds | TSS median=%.3f min=%.3f max=%.3f | ROC median=%.3f min=%.3f max=%.3f",
      algo, nrow(sub), median(sub$TSS), min(sub$TSS), max(sub$TSS),
      median(sub$ROC), min(sub$ROC), max(sub$ROC)))
  }
}

# Save evaluation
write.csv(eval_summary, file.path(AUDIT_DIR, paste0("gap13_repair_eval_", SP_USC, ".csv")), row.names=FALSE)

# ── Model selection with frozen thresholds ──
primary  <- eval_summary$TSS > 0.8 & eval_summary$ROC > 0.8 & !is.na(eval_summary$TSS)
fallback <- eval_summary$TSS > 0.7 & eval_summary$ROC > 0.7 & !is.na(eval_summary$TSS)
primary[is.na(primary)] <- FALSE
fallback[is.na(fallback)] <- FALSE

if (any(primary)) {
  selected <- eval_summary$model_name[primary]
  tier <- "PRIMARY"
} else if (any(fallback)) {
  selected <- eval_summary$model_name[fallback]
  tier <- "FALLBACK"
} else {
  selected <- character(0)
  tier <- "NONE"
}

log_msg(sprintf("Selection: tier=%s models=%d", tier, length(selected)))

# ── Save selection manifest ──
selection_row <- data.frame(
  species = SP_NAME,
  repair_reason = ifelse(any(hycom_preds %in% hist_preds), 
    "HYCOM_PREDICTOR_INCOMPATIBILITY", "NO_QUALIFIED_HISTORICAL_MODEL"),
  repair_predictors = paste(repair_preds, collapse=";"),
  n_repair_predictors = length(repair_preds),
  selected_model_names = paste(selected, collapse=";"),
  selected_model_count = length(selected),
  selected_algorithms = paste(unique(eval_summary$algorithm[eval_summary$model_name %in% selected]), collapse=";"),
  selection_tier = tier,
  best_TSS = if(length(selected)>0) max(eval_summary$TSS[eval_summary$model_name %in% selected], na.rm=TRUE) else NA,
  best_ROC = if(length(selected)>0) max(eval_summary$ROC[eval_summary$model_name %in% selected], na.rm=TRUE) else NA,
  MAXNET_status = if(MAXNET_EXCLUDED) "EXCLUDED_TECHNICAL_INCOMPLETENESS" else "COMPLETE",
  expected_product_type = if(length(selected)>=2) "ENSEMBLE_EMMEAN_EMMEDIAN" else if(length(selected)==1) "SINGLE_QUALIFIED_MODEL" else "NONE",
  aggregation_product_type = if(length(selected)>=2) "EMmeanByTSS" else if(length(selected)==1) "INDIVIDUAL_CONTINUOUS_PROJECTION" else "NONE",
  ready_for_projection = length(selected) > 0,
  model_rds = model_rds,
  stringsAsFactors = FALSE
)

selection_file <- file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv"))
write.csv(selection_row, selection_file, row.names=FALSE)

log_msg(paste0("DONE. tier=", tier, " ready_for_proj=", selection_row$ready_for_projection))

sink(type="output"); sink(type="message")
close(log_con)

cat(paste0("REPAIR_DONE: ", SP_NAME, " | tier=", tier, " | models=", length(selected), " | ready=", selection_row$ready_for_projection, "\n"))
