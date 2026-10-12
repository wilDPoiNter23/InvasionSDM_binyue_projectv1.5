# Original source: repair_gap13_v1.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/repair_gap13_v1.r
# Original source SHA-256: d8b230a31301869da3ac0e20c0487beb9b530cad010c6d3d5d9b3fcd20a4787a
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_gap13_v1.r — Model repair for 13 species (refit on 35-stack)
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: repair_gap13_v1.r <species_name>")
SP_NAME <- args[1]

BASE       <- "<WORK_ROOT>"
STACK_PATH <- file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
MODEL_DIR  <- file.path(BASE, "models")
OUT_DIR    <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1")
SEED       <- 42L
N_FOLDS    <- 4L

SP_DOT <- gsub(" ", ".", SP_NAME)
SP_USC <- gsub(" ", "_", SP_NAME)

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

# Logging
log_file <- file.path(OUT_DIR, paste0(SP_USC, "_repair.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); cat(paste0("[",ts,"] ",msg,"\n"), file=log_con, append=TRUE); cat(paste0("[",ts,"] ",msg,"\n")) }
sink(log_con, append=TRUE, split=FALSE, type="output"); sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Repair: ", SP_NAME, " ==="))

# Disk-backed
task_tmp <- file.path(BASE, "temp", paste0("repair_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# Load original formal data
setwd(file.path(MODEL_DIR, SP_DOT))
mf <- list.files(".", pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
m_hist <- get(load(mf[1]))
fd_hist <- get_formal_data(m_hist)

sp_data_hist <- get_species_data(fd_hist)
coord_all <- fd_hist@coord  # includes both presence and absence points
resp_vec <- sp_data_hist[[SP_DOT]]  # 0/1 or 1/NA
n_pres <- sum(resp_vec == 1, na.rm = TRUE)
n_abs  <- sum(is.na(resp_vec))
n_true <- sum(resp_vec == 0, na.rm = TRUE)

log_msg(sprintf("Original: %d sites, pres=%d abs_candidate(NA)=%d true_abs(0)=%d", 
  nrow(coord_all), n_pres, n_abs, n_true))

# Determine repair predictors (remove HYCOM, keep VIIRS)
hist_preds <- m_hist@expl.var.names
hycom_preds <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
repair_preds <- setdiff(hist_preds, hycom_preds)

stack35 <- rast(STACK_PATH)
repair_preds <- intersect(repair_preds, names(stack35))

log_msg(sprintf("Repair predictors (%d): %s", length(repair_preds), paste(repair_preds, collapse=", ")))

# Subset stack and extract values at coordinates
repair_stack <- stack35[[repair_preds]]
log_msg("Extracting predictors from 35-stack...")
extracted <- extract(repair_stack, coord_all)
extracted <- as.data.frame(extracted)

# Check for NAs
na_rows <- which(!complete.cases(extracted))
log_msg(sprintf("Complete cases: %d/%d (NA rows: %d)", nrow(extracted)-length(na_rows), nrow(extracted), length(na_rows)))

if (length(na_rows) > nrow(extracted) * 0.5) stop("Too many NA extractions")

# Create new BIOMOD_FormatingData
log_msg("Creating BIOMOD_FormatingData...")
biomod_data <- tryCatch({
  BIOMOD_FormatingData(
    resp.var = resp_vec,
    expl.var = extracted,
    resp.xy = coord_all,
    resp.name = SP_NAME,
    PA.nb.rep = 4L,
    PA.strategy = "random",
    PA.nb.absences = max(1000, min(n_abs, n_pres * 10)),
    na.rm = TRUE
  )
}, error = function(e) { log_msg(paste0("FormatingData ERROR: ", conditionMessage(e))); NULL })
if (is.null(biomod_data)) stop("FormatingData failed")

log_msg(sprintf("Formated: %d sites, %d expl vars", length(biomod_data@data.species), length(repair_preds)))

# CV design — adapt folds to presence count
n_folds <- min(4L, max(2L, as.integer(floor(n_pres / 2))))
log_msg(sprintf("Adaptive CV: %d-fold (pres=%d)", n_folds, n_pres))

set.seed(SEED)
n_sites <- length(biomod_data@data.species)
fold_ids <- sample(rep(1:n_folds, length.out = n_sites))
cv_table <- data.frame(matrix(TRUE, nrow=n_sites, ncol=n_folds))
colnames(cv_table) <- paste0("_PA1_RUN", 1:n_folds)
for (k in 1:n_folds) cv_table[fold_ids == k, k] <- FALSE
log_msg(sprintf("CV table: %d sites, %d folds", n_sites, n_folds))

# Model options
model_opts <- BIOMOD_ModelingOptions(
  GLM = list(type = "quadratic", interaction.level = 0, test = "AIC"),
  RF = list(do.classif = FALSE, ntree = 500),
  MAXNET = list(path_to_maxent.jar = "<HPC_HOME>/software/maxent/maxent.jar",
                memory = TRUE, maximumiterations = 200)
)

# Run modeling (try GLM+RF+MAXNET, fallback to GLM+RF if MAXNET fails)
log_msg("Running BIOMOD_Modeling...")
MAXNET_EXCLUDED <- FALSE

# Strategy: GLM+RF first (most reliable). MAXNET separately if enough presences.
algo_safe <- c("GLM", "RF")
if (n_pres >= 20) algo_safe <- c(algo_safe, "MAXNET")

rc <- system.time({
  mod_repair <- tryCatch({
    BIOMOD_Modeling(bm.format=biomod_data, models=algo_safe, bm.options=model_opts,
      CV.strategy="user.defined", CV.user.table=cv_table, CV.do.full.models=FALSE,
      nb.cpu=1, seed.val=SEED, metric.eval=c("TSS","ROC"), var.import=0, do.progress=FALSE)
  }, error=function(e){ log_msg(paste0("Modeling error: ",conditionMessage(e))); NULL })
})

# Try MAXNET separately for species with enough presences
if (is.null(mod_repair)) {
  # Fallback: just GLM
  log_msg("GLM+RF failed, trying GLM only")
  mod_repair <- tryCatch({
    BIOMOD_Modeling(bm.format=biomod_data, models="GLM", bm.options=model_opts,
      CV.strategy="user.defined", CV.user.table=cv_table, CV.do.full.models=FALSE,
      nb.cpu=1, seed.val=SEED, metric.eval=c("TSS","ROC"), var.import=0, do.progress=FALSE)
  }, error=function(e){ log_msg(paste0("GLM-only error: ",conditionMessage(e))); NULL })
} else if (n_pres >= 20 && !"MAXNET" %in% algo_safe) {
  # Try adding MAXNET
  log_msg("Attempting MAXNET separately...")
  mod_maxnet <- tryCatch({
    BIOMOD_Modeling(bm.format=biomod_data, models="MAXNET", bm.options=model_opts,
      CV.strategy="user.defined", CV.user.table=cv_table, CV.do.full.models=FALSE,
      nb.cpu=1, seed.val=SEED, metric.eval=c("TSS","ROC"), var.import=0, do.progress=FALSE)
  }, error=function(e){ log_msg(paste0("MAXNET error: ",conditionMessage(e))); NULL })
  if (is.null(mod_maxnet)) {
    MAXNET_EXCLUDED <- TRUE
    log_msg("MAXNET_EXCLUDED_TECHNICAL_INCOMPLETENESS")
  }
}

if (is.null(mod_repair)) {
  log_msg("ALL MODELING FAILED — no qualified models possible")
  sel <- data.frame(species=SP_NAME, repair_reason=ifelse(any(hycom_preds %in% hist_preds),"HYCOM_PREDICTOR_INCOMPATIBILITY","NO_QUALIFIED_HISTORICAL_MODEL"),
    n_repair_preds=length(repair_preds), selected_models="", n_selected=0,
    tier="NONE", best_TSS=NA, best_ROC=NA,
    MAXNET_status=if(MAXNET_EXCLUDED) "EXCLUDED" else "N/A",
    product_type="NONE", ready_for_projection=FALSE, stringsAsFactors=FALSE)
  write.csv(sel, file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv")), row.names=FALSE)
  log_msg("DONE. ALL_FAILED tier=NONE ready=FALSE")
  sink(type="output"); sink(type="message"); close(log_con)
  cat(paste0("REPAIR_FAILED: ", SP_NAME, " | all modeling failed\n"))
  quit(save="no", status=0)
}

log_msg(sprintf("Modeling time: %.1f min", rc[3]/60))

# Save model
model_rds <- file.path(OUT_DIR, paste0(SP_USC, "_repair_models.rds"))
saveRDS(mod_repair, model_rds)

# Evaluation summary
ev <- mod_repair@models.evaluation@val
built_models <- unique(ev$full.name)

eval_summary <- data.frame(species=SP_NAME, model_name=character(0), algorithm=character(0), fold=character(0), TSS=numeric(0), ROC=numeric(0), stringsAsFactors=FALSE)
for (mn in built_models) {
  sub <- ev[ev$full.name == mn,]
  eval_summary <- rbind(eval_summary, data.frame(species=SP_NAME, model_name=mn, algorithm=sub$algo[1], fold=sub$run[1],
    TSS=suppressWarnings(max(sub$validation[sub$metric.eval=="TSS"], na.rm=TRUE)),
    ROC=suppressWarnings(max(sub$validation[sub$metric.eval=="ROC"], na.rm=TRUE)),
    stringsAsFactors=FALSE))
}
write.csv(eval_summary, file.path(OUT_DIR, paste0("eval_", SP_USC, ".csv")), row.names=FALSE)

# Model selection
tss_vals <- eval_summary$TSS; roc_vals <- eval_summary$ROC
primary_idx  <- which(tss_vals > 0.8 & roc_vals > 0.8)
fallback_idx <- which(tss_vals > 0.7 & roc_vals > 0.7)

if (length(primary_idx) > 0) {
  selected <- eval_summary$model_name[primary_idx]; tier <- "PRIMARY"
} else if (length(fallback_idx) > 0) {
  selected <- eval_summary$model_name[fallback_idx]; tier <- "FALLBACK"
} else {
  selected <- character(0); tier <- "NONE"
}

log_msg(sprintf("Selection: tier=%s models=%d", tier, length(selected)))
for (algo in unique(eval_summary$algorithm)) {
  sub <- eval_summary[eval_summary$algorithm==algo,]
  if (nrow(sub)>0) log_msg(sprintf("  %s: %d folds TSS=%.3f/%.3f/%.3f ROC=%.3f/%.3f/%.3f",
    algo, nrow(sub), min(sub$TSS,na.rm=T), median(sub$TSS,na.rm=T), max(sub$TSS,na.rm=T),
    min(sub$ROC,na.rm=T), median(sub$ROC,na.rm=T), max(sub$ROC,na.rm=T)))
}

# Selection manifest
sel <- data.frame(species=SP_NAME, repair_reason=ifelse(any(hycom_preds %in% hist_preds),"HYCOM_PREDICTOR_INCOMPATIBILITY","NO_QUALIFIED_HISTORICAL_MODEL"),
  n_repair_preds=length(repair_preds), selected_models=paste(selected,collapse=";"), n_selected=length(selected),
  tier=tier, best_TSS=if(length(selected)>0) max(tss_vals[primary_idx %||% fallback_idx],na.rm=T) else NA,
  best_ROC=if(length(selected)>0) max(roc_vals[primary_idx %||% fallback_idx],na.rm=T) else NA,
  MAXNET_status=if(MAXNET_EXCLUDED) "EXCLUDED" else "COMPLETE",
  product_type=if(length(selected)>=2) "ENSEMBLE_EMMEAN_EMMEDIAN" else if(length(selected)==1) "SINGLE" else "NONE",
  ready_for_projection=length(selected)>0,
  stringsAsFactors=FALSE)
write.csv(sel, file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv")), row.names=FALSE)

log_msg(paste0("DONE. tier=", tier, " ready=", sel$ready_for_projection))
sink(type="output"); sink(type="message"); close(log_con)
cat(paste0("REPAIR_DONE: ", SP_NAME, " | tier=", tier, " | models=", length(selected), " | ready=", sel$ready_for_projection, "\n"))
