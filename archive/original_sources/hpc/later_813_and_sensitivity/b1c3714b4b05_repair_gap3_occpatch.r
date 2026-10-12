# Original source: repair_gap3_occpatch.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/repair_gap3_occpatch.r
# Original source SHA-256: b1c3714b4b0549e26fb985a48a3f43592331264c8bc793c71f585077e378152f
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_gap3_occpatch.r — Re-run 3 failed species with cleaned GBIF occurrences
# Input: <HPC_INPUT_ROOT>/occ_patch/{species_dot}.csv
# Stack: 35-layer (VIIRS safe, no HYCOM, no clay)

suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: repair_gap3_occpatch.r <species_name>")
SP_NAME <- args[1]

BASE       <- "<WORK_ROOT>"
STACK_PATH <- file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
OCC_DIR    <- "<HPC_INPUT_ROOT>/occ_patch"
OUT_DIR    <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1")
SEED       <- 42L

SP_DOT <- gsub(" ", ".", SP_NAME)
SP_USC <- gsub(" ", "_", SP_NAME)
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_USC, "_repair.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); cat(paste0("[",ts,"] ",msg,"\n"), file=log_con, append=TRUE); cat(paste0("[",ts,"] ",msg,"\n")) }
sink(log_con, append=TRUE, split=FALSE, type="output"); sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Repair occ_patch: ", SP_NAME, " ==="))

task_tmp <- file.path(BASE, "temp", paste0("repair3_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# ── Load cleaned occurrences ──
occ_file <- file.path(OCC_DIR, paste0(SP_DOT, ".csv"))
if (!file.exists(occ_file)) stop("Occurrence file not found: ", occ_file)

occ <- read.delim(occ_file, stringsAsFactors=FALSE)
log_msg(sprintf("Raw records: %d", nrow(occ)))

# Extract coordinates
lat <- as.numeric(occ$decimalLatitude)
lon <- as.numeric(occ$decimalLongitude)
valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
occ_clean <- occ[valid, ]
lat <- lat[valid]; lon <- lon[valid]
log_msg(sprintf("Valid coords: %d / %d", sum(valid), nrow(occ)))

# Remove duplicates
coord_df <- data.frame(lon=lon, lat=lat)
coord_unique <- coord_df[!duplicated(coord_df), ]
n_pres <- nrow(coord_unique)
log_msg(sprintf("Unique presences: %d", n_pres))

if (n_pres < 3) stop("Too few unique presences: ", n_pres)

# ── Determine predictors from historical model ──
# Load historical model to get original predictor set
model_sp_dir <- file.path(BASE, "models", SP_DOT)
setwd(model_sp_dir)
mf <- list.files(".", pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
m_hist <- get(load(mf[1]))
hist_preds <- m_hist@expl.var.names

# Remove HYCOM, keep VIIRS and terrestrial
hycom_preds <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
repair_preds <- setdiff(hist_preds, hycom_preds)
stack35 <- rast(STACK_PATH)
repair_preds <- intersect(repair_preds, names(stack35))
log_msg(sprintf("Repair predictors: %d", length(repair_preds)))

# ── Extract from 35-stack ──
repair_stack <- stack35[[repair_preds]]
log_msg("Extracting predictors at occurrence points...")
extracted <- extract(repair_stack, coord_unique)
extracted <- as.data.frame(extracted)

# Remove rows with any NA
complete <- complete.cases(extracted)
log_msg(sprintf("Complete cases: %d / %d", sum(complete), nrow(extracted)))

if (sum(complete) < 5) stop("Too few complete cases: ", sum(complete))

extracted <- extracted[complete, ]
coord_final <- coord_unique[complete, ]
n_pres <- nrow(coord_final)
resp <- rep(1, n_pres)  # all presences for BIOMOD_FormatingData
log_msg(sprintf("Final presences for modeling: %d", n_pres))

# ── BIOMOD_FormatingData ──
# For presence-only, pass the RasterStack so BIOMOD can sample pseudo-absences
log_msg("FormatingData (presence-only, raster-based PA sampling)...")
biomod_data <- tryCatch({
  BIOMOD_FormatingData(resp.var=resp, expl.var=repair_stack, resp.xy=coord_final,
    resp.name=SP_NAME, PA.nb.rep=4L, PA.strategy="random",
    PA.nb.absences=max(1000, n_pres*2), na.rm=TRUE)
}, error=function(e){ log_msg(paste0("FD ERROR: ",conditionMessage(e))); NULL })
if (is.null(biomod_data)) { log_msg("FAILED: FormatingData"); quit(save="no",status=1) }

n_sites <- length(biomod_data@data.species)
log_msg(sprintf("Formated: %d sites (pres + abs)", n_sites))

# ── Adaptive CV ──
n_folds <- min(4L, max(2L, as.integer(floor(n_pres / 10))))
if (n_pres < 20) n_folds <- 2L
log_msg(sprintf("CV: %d-fold (pres=%d)", n_folds, n_pres))

set.seed(SEED)
fold_ids <- sample(rep(1:n_folds, length.out=n_sites))
cv_table <- data.frame(matrix(TRUE, nrow=n_sites, ncol=n_folds))
colnames(cv_table) <- paste0("_PA1_RUN", 1:n_folds)
for (k in 1:n_folds) cv_table[fold_ids==k, k] <- FALSE

# ── Model options ──
model_opts <- suppressWarnings(BIOMOD_ModelingOptions(
  GLM=list(type="quadratic",interaction.level=0,test="AIC"),
  RF=list(do.classif=FALSE,ntree=500),
  MAXNET=list(path_to_maxent.jar="<HPC_HOME>/software/maxent/maxent.jar",
              memory=TRUE,maximumiterations=200)
))

# ── Modeling ──
algo_safe <- c("GLM","RF")
if (n_pres >= 30) algo_safe <- c(algo_safe, "MAXNET")
MAXNET_EXCLUDED <- (n_pres < 30)

log_msg(paste0("Modeling: ", paste(algo_safe,collapse=", ")))
rc <- system.time({
  mod_repair <- tryCatch({
    BIOMOD_Modeling(bm.format=biomod_data, models=algo_safe, bm.options=model_opts,
      CV.strategy="user.defined", CV.user.table=cv_table, CV.do.full.models=FALSE,
      nb.cpu=1, seed.val=SEED, metric.eval=c("TSS","ROC"), var.import=0, do.progress=FALSE)
  }, error=function(e){ log_msg(paste0("Modeling error: ",conditionMessage(e))); NULL })
})

if (is.null(mod_repair)) {
  log_msg("ALL MODELING FAILED")
  sel <- data.frame(species=SP_NAME, tier="NONE", n_selected=0, ready_for_projection=FALSE, stringsAsFactors=FALSE)
  write.csv(sel, file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv")), row.names=FALSE)
  log_msg("DONE. ALL_FAILED"); sink(type="output"); sink(type="message"); close(log_con)
  cat(paste0("REPAIR_FAILED: ",SP_NAME,"\n")); quit(save="no",status=0)
}

log_msg(sprintf("Modeling: %.1f min", rc[3]/60))

# Save
model_rds <- file.path(OUT_DIR, paste0(SP_USC, "_repair_models.rds"))
saveRDS(mod_repair, model_rds)
log_msg(paste0("Saved: ", model_rds, " (", file.info(model_rds)$size/1e6, " MB)"))

# ── Evaluation ──
ev <- mod_repair@models.evaluation@val
built <- unique(ev$full.name)
eval_df <- data.frame(stringsAsFactors=FALSE)
for (mn in built) {
  sub <- ev[ev$full.name==mn,]
  eval_df <- rbind(eval_df, data.frame(model=mn, algo=sub$algo[1], run=sub$run[1],
    TSS=suppressWarnings(max(sub$validation[sub$metric.eval=="TSS"],na.rm=T)),
    ROC=suppressWarnings(max(sub$validation[sub$metric.eval=="ROC"],na.rm=T)),
    stringsAsFactors=FALSE))
}
write.csv(eval_df, file.path(OUT_DIR, paste0("eval_", SP_USC, ".csv")), row.names=FALSE)

# ── Selection ──
tss <- eval_df$TSS; roc <- eval_df$ROC
p_idx <- which(tss>0.8 & roc>0.8); f_idx <- which(tss>0.7 & roc>0.7)
if (length(p_idx)>0) {
  sel_models <- eval_df$model[p_idx]; tier <- "PRIMARY"
} else if (length(f_idx)>0) {
  sel_models <- eval_df$model[f_idx]; tier <- "FALLBACK"
} else {
  sel_models <- character(0); tier <- "NONE"
}

log_msg(sprintf("Tier=%s models=%d", tier, length(sel_models)))
for (a in unique(eval_df$algo)) {
  s <- eval_df[eval_df$algo==a,]
  if (nrow(s)>0) log_msg(sprintf("  %s: %d folds | TSS %.3f/%.3f/%.3f | ROC %.3f/%.3f/%.3f",
    a, nrow(s), min(s$TSS,na.rm=T), median(s$TSS,na.rm=T), max(s$TSS,na.rm=T),
    min(s$ROC,na.rm=T), median(s$ROC,na.rm=T), max(s$ROC,na.rm=T)))
}

# ── Save selection ──
sel <- data.frame(species=SP_NAME, tier=tier, n_selected=length(sel_models),
  models=paste(sel_models,collapse=";"),
  best_TSS=if(length(sel_models)>0) max(tss[c(p_idx,f_idx)],na.rm=T) else NA,
  best_ROC=if(length(sel_models)>0) max(roc[c(p_idx,f_idx)],na.rm=T) else NA,
  MAXNET=if(MAXNET_EXCLUDED) "SKIPPED_LOW_N" else "COMPLETE",
  ready=length(sel_models)>0, n_folds=n_folds, n_pres=n_pres,
  predictors=paste(repair_preds,collapse=";"), stringsAsFactors=FALSE)
write.csv(sel, file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv")), row.names=FALSE)

log_msg(sprintf("DONE. tier=%s n=%d ready=%s", tier, length(sel_models), sel$ready))
sink(type="output"); sink(type="message"); close(log_con)
cat(paste0("REPAIR_DONE: ",SP_NAME," | pres=",n_pres," | tier=",tier," | n=",length(sel_models)," | ready=",sel$ready,"\n"))
