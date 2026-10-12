# Original source: repair_gap13_v2.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/repair_gap13_v2.r
# Original source SHA-256: 2a01009c8251228aeb392ecf999db1e525490d03367bea1df9e6f47a3898952e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_gap13_v2.r — Minimal repair: reuse historical formal data, subset predictors
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: repair_gap13_v2.r <species_name>")
SP_NAME <- args[1]

BASE       <- "<WORK_ROOT>"
STACK_PATH <- file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
MODEL_DIR  <- file.path(BASE, "models")
OUT_DIR    <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1")
SEED       <- 42L

SP_DOT <- gsub(" ", ".", SP_NAME)
SP_USC <- gsub(" ", "_", SP_NAME)
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_USC, "_repair.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); cat(paste0("[",ts,"] ",msg,"\n"), file=log_con, append=TRUE); cat(paste0("[",ts,"] ",msg,"\n")) }
sink(log_con, append=TRUE, split=FALSE, type="output"); sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Repair v2: ", SP_NAME, " ==="))

task_tmp <- file.path(BASE, "temp", paste0("repair2_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# ── Load historical data ──
setwd(file.path(MODEL_DIR, SP_DOT))
mf <- list.files(".", pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
m_hist <- get(load(mf[1]))
fd_hist <- get_formal_data(m_hist)

sp_data <- get_species_data(fd_hist)
coord_all <- fd_hist@coord
n_pres <- sum(sp_data[[SP_DOT]] == 1, na.rm=TRUE)
log_msg(sprintf("Historical: %d sites, %d presences", nrow(coord_all), n_pres))

# ── Determine repair predictors ──
hist_preds <- m_hist@expl.var.names
hycom_preds <- c("salinity_mean","salinity_range","temperature_mean","temperature_range")
repair_preds <- setdiff(hist_preds, hycom_preds)
stack35 <- rast(STACK_PATH)
repair_preds <- intersect(repair_preds, names(stack35))
log_msg(sprintf("Repair predictors: %d", length(repair_preds)))

# ── Extract from 35-stack ──
repair_stack <- stack35[[repair_preds]]
log_msg("Extracting predictors...")
extracted <- extract(repair_stack, coord_all)
extracted <- as.data.frame(extracted)
na_rows <- which(!complete.cases(extracted))
log_msg(sprintf("Complete cases: %d/%d", nrow(extracted)-length(na_rows), nrow(extracted)))

# Build response: 1=presence, NA=absence
resp <- sp_data[[SP_DOT]]
resp[is.na(resp)] <- NA  # ensure NAs stay NA
log_msg(sprintf("Response: %d presences, %d NA (absence candidates)", sum(resp==1,na.rm=T), sum(is.na(resp))))

# ── BIOMOD_FormatingData ──
log_msg("FormatingData...")
biomod_data <- tryCatch({
  BIOMOD_FormatingData(resp.var=resp, expl.var=extracted, resp.xy=coord_all,
    resp.name=SP_NAME, PA.nb.rep=4L, PA.strategy="random",
    PA.nb.absences=max(1000, n_pres*10), na.rm=TRUE)
}, error=function(e){ log_msg(paste0("FD ERROR: ",conditionMessage(e))); NULL })
if (is.null(biomod_data)) { log_msg("FormatingData FAILED"); sink(type="output"); sink(type="message"); close(log_con); quit(save="no",status=1) }

n_sites <- length(biomod_data@data.species)
log_msg(sprintf("Formated: %d sites", n_sites))

# ── Adaptive CV ──
n_folds <- min(4L, max(2L, as.integer(floor(n_pres/2))))
log_msg(sprintf("CV: %d-fold (pres=%d)", n_folds, n_pres))
set.seed(SEED)
fold_ids <- sample(rep(1:n_folds, length.out=n_sites))
cv_table <- data.frame(matrix(TRUE, nrow=n_sites, ncol=n_folds))
colnames(cv_table) <- paste0("_PA1_RUN", 1:n_folds)
for (k in 1:n_folds) cv_table[fold_ids==k, k] <- FALSE

# ── Model options ──
model_opts <- tryCatch(
  suppressWarnings(BIOMOD_ModelingOptions(
    GLM=list(type="quadratic",interaction.level=0,test="AIC"),
    RF=list(do.classif=FALSE,ntree=500),
    MAXNET=list(path_to_maxent.jar="<HPC_HOME>/software/maxent/maxent.jar",memory=TRUE,maximumiterations=200)
  )), error=function(e) NULL)
if (is.null(model_opts)) model_opts <- bm_ModelingOptions(data.type="binary",models=c("GLM","RF","MAXNET"))

# ── Modeling: GLM+RF (safe), MAXNET if n_pres>=10 ──
algo_safe <- c("GLM","RF")
if (n_pres >= 10) algo_safe <- c(algo_safe, "MAXNET")
MAXNET_EXCLUDED <- FALSE

log_msg(paste0("Modeling with: ", paste(algo_safe,collapse=", ")))
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
  if (nrow(s)>0) log_msg(sprintf("  %s: %d TSS=%.3f/%.3f/%.3f ROC=%.3f/%.3f/%.3f",
    a, nrow(s), min(s$TSS,na.rm=T), median(s$TSS,na.rm=T), max(s$TSS,na.rm=T),
    min(s$ROC,na.rm=T), median(s$ROC,na.rm=T), max(s$ROC,na.rm=T)))
}

sel <- data.frame(species=SP_NAME, tier=tier, n_selected=length(sel_models),
  models=paste(sel_models,collapse=";"), best_TSS=if(length(sel_models)>0) max(tss[c(p_idx,f_idx)],na.rm=T) else NA,
  best_ROC=if(length(sel_models)>0) max(roc[c(p_idx,f_idx)],na.rm=T) else NA,
  MAXNET=if(MAXNET_EXCLUDED) "EXCLUDED" else if("MAXNET" %in% algo_safe) "COMPLETE" else "SKIPPED_LOW_N",
  ready=length(sel_models)>0, n_folds=n_folds, n_pres=n_pres, stringsAsFactors=FALSE)
write.csv(sel, file.path(OUT_DIR, paste0("selection_", SP_USC, ".csv")), row.names=FALSE)

log_msg(sprintf("DONE. tier=%s ready=%s", tier, sel$ready))
sink(type="output"); sink(type="message"); close(log_con)
cat(paste0("REPAIR_DONE: ",SP_NAME," | tier=",tier," | n=",length(sel_models)," | ready=",sel$ready,"\n"))
