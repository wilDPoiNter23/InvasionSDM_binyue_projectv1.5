# Original source: multifold_repair_v2.r
# Original source location: ${WORK_ROOT}/multifold_repair_v2.r
# Original source SHA-256: 5cc23aded1fd4b33c58d860f7eaf3365274a2d4894b04ae8325eb593c7401265
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# multifold_repair_v2.r — 26-species multifold CV repair
# 24 primary: 4-fold BIOMOD2 block CV (user.defined)
# 2 fallback: 3-fold kfold CV (Cinnamomum camphora, Croton setiger)
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: multifold_repair_v2.r <species_name>")
SP_NAME <- args[1]

STACK_P <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OUT_DIR <- "<WORK_ROOT>/results/targeted_negative_TSS_repair_multifold_v2"
AUDIT_DIR <- "<WORK_ROOT>/audit"
SEED <- 42L; MODELS <- c("GLM","RF","MAXNET")
KFOLD_SP <- c("Cinnamomum camphora","Croton setiger")
IS_KFOLD <- SP_NAME %in% KFOLD_SP
N_EXPECTED_FOLDS <- if(IS_KFOLD) 3L else 4L
N_EXPECTED_MODELS <- N_EXPECTED_FOLDS * length(MODELS)
SP_DIR <- gsub(" ","_",SP_NAME)

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_DIR,"_multifold_v2.log"))
log_con <- file(log_file, open="wt")
log_msg <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); line <- paste0("[",ts,"] ",msg); cat(line,"\n",file=log_con,append=TRUE); cat(line,"\n") }
sink(log_con,append=TRUE,split=FALSE,type="output"); sink(log_con,append=TRUE,split=FALSE,type="message")

log_msg(paste0("=== Multifold Repair v2: ",SP_NAME," ==="))
log_msg(paste0("CV type: ",if(IS_KFOLD) "K_FOLD_3_FALLBACK_V2" else "BIOMOD2_BLOCK4_PRIMARY"))
log_msg(paste0("Expected folds: ",N_EXPECTED_FOLDS," | Expected models: ",N_EXPECTED_MODELS))

# ── Load existing formatted data ──
PILOT_DIR <- "results/repair_pilot_v2"
TARGET_DIR <- "results/targeted_negative_TSS_repair_v1"
PILOT_SP <- c("Pachira aquatica","Geranium dissectum","Ailanthus altissima","Tamarix aphylla","Euphorbia terracina")
src_dir <- if(SP_NAME %in% PILOT_SP) PILOT_DIR else TARGET_DIR
fmt_file <- file.path(src_dir, paste0(SP_DIR,"_formatted_BranchP.rds"))
if (!file.exists(fmt_file)) stop("Formatted data not found: ",fmt_file)
fd <- readRDS(fmt_file)
log_msg(sprintf("Formatted data: %d rows, %d pres, %d PA",
  length(fd@data.species), sum(!is.na(fd@data.species)&fd@data.species==1), sum(is.na(fd@data.species))))

# ── Generate CV table ──
log_msg("Generating CV table...")
set.seed(SEED)
if (IS_KFOLD) {
  cv <- bm_CrossValidation(bm.format=fd, strategy="kfold", k=3, nb.rep=1, do.full.models=FALSE)
  colnames(cv) <- c("_PA1_RUN1","_PA1_RUN2","_PA1_RUN3")
  stopifnot(ncol(cv)==3)
  stopifnot(identical(colnames(cv), c("_PA1_RUN1","_PA1_RUN2","_PA1_RUN3")))
} else {
  cv <- bm_CrossValidation(bm.format=fd, strategy="block", do.full.models=FALSE)
  colnames(cv) <- c("_PA1_RUN1","_PA1_RUN2","_PA1_RUN3","_PA1_RUN4")
  stopifnot(ncol(cv)==4)
  stopifnot(identical(colnames(cv), c("_PA1_RUN1","_PA1_RUN2","_PA1_RUN3","_PA1_RUN4")))
}
log_msg(sprintf("CV table: %d x %d | colnames: %s", nrow(cv), ncol(cv), paste(colnames(cv),collapse=", ")))

# ── Validate folds ──
resp <- fd@data.species; is_pres <- !is.na(resp)&resp==1; is_PA <- is.na(resp)
fold_ok <- TRUE
for (j in 1:ncol(cv)) {
  cm <- cv[,j]==TRUE; vm <- cv[,j]==FALSE
  cp <- sum(cm&is_pres); cPA <- sum(cm&is_PA); vp <- sum(vm&is_pres); vPA <- sum(vm&is_PA)
  ok <- cp>0 && vp>0 && cPA>0 && vPA>0
  if (!ok) fold_ok <- FALSE
  log_msg(sprintf("Fold %d: cal_pres=%d cal_PA=%d | val_pres=%d val_PA=%d [%s]", j, cp, cPA, vp, vPA, if(ok)"PASS" else "FAIL"))
}
if (!fold_ok) { log_msg("FOLD VALIDATION FAILED — stopping"); quit(status=1) }

# Save CV table
saveRDS(cv, file.path(OUT_DIR, paste0(SP_DIR,"_CV_input.rds")))
write.csv(as.data.frame(cv), file.path(OUT_DIR, paste0(SP_DIR,"_CV_input.csv")), row.names=FALSE)

# ── Fit model ──
log_msg("Fitting BIOMOD_Modeling...")
model_opts <- suppressMessages(bm_ModelingOptions(data.type="binary", models=MODELS, strategy="default"))
t_start <- Sys.time()
mod <- tryCatch({
  BIOMOD_Modeling(bm.format=fd, modeling.id=paste0(SP_DIR,"_multifold_v2"),
    models=MODELS, bm.options=model_opts,
    CV.strategy="user.defined", CV.user.table=cv, CV.do.full.models=FALSE,
    OPT.strategy="bigboss", metric.eval=c("TSS","ROC"), seed.val=SEED, nb.cpu=2)
}, error=function(e){ log_msg(paste0("MODELING ERROR: ",e$message)); NULL })
t_elapsed <- difftime(Sys.time(),t_start,units="mins")
log_msg(sprintf("Modeling: %.1f min | Models: %s | Failed: %s",
  t_elapsed, paste(mod@models.computed,collapse="; "), paste(mod@models.failed,collapse="; ")))
if (is.null(mod)) quit(status=1)

saveRDS(mod, file.path(OUT_DIR, paste0(SP_DIR,"_models_multifold_v2.rds")))

# ── CV transmission verification ──
actual_cv <- get_calib_lines(mod)
expected_cols <- paste0("_PA1_RUN",1:N_EXPECTED_FOLDS)
run_cols <- grep(paste0("^_PA1_RUN[1-",N_EXPECTED_FOLDS,"]$"), colnames(actual_cv), value=TRUE)

stopifnot(all(expected_cols %in% colnames(actual_cv)))
identical_mtx <- identical(cv, actual_cv[, expected_cols, drop=FALSE])
stopifnot(identical_mtx)

# Hashes
tmp_in <- tempfile(); tmp_out <- tempfile()
write.csv(as.data.frame(cv), tmp_in, row.names=FALSE)
write.csv(as.data.frame(actual_cv[, expected_cols, drop=FALSE]), tmp_out, row.names=FALSE)
hash_in <- system(paste0("sha256sum ",tmp_in," | cut -d\" \" -f1"), intern=TRUE)
hash_out <- system(paste0("sha256sum ",tmp_out," | cut -d\" \" -f1"), intern=TRUE)
unlink(c(tmp_in, tmp_out))

log_msg(sprintf("CV transmission: identical=%s hash_match=%s", identical_mtx, hash_in==hash_out))

# ── Extract metrics ──
ev <- get_evaluations(mod)
indep <- ev[ev$PA != "allData" & ev$run != "allRun" & !is.na(ev$validation),]
log_msg(sprintf("Independent validation rows: %d", nrow(indep)))

fold_metrics <- data.frame()
for (j in 1:N_EXPECTED_FOLDS) {
  cm <- cv[,j]==TRUE; vm <- cv[,j]==FALSE
  cp <- sum(cm&is_pres); cPA <- sum(cm&is_PA); vp <- sum(vm&is_pres); vPA <- sum(vm&is_PA)
  for (algo in MODELS) {
    run_name <- paste0("RUN",j)
    av <- indep[indep$algo==algo & indep$run==run_name,]
    tss_cal <- av$calibration[av$metric.eval=="TSS"]; tss_val <- av$validation[av$metric.eval=="TSS"]
    roc_cal <- av$calibration[av$metric.eval=="ROC"]; roc_val <- av$validation[av$metric.eval=="ROC"]
    fold_metrics <- rbind(fold_metrics, data.frame(species=SP_NAME, fold=j, algo=algo,
      cal_pres=cp, cal_PA=cPA, val_pres=vp, val_PA=vPA,
      calibration_TSS=if(length(tss_cal)>0) tss_cal[1] else NA,
      validation_TSS=if(length(tss_val)>0) tss_val[1] else NA,
      calibration_ROC=if(length(roc_cal)>0) roc_cal[1] else NA,
      validation_ROC=if(length(roc_val)>0) roc_val[1] else NA,
      model_success=run_name %in% indep$run[indep$algo==algo], stringsAsFactors=FALSE))
  }
}

write.csv(fold_metrics, file.path(AUDIT_DIR, paste0(SP_DIR,"_multifold_v2_fold_metrics.csv")), row.names=FALSE)

# ── Species summary ──
tss_vals <- fold_metrics$validation_TSS[!is.na(fold_metrics$validation_TSS)]
roc_vals <- fold_metrics$validation_ROC[!is.na(fold_metrics$validation_ROC)]
fold_med_tss <- tapply(fold_metrics$validation_TSS, fold_metrics$fold, median, na.rm=TRUE)

summary <- data.frame(species=SP_NAME, pipeline_group=if(IS_KFOLD) "K_FOLD_3_FALLBACK_V2" else "BIOMOD2_BLOCK4_PRIMARY",
  n_folds=N_EXPECTED_FOLDS, n_models=length(mod@models.computed), n_failed=length(mod@models.failed),
  median_val_TSS=if(length(tss_vals)>0) median(tss_vals) else NA,
  median_val_ROC=if(length(roc_vals)>0) median(roc_vals) else NA,
  min_val_TSS=if(length(tss_vals)>0) min(tss_vals) else NA,
  q25_TSS=if(length(tss_vals)>0) quantile(tss_vals,0.25) else NA,
  q75_TSS=if(length(tss_vals)>0) quantile(tss_vals,0.75) else NA,
  prop_runs_TSS_positive=if(length(tss_vals)>0) mean(tss_vals>0) else NA,
  prop_runs_TSS_ge04=if(length(tss_vals)>0) mean(tss_vals>=0.4) else NA,
  median_fold_TSS=if(length(fold_med_tss)>0) median(fold_med_tss) else NA,
  min_fold_TSS=if(length(fold_med_tss)>0) min(fold_med_tss) else NA,
  prop_folds_TSS_pos=if(length(fold_med_tss)>0) mean(fold_med_tss>0) else NA,
  prop_folds_TSS_ge04=if(length(fold_med_tss)>0) mean(fold_med_tss>=0.4) else NA,
  cv_transmission_pass=identical_mtx, time_min=round(t_elapsed,1))
write.csv(summary, file.path(AUDIT_DIR, paste0(SP_DIR,"_multifold_v2_summary.csv")), row.names=FALSE)

cat(sprintf("\n=== %s SUMMARY ===\n",SP_NAME))
cat(sprintf("Models: %d/%d | TSS median=%.3f range=[%.3f,%.3f] | %%pos=%.0f%% | Fold TSS=%.3f\n",
  summary$n_models, N_EXPECTED_MODELS, summary$median_val_TSS, summary$min_val_TSS,
  max(tss_vals,na.rm=TRUE), summary$prop_runs_TSS_positive*100, summary$median_fold_TSS))
cat(sprintf("CV transmission: %s\n", if(identical_mtx) "PASS" else "FAIL"))

log_msg("=== DONE ===")
sink(type="output"); sink(type="message"); close(log_con)
