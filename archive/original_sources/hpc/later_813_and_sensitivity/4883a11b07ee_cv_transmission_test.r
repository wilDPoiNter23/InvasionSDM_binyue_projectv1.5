# Original source: cv_transmission_test.r
# Original source location: ${WORK_ROOT}/cv_transmission_test.r
# Original source SHA-256: 4883a11b07ee1b376b07348385f877018b9b6d435d59d0d7cc2ac4bd5e72a637
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# cv_transmission_test.r — One-species block4 CV transmission test (Tamarix aphylla)
suppressMessages({ library(biomod2); library(terra) })

SP_NAME <- "Tamarix aphylla"
STACK_P <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OUT_DIR <- "<WORK_ROOT>/results/CV_transmission_test_block4"
AUDIT_DIR <- "<WORK_ROOT>/audit"
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
SEED <- 42L

log_file <- file.path(OUT_DIR, "Tamarix_aphylla_block4_test.log")
log_con <- file(log_file, open="wt")
log_msg <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); line <- paste0("[",ts,"] ",msg); cat(line,"\n",file=log_con,append=TRUE); cat(line,"\n") }
sink(log_con,append=TRUE,split=FALSE,type="output"); sink(log_con,append=TRUE,split=FALSE,type="message")

log_msg("=== Block4 CV Transmission Test: Tamarix aphylla ===")

# Load formatted data
fd <- readRDS("results/repair_pilot_v2/Tamarix_aphylla_formatted_BranchP.rds")
log_msg(sprintf("Formatted data: %d rows, %d pres, %d PA",
  length(fd@data.species), sum(!is.na(fd@data.species)&fd@data.species==1), sum(is.na(fd@data.species))))

# Load pre-built CV table
cv_input <- readRDS("audit/Tamarix_aphylla_block4_CV_input.rds")
stopifnot(ncol(cv_input)==4)
stopifnot(identical(colnames(cv_input),c("_PA1_RUN1","_PA1_RUN2","_PA1_RUN3","_PA1_RUN4")))
log_msg(sprintf("CV input: %d x %d", nrow(cv_input), ncol(cv_input)))

# Fit model
log_msg("Fitting BIOMOD_Modeling with CV.user.table...")
model_opts <- suppressMessages(bm_ModelingOptions(data.type="binary", models=c("GLM","RF","MAXNET"), strategy="default"))

t_start <- Sys.time()
mod <- tryCatch({
  BIOMOD_Modeling(
    bm.format = fd, modeling.id = "Tamarix.aphylla_block4",
    models = c("GLM","RF","MAXNET"), bm.options = model_opts,
    CV.strategy = "user.defined", CV.user.table = cv_input,
    CV.do.full.models = FALSE, OPT.strategy = "bigboss",
    metric.eval = c("TSS","ROC"), seed.val = SEED, nb.cpu = 2)
}, error=function(e){ log_msg(paste0("MODELING ERROR: ",e$message)); NULL })
t_elapsed <- difftime(Sys.time(),t_start,units="mins")
log_msg(sprintf("Modeling: %.1f min", t_elapsed))
if (is.null(mod)) quit(status=1)

saveRDS(mod, file.path(OUT_DIR,"Tamarix_aphylla_models_block4.rds"))
log_msg("Model saved.")

# ── Step 4: Verify CV transmission ──
actual_cv <- get_calib_lines(mod)
log_msg(sprintf("Stored calib.lines: %d x %d, colnames=%s",
  nrow(actual_cv), ncol(actual_cv), paste(colnames(actual_cv),collapse="; ")))

# Identify _PA1_RUNx columns
run_cols <- grep("^_PA1_RUN[0-9]+$", colnames(actual_cv), value=TRUE)
log_msg(sprintf("RUN columns in stored calib: %s", paste(run_cols, collapse=", ")))

# Compare input vs stored
input_cols <- colnames(cv_input)
found_cols <- input_cols %in% colnames(actual_cv)
identical_mtx <- FALSE
if (all(found_cols)) {
  identical_mtx <- identical(cv_input, actual_cv[, input_cols, drop=FALSE])
}

# Serialize for hashing
tmp_in <- tempfile(); tmp_out <- tempfile()
write.csv(as.data.frame(cv_input), tmp_in, row.names=FALSE)
write.csv(as.data.frame(actual_cv[, input_cols, drop=FALSE]), tmp_out, row.names=FALSE)
hash_in <- system(paste0("sha256sum ",tmp_in," | cut -d\" \" -f1"), intern=TRUE)
hash_out <- system(paste0("sha256sum ",tmp_out," | cut -d\" \" -f1"), intern=TRUE)
unlink(c(tmp_in, tmp_out))

decision <- if(identical_mtx) "BLOCK4_CV_TRANSMISSION_PASS" else "BLOCK4_CV_TRANSMISSION_FAIL"

verif <- data.frame(
  input_CV_rows = nrow(cv_input), input_CV_cols = ncol(cv_input),
  model_CV_rows = nrow(actual_cv), model_CV_cols = ncol(actual_cv),
  expected_RUN_columns = paste(input_cols, collapse="; "),
  observed_RUN_columns = paste(run_cols, collapse="; "),
  matrix_identical_yes_no = identical_mtx,
  canonical_input_hash = hash_in, canonical_model_hash = hash_out,
  verification_decision = decision,
  stringsAsFactors = FALSE)
write.csv(verif, file.path(AUDIT_DIR,"Tamarix_aphylla_block4_CV_transmission_verification.csv"), row.names=FALSE)

log_msg(sprintf("CV transmission: %s", decision))
log_msg(sprintf("Matrix identical: %s | Input hash: %s | Model hash: %s", identical_mtx, hash_in, hash_out))

# ── Step 5: Per-fold evaluation ──
ev <- get_evaluations(mod)
indep <- ev[ev$PA != "allData" & ev$run != "allRun" & !is.na(ev$validation),]
log_msg(sprintf("Independent validation rows: %d", nrow(indep)))

fold_metrics <- data.frame()
resp <- fd@data.species; is_pres <- !is.na(resp)&resp==1; is_PA <- is.na(resp)

for (j in 1:4) {
  cm <- cv_input[,j]==TRUE; vm <- cv_input[,j]==FALSE
  cp <- sum(cm&is_pres); cPA <- sum(cm&is_PA); vp <- sum(vm&is_pres); vPA <- sum(vm&is_PA)
  
  for (algo in unique(indep$algo)) {
    av <- indep[indep$algo==algo & indep$run==paste0("RUN",j),]
    tss_val <- av$validation[av$metric.eval=="TSS"]
    roc_val <- av$validation[av$metric.eval=="ROC"]
    tss_cal <- av$calibration[av$metric.eval=="TSS"]
    roc_cal <- av$calibration[av$metric.eval=="ROC"]
    
    fold_metrics <- rbind(fold_metrics, data.frame(
      fold=j, algo=algo,
      cal_pres=cp, cal_PA=cPA, val_pres=vp, val_PA=vPA,
      calibration_TSS=if(length(tss_cal)>0) tss_cal[1] else NA,
      validation_TSS=if(length(tss_val)>0) tss_val[1] else NA,
      calibration_ROC=if(length(roc_cal)>0) roc_cal[1] else NA,
      validation_ROC=if(length(roc_val)>0) roc_val[1] else NA,
      stringsAsFactors=FALSE))
  }
}

write.csv(fold_metrics, file.path(AUDIT_DIR,"Tamarix_aphylla_block4_fold_metrics.csv"), row.names=FALSE)

log_msg(sprintf("Fold metrics: %d rows", nrow(fold_metrics)))
cat("\n=== Per-fold metrics ===\n"); print(fold_metrics, row.names=FALSE)

# Final report
cat(sprintf("\nTransmission: %s\n", decision))
cat(sprintf("Models computed: %s\n", paste(mod@models.computed,collapse="; ")))
cat(sprintf("Models failed: %s\n", paste(mod@models.failed,collapse="; ")))

log_msg("=== DONE ==="); sink(type="output"); sink(type="message"); close(log_con)
