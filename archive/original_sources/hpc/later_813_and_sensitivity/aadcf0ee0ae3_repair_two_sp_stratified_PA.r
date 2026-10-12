# Original source: repair_two_sp_stratified_PA.r
# Original source location: ${WORK_ROOT}/repair_two_sp_stratified_PA.r
# Original source SHA-256: aadcf0ee0ae33ef29f5e19ac6b4c9ec4dfca6063ec8f8bb38f621db23df79b17
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# repair_two_sp_stratified_PA.r — Spatially-stratified PA for Cinnamomum camphora & Croton setiger
# Fixes val_PA=0 in block CV by sampling PAs evenly across the 3 spatial fold regions
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: repair_two_sp_stratified_PA.r <species_name>")
SP_NAME <- args[1]

STACK_P  <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
OCC_BASE <- "<WORK_ROOT>/data/species_occ"
OUT_DIR  <- "<WORK_ROOT>/results/targeted_negative_TSS_repair_v1"
AUDIT_DIR <- "<WORK_ROOT>/audit"

SEED   <- 42L; PA_TOTAL <- 1000L; CV_K <- 3L; CV_REP <- 1L; CV_PERC <- 0.8
MODELS <- c("GLM", "RF", "MAXNET")
SP_DIR <- gsub(" ", "_", SP_NAME)

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(file.path(OUT_DIR,"logs"), recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_DIR, "_stratified_PA.log"))
log_con  <- file(log_file, open="wt")
log_msg <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); line <- paste0("[",ts,"] ",msg); cat(line,"\n",file=log_con,append=TRUE); cat(line,"\n") }
sink(log_con, append=TRUE, split=FALSE, type="output")
sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Spatially Stratified PA Repair: ", SP_NAME, " ==="))

# Load occurrences & deduplicate
stack <- rast(STACK_P)
bio1 <- stack[["wc2.1_2.5m_bio_1"]]
occ_csv <- list.files(file.path(OCC_BASE, SP_DIR), pattern="occurrences\\.csv$", full.names=TRUE, ignore.case=TRUE)
occ <- read.csv(occ_csv[1], stringsAsFactors=FALSE)
lat <- occ[["decimalLatitude"]]; lon <- occ[["decimalLongitude"]]
valid <- !is.na(lat)&!is.na(lon)&lat>=-90&lat<=90&lon>=-180&lon<=180
xy <- cbind(lon[valid], lat[valid])
is_land <- !is.na(terra::extract(bio1, xy)[,1])
xy_land <- xy[is_land,,drop=FALSE]
cells <- terra::cellFromXY(bio1, xy_land)
cell_xy <- terra::xyFromCell(bio1, as.integer(names(table(cells))))
ext <- terra::extract(stack, cell_xy)
cc <- complete.cases(ext[,-1,drop=FALSE])
pres_xy <- cell_xy[cc,,drop=FALSE]
n_pres <- nrow(pres_xy)
log_msg(sprintf("Presences after CC: %d", n_pres))
resp_var <- rep(1L, n_pres)

# ── Phase 1: Generate spatial fold boundaries (lightweight, no PAs needed) ──
# Use a minimal random PA set just to get bm_CrossValidation folds
log_msg("Phase 1: Generating spatial fold boundaries...")
fd_phase1 <- tryCatch({
  set.seed(SEED)
  BIOMOD_FormatingData(resp.var=resp_var, expl.var=stack, resp.xy=pres_xy,
    resp.name=SP_NAME, PA.nb.rep=1, PA.nb.absences=PA_TOTAL,
    PA.strategy="random", na.rm=TRUE)
}, error=function(e){ log_msg(paste0("Phase1 format ERROR:",e$message)); NULL })
if (is.null(fd_phase1)) quit(status=1)

set.seed(SEED)
cv_phase1 <- bm_CrossValidation(bm.format=fd_phase1, strategy="block", k=CV_K, nb.rep=CV_REP, perc=CV_PERC, do.full.models=TRUE)
log_msg(sprintf("CV table: %d x %d", nrow(cv_phase1), ncol(cv_phase1)))

# Get ALL coordinates (pres + PAs) and their fold assignments
all_coords <- fd_phase1@coord
is_PA_phase1 <- is.na(fd_phase1@data.species)

# Phase 2: Determine spatial fold regions from pres+PA coordinates
log_msg("Phase 2: Determining fold spatial regions...")
fold_bbox <- list()
for (j in 1:CV_K) {
  fold_mask <- cv_phase1[, j]
  fold_coords <- all_coords[fold_mask, , drop=FALSE]
  fold_bbox[[j]] <- list(
    xmin=min(fold_coords[,1])-0.05, xmax=max(fold_coords[,1])+0.05,
    ymin=min(fold_coords[,2])-0.05, ymax=max(fold_coords[,2])+0.05)
  log_msg(sprintf("  Fold %d bbox: lon=[%.1f,%.1f] lat=[%.1f,%.1f]",
    j, fold_bbox[[j]]$xmin, fold_bbox[[j]]$xmax, fold_bbox[[j]]$ymin, fold_bbox[[j]]$ymax))
}

# Phase 3: Sample ~333 PAs within each fold's spatial extent
log_msg("Phase 3: Sampling stratified PAs...")
PA_PER_FOLD <- floor(PA_TOTAL / CV_K)
extra <- PA_TOTAL - PA_PER_FOLD * CV_K

all_pa_cells <- integer(0)
pres_cells <- terra::cellFromXY(bio1, pres_xy)

for (j in 1:CV_K) {
  n_target <- PA_PER_FOLD + if(j <= extra) 1L else 0L
  bb <- fold_bbox[[j]]
  fold_pa_cells <- integer(0)
  
  for (attempt in 1:100) {
    set.seed(SEED + j*10000 + attempt)
    rand_lon <- runif(n_target * 10, bb$xmin, bb$xmax)
    rand_lat <- runif(n_target * 10, bb$ymin, bb$ymax)
    land_idx <- which(!is.na(terra::extract(bio1, cbind(rand_lon,rand_lat))[,1]))
    if (length(land_idx)==0) next
    land_cells <- terra::cellFromXY(bio1, cbind(rand_lon[land_idx], rand_lat[land_idx]))
    land_cells <- unique(setdiff(land_cells, c(pres_cells, all_pa_cells, fold_pa_cells)))
    land_cells <- land_cells[!is.na(land_cells)]
    need <- n_target - length(fold_pa_cells)
    if (length(land_cells) >= need) {
      fold_pa_cells <- c(fold_pa_cells, sample(land_cells, need))
    } else if (length(land_cells) > 0) {
      fold_pa_cells <- c(fold_pa_cells, land_cells)
    }
    if (length(fold_pa_cells) >= n_target) break
  }
  log_msg(sprintf("  Fold %d: %d PAs (target %d)", j, length(fold_pa_cells), n_target))
  all_pa_cells <- c(all_pa_cells, fold_pa_cells)
}

pa_xy <- terra::xyFromCell(bio1, all_pa_cells)
log_msg(sprintf("Total stratified PAs: %d", nrow(pa_xy)))

# Phase 4: Build final formatted data with stratified PAs
log_msg("Phase 4: Building formatted data with stratified PAs...")
fd <- tryCatch({
  set.seed(SEED)
  BIOMOD_FormatingData(resp.var=resp_var, expl.var=stack, resp.xy=pres_xy,
    resp.name=SP_NAME, PA.nb.rep=1, PA.nb.absences=nrow(pa_xy),
    PA.strategy="user", PA.user.table=as.data.frame(pa_xy), na.rm=TRUE)
}, error=function(e){ log_msg(paste0("Format ERROR:",e$message)); NULL })
if (is.null(fd)) quit(status=1)

fd_rds <- file.path(OUT_DIR, paste0(SP_DIR, "_formatted_BranchP.rds"))
saveRDS(fd, fd_rds); log_msg(paste0("Formatted: ", basename(fd_rds)))

# Phase 5: Generate CV folds and validate
log_msg("Phase 5: CV generation and validation...")
set.seed(SEED)
cv <- bm_CrossValidation(bm.format=fd, strategy="block", k=CV_K, nb.rep=CV_REP, perc=CV_PERC, do.full.models=TRUE)

resp <- fd@data.species
is_pres <- !is.na(resp) & resp==1; is_PA <- is.na(resp)
cv_ok <- TRUE; cv_pf <- data.frame()
for (j in 1:CV_K) {
  cm <- cv[,j]; vm <- !cv[,j]
  cp<-sum(cm&is_pres); cPA<-sum(cm&is_PA); vp<-sum(vm&is_pres); vPA<-sum(vm&is_PA)
  ok <- cp>0 && vp>0 && cPA>0 && vPA>0
  if(!ok) cv_ok <- FALSE
  cv_pf <- rbind(cv_pf, data.frame(branch="BranchP",fold=j,cal_pres=cp,cal_PA=cPA,val_pres=vp,val_PA=vPA,ok=ok))
}
write.csv(cv_pf, file.path(AUDIT_DIR, paste0(SP_DIR,"_cv_preflight_BranchP.csv")), row.names=FALSE)
log_msg(paste0("All folds valid: ", cv_ok))
cat("\n--- CV Preflight ---\n"); print(cv_pf, row.names=FALSE)
if (!cv_ok) { log_msg("CV PREFLIGHT FAILED — stopping"); quit(status=1) }

# Phase 6: Modeling
log_msg("Phase 6: BIOMOD_Modeling...")
model_opts <- suppressMessages(bm_ModelingOptions(data.type="binary", models=MODELS, strategy="default"))
t_start <- Sys.time()
ind_models <- tryCatch({
  BIOMOD_Modeling(bm.format=fd, modeling.id=paste0(SP_DIR,"_BranchP"),
    models=MODELS, bm.options=model_opts, CV.user.table=cv, CV.perc=CV_PERC,
    OPT.strategy="bigboss", metric.eval=c("TSS","ROC"), do.full.models=TRUE,
    seed.val=SEED, nb.cpu=2)
}, error=function(e){ log_msg(paste0("MODELING ERROR:",e$message)); NULL })
t_elapsed <- difftime(Sys.time(), t_start, units="mins")
log_msg(sprintf("Modeling: %.1f min", t_elapsed))

if (is.null(ind_models)) quit(status=1)

mod_rds <- file.path(OUT_DIR, paste0(SP_DIR, "_models_BranchP.rds"))
saveRDS(ind_models, mod_rds)

# Phase 7: Evaluation
evals <- get_evaluations(ind_models)
tss_sub <- evals[evals$metric.eval=="TSS" & !is.na(evals$validation),]
roc_sub <- evals[evals$metric.eval=="ROC" & !is.na(evals$validation),]
tss_best <- if(nrow(tss_sub)>0) max(tss_sub$validation,na.rm=TRUE) else NA
roc_best <- if(nrow(roc_sub)>0) max(roc_sub$validation,na.rm=TRUE) else NA
tss_med  <- if(nrow(tss_sub)>0) median(tss_sub$validation,na.rm=TRUE) else NA
log_msg(sprintf("RESULTS: best_TSS=%.3f median_TSS=%.3f best_ROC=%.3f", tss_best, tss_med, roc_best))

comp <- data.frame(species=SP_NAME, prior_TSS="see_queue",
  BranchP_maxTSS=tss_best, BranchP_maxROC=roc_best, BranchP_medianTSS=tss_med,
  BranchP_status="modeled", all_CV_folds_ok=cv_ok, n_pres=n_pres, n_PA=nrow(pa_xy),
  PA_strategy="spatially_stratified_v1", stringsAsFactors=FALSE)
write.csv(comp, file.path(OUT_DIR, paste0(SP_DIR,"_comparison.csv")), row.names=FALSE)

log_msg("=== DONE ==="); sink(type="output"); sink(type="message"); close(log_con)
