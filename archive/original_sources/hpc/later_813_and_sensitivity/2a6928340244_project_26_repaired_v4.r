# Original source: project_26_repaired_v4.r
# Original source location: ${WORK_ROOT}/project_26_repaired_v4.r
# Original source SHA-256: 2a6928340244c53ea9e7e1aeec8a6fb049a019c1a46144f3cc53d8ae1e732cc5
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# project_26_repaired_v4.r — Disk-backed ensemble projection v4
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: project_26_repaired_v4.r <species_name>")
SP_NAME <- args[1]

STACK_P <- "<WORK_ROOT>/data/predictors/full_stack_terrestrial_primary_v1.tif"
MODEL_DIR <- "<WORK_ROOT>/results/targeted_negative_TSS_repair_multifold_v2"
OUT_DIR <- "<WORK_ROOT>/results/targeted_negative_TSS_projection_multifold_v4"
AUDIT_DIR <- "<WORK_ROOT>/audit"
SELECTION_CSV <- file.path(AUDIT_DIR, "publication_projection_model_selection_final.csv")
MAXNET_EXCLUDE <- c("Dracaena trifasciata","Harrisia tortuosa","Melinis minutiflora","Pachira aquatica","Pluchea carolinensis","Urochloa dictyoneura","Vicia eriocarpa")
SP_DIR <- gsub(" ","_",SP_NAME)
SEED <- 42L

# ── Disk-backed terra settings ──
task_tmp <- file.path("<WORK_ROOT>/temp",
  paste0("projection_v4_", Sys.getenv("SLURM_JOB_ID"), "_", Sys.getenv("SLURM_ARRAY_TASK_ID")))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=20, todisk=TRUE, threads=1, progress=1)

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
log_file <- file.path(OUT_DIR, paste0(SP_DIR,"_projection_v4.log"))
log_con <- file(log_file, open="wt")
log_msg <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); line <- paste0("[",ts,"] ",msg); cat(line,"\n",file=log_con,append=TRUE); cat(line,"\n") }
sink(log_con,append=TRUE,split=FALSE,type="output"); sink(log_con,append=TRUE,split=FALSE,type="message")

log_msg(paste0("=== v4 Projection: ",SP_NAME," ==="))
log_msg(paste0("TMPDIR: ", task_tmp))

# ── Read frozen selection ──
selection <- read.csv(SELECTION_CSV, stringsAsFactors=FALSE)
selected_models <- unique(selection$full.name[selection$species==SP_NAME & selection$final_selected %in% TRUE])
if (length(selected_models)==0) stop("No frozen qualified model for: ", SP_NAME)

log_msg(sprintf("Selected models: %d", length(selected_models)))
for (m in selected_models) log_msg(sprintf("  %s", m))

# ── Load model, verify ──
mod <- readRDS(file.path(MODEL_DIR, paste0(SP_DIR,"_models_multifold_v2.rds")))
built <- get_built_models(mod)
if (!all(selected_models %in% built)) stop("Selected model not in built_models: ", paste(setdiff(selected_models,built),collapse="; "))

# Forbidden MAXNET
if (SP_NAME %in% MAXNET_EXCLUDE && any(grepl("MAXNET", selected_models))) stop("Forbidden MAXNET for: ", SP_NAME)

PRODUCT_TYPE <- if(length(selected_models)>=2) "ENSEMBLE_EMMEAN_EMMEDIAN" else "SINGLE_QUALIFIED_MODEL"
log_msg(sprintf("Product: %s (%d models)", PRODUCT_TYPE, length(selected_models)))

# ── Build ensemble if multi-model ──
if (PRODUCT_TYPE == "ENSEMBLE_EMMEAN_EMMEDIAN") {
  log_msg("Building ensemble...")
  ens <- tryCatch({
    BIOMOD_EnsembleModeling(bm.mod=mod, models.chosen=selected_models, em.by="all",
      em.algo=c("EMmean","EMmedian"), metric.select=c("TSS"), metric.select.thresh=c(0.0),
      metric.eval=c("TSS","ROC"), var.import=3, EMci.alpha=0.05, EMwmean.decay="proportional")
  }, error=function(e){ log_msg(paste0("Ensemble ERROR: ",e$message)); NULL })
  if (is.null(ens)) stop("Ensemble build failed")
  saveRDS(ens, file.path(OUT_DIR, paste0(SP_DIR,"_ensemble.rds")))
  log_msg("Ensemble saved")
}

# ── Project ──
log_msg("Loading predictor stack...")
stack <- rast(STACK_P)
log_msg(sprintf("Stack: %d layers, %d x %d", nlyr(stack), nrow(stack), ncol(stack)))

log_msg("Projecting selected models...")
proj <- tryCatch({
  BIOMOD_Projection(bm.mod=mod, new.env=stack, proj.name=paste0("proj_",SP_DIR,"_multifold_v4"),
    models.chosen=selected_models, binary.meth=NULL, clamping.mask=TRUE,
    build.clamping.mask=TRUE, output.format=".tif", compress=TRUE,
    do.stack=FALSE, keep.in.memory=FALSE, nb.cpu=1, overwrite=FALSE)
}, error=function(e){ log_msg(paste0("Projection ERROR: ",e$message)); NULL })
if (is.null(proj)) stop("Projection failed")
log_msg("Projection done")

# ── Ensemble forecast (if applicable) ──
if (PRODUCT_TYPE == "ENSEMBLE_EMMEAN_EMMEDIAN") {
  log_msg("Ensemble forecasting...")
  ef <- tryCatch({
    BIOMOD_EnsembleForecasting(bm.em=ens, bm.proj=proj,
      proj.name=paste0("proj_",SP_DIR,"_multifold_v4"),
      models.chosen="all", metric.binary="TSS", metric.filter="TSS")
  }, error=function(e){ log_msg(paste0("EnsembleForecast ERROR: ",e$message)); NULL })
  if (is.null(ef)) log_msg("Forecast failed — individual projections available")
  else log_msg("Forecast done")
}

# ── Quick QC ──
proj_dir <- file.path(getwd(), SP_NAME, paste0("proj_",SP_DIR,"_multifold_v4"))
if (dir.exists(proj_dir)) {
  rasters <- list.files(proj_dir, pattern="\\.tif$", recursive=TRUE, full.names=TRUE)
  if (length(rasters)>0) {
    r <- rast(rasters[1])
    nc <- ncell(r); nna <- sum(!is.na(values(r)))
    rng <- range(values(r),na.rm=TRUE)
    log_msg(sprintf("QC: cells=%d non-NA=%d range=[%.4f,%.4f]", nc, nna, rng[1], rng[2]))
  } else { log_msg("QC: no raster found") }
} else { log_msg("QC: projection dir not found") }

log_msg(sprintf("=== DONE: %s (%s) ===", SP_NAME, PRODUCT_TYPE))
sink(type="output"); sink(type="message"); close(log_con)
