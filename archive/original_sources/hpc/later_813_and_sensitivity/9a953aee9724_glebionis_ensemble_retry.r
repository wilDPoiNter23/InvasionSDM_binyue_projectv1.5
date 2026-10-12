# Original source: glebionis_ensemble_retry.r
# Original source location: ${WORK_ROOT}/glebionis_ensemble_retry.r
# Original source SHA-256: 9a953aee9724309ef92c793b095de763fd624bdb8b4cff1328c8ac0165b5098e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Glebionis coronaria — ensemble forecasting retry only
suppressMessages({ library(biomod2); library(terra) })

BASE <- "<WORK_ROOT>"
SP_DOT <- "Glebionis.coronaria"
SP_DIR <- "Glebionis_coronaria"
setwd(BASE)

task_tmp <- file.path(BASE, "temp", paste0("glebionis_retry_", Sys.getenv("SLURM_JOB_ID")))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=80, todisk=TRUE, threads=1, progress=1)

cat("Loading model...\n")
mod <- readRDS(file.path(BASE, "results/targeted_negative_TSS_repair_multifold_v2", paste0(SP_DIR,"_models_multifold_v2.rds")))
sel <- read.csv(file.path(BASE, "audit/publication_projection_model_selection_final.csv"), stringsAsFactors=FALSE)
selected <- unique(sel$full.name[sel$species=="Glebionis coronaria" & sel$final_selected %in% TRUE])
cat("Selected:", length(selected), "models\n")

cat("Loading projection...\n")
proj_file <- file.path(BASE, SP_DOT, "proj_proj_Glebionis_coronaria_multifold_v4", "Glebionis.coronaria.proj_Glebionis_coronaria_multifold_v4.projection.out")
proj <- get(load(proj_file))

cat("Building ensemble...\n")
ens <- BIOMOD_EnsembleModeling(bm.mod=mod, models.chosen=selected, em.by="all",
  em.algo=c("EMmean","EMmedian"), metric.select=c("TSS"), metric.select.thresh=c(0.0),
  metric.eval=c("TSS","ROC"), var.import=3, EMci.alpha=0.05, EMwmean.decay="proportional")
cat("Ensemble built.\n")

cat("Ensemble forecasting (do.stack=FALSE, keep.in.memory=FALSE)...\n")
ef <- tryCatch({
  BIOMOD_EnsembleForecasting(bm.em=ens, bm.proj=proj,
    proj.name="proj_Glebionis_coronaria_multifold_v4_retry1",
    models.chosen="all", metric.binary="TSS", metric.filter="TSS",
    do.stack=FALSE, keep.in.memory=FALSE, nb.cpu=1)
}, error=function(e) { cat("ERROR:", e$message, "\n"); NULL })

if (is.null(ef)) {
  cat("FAILED\n"); quit(status=1)
}

cat("SUCCESS\n")
retry_dir <- file.path(BASE, SP_DOT, "proj_proj_Glebionis_coronaria_multifold_v4_retry1")
tifs <- list.files(retry_dir, pattern="EMmean.*\\.tif$", recursive=TRUE, full.names=TRUE)
tifs <- tifs[!grepl("_TSSbin|_TSSfilt", tifs)]
cat("EMmean:", length(tifs), "files\n")
for (f in tifs) cat("  ", f, "\n")
