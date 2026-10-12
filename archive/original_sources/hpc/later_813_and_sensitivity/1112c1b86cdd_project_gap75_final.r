# Original source: project_gap75_final.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/project_gap75_final.r
# Original source SHA-256: 1112c1b86cdd5139d72172889f7c812d06e0615dd958ca50c3f61ca64a8e721e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# project_gap75_final.r — BIOMOD_Projection with symlink path fix
# Usage: Rscript project_gap75_final.r <species_name> <stack_path> <group_label> <output_dir>
# Uses: setwd(models/{spd}/) to resolve BIOMOD path conventions

suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 3) stop("Usage: project_gap75_final.r <species_name> <stack_path> <group_label> [output_dir]")
SP_NAME    <- args[1]
STACK_PATH <- args[2]
GROUP_LABEL <- args[3]
OUT_DIR    <- if (length(args) >= 4) args[4] else file.path("<WORK_ROOT>/results", paste0("gap75_publication_", GROUP_LABEL))

BASE    <- "<WORK_ROOT>"
SP_DOT  <- gsub(" ", ".", SP_NAME)
SP_USC  <- gsub(" ", "_", SP_NAME)

# Resolve stack path to absolute (before setwd changes directory)
if (!grepl("^/", STACK_PATH)) {
  STACK_PATH <- file.path(BASE, STACK_PATH)
}
STACK_PATH <- normalizePath(STACK_PATH, mustWork=TRUE)

# Resolve output dir to absolute
if (!grepl("^/", OUT_DIR)) {
  OUT_DIR <- file.path(BASE, OUT_DIR)
}
OUT_DIR <- normalizePath(OUT_DIR, mustWork=FALSE)

# ── Setup ──
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
log_file <- file.path(OUT_DIR, paste0(SP_USC, "_projection.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n", file=log_con, append=TRUE)
  cat(line, "\n")
}

sink(log_con, append=TRUE, split=FALSE, type="output")
sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Gap75 Projection: ", SP_NAME, " ==="))
log_msg(paste0("Stack: ", STACK_PATH))
log_msg(paste0("Group: ", GROUP_LABEL))
log_msg(paste0("Output: ", OUT_DIR))

# ── Disk-backed settings ──
task_tmp <- file.path(BASE, "temp", paste0("proj_gap75_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# ── Enter species model directory ──
model_sp_dir <- file.path(BASE, "models", SP_DOT)
if (!dir.exists(model_sp_dir)) stop("Model directory not found: ", model_sp_dir)
setwd(model_sp_dir)
log_msg(paste0("Working dir: ", getwd()))

# ── Load .models.out ──
mf <- list.files(".", pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
if (length(mf) == 0) stop("No .models.out found")
m <- get(load(mf[1]))
log_msg(paste0("Model loaded: ", mf[1]))

mod_preds <- m@expl.var.names
log_msg(paste0("Predictors (", length(mod_preds), "): ", paste(mod_preds, collapse=", ")))

# ── Model selection (frozen thresholds) ──
ev <- m@models.evaluation@val
mod_names <- unique(ev$full.name)

all_tss <- c(); all_roc <- c(); all_mn <- c()
for (mn in mod_names) {
  sub <- ev[ev$full.name == mn,]
  tss_v <- suppressWarnings(max(sub$validation[sub$metric.eval == "TSS"], na.rm=TRUE))
  roc_v <- suppressWarnings(max(sub$validation[sub$metric.eval == "ROC"], na.rm=TRUE))
  if (is.finite(tss_v) && is.finite(roc_v)) {
    all_tss <- c(all_tss, tss_v)
    all_roc <- c(all_roc, roc_v)
    all_mn  <- c(all_mn, mn)
  }
}

primary  <- which(all_tss > 0.8 & all_roc > 0.8)
fallback <- which(all_tss > 0.7 & all_roc > 0.7)
if (length(primary) > 0) {
  selected <- all_mn[primary]
  tier <- "PRIMARY"
} else if (length(fallback) > 0) {
  selected <- all_mn[fallback]
  tier <- "FALLBACK"
} else {
  stop("No qualified models (TSS>0.7 & ROC>0.7)")
}

log_msg(sprintf("Tier: %s | Models: %d | TSS: %.3f-%.3f | ROC: %.3f-%.3f",
  tier, length(selected),
  min(all_tss[primary %||% fallback]), max(all_tss[primary %||% fallback]),
  min(all_roc[primary %||% fallback]), max(all_roc[primary %||% fallback])))

# ── Subset stack ──
log_msg("Loading predictor stack...")
full_stack <- rast(STACK_PATH)
missing <- setdiff(mod_preds, names(full_stack))
if (length(missing) > 0) stop("Missing predictors: ", paste(missing, collapse=", "))
stack <- full_stack[[mod_preds]]
log_msg(sprintf("Stack subset: %d layers", nlyr(stack)))

# ── Project ──
# BIOMOD prepends "proj_" to proj.name, so we use just "gap75_{species}"
PROJ_NAME <- paste0("gap75_", SP_USC)
log_msg(paste0("Projecting ", length(selected), " models..."))

rc <- system.time({
  proj <- tryCatch({
    BIOMOD_Projection(
      bm.mod = m,
      new.env = stack,
      proj.name = PROJ_NAME,
      models.chosen = selected,
      binary.meth = NULL,
      clamping.mask = TRUE,
      build.clamping.mask = TRUE,
      output.format = ".tif",
      compress = TRUE,
      do.stack = FALSE,
      keep.in.memory = FALSE,
      nb.cpu = 1,
      overwrite = TRUE
    )
  }, error = function(e) {
    log_msg(paste0("PROJECTION ERROR: ", conditionMessage(e)))
    return(NULL)
  })
})

log_msg(sprintf("Projection time: %.1f min", rc[3]/60))

if (is.null(proj)) stop("Projection failed")

# ── Locate output rasters ──
# BIOMOD creates: {sp.name}/proj_{proj.name}/individual_projections/
proj_out_dir <- file.path(getwd(), m@sp.name, paste0("proj_", PROJ_NAME), "individual_projections")
log_msg(paste0("Looking for rasters in: ", proj_out_dir))

indiv_rasters <- if (dir.exists(proj_out_dir)) {
  list.files(proj_out_dir, pattern="\\.tif$", full.names=TRUE)
} else {
  character(0)
}

log_msg(sprintf("Found %d projection rasters", length(indiv_rasters)))

# ── Manual EMmean ──
FINAL_RASTER <- file.path(OUT_DIR, paste0(SP_USC, "_ensemble_EMmean.tif"))
PRODUCT_TYPE <- if (length(indiv_rasters) >= 2) "ENSEMBLE_EMMEAN_MANUAL" else "SINGLE_QUALIFIED_MODEL"

if (length(indiv_rasters) >= 2) {
  log_msg("Computing manual EMmean...")
  rast_list <- lapply(indiv_rasters, function(f) {
    r <- tryCatch(rast(f), error=function(e) NULL)
    if (!is.null(r)) r[[1]] else NULL
  })
  rast_list <- rast_list[!sapply(rast_list, is.null)]
  
  if (length(rast_list) >= 2) {
    rast_stack <- rast(rast_list)
    emmean <- mean(rast_stack, na.rm=TRUE)
    writeRaster(emmean, FINAL_RASTER, overwrite=TRUE,
      gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
    log_msg(sprintf("EMmean written: %s", FINAL_RASTER))
  } else {
    # Fallback to first available
    writeRaster(rast_list[[1]], FINAL_RASTER, overwrite=TRUE,
      gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
    PRODUCT_TYPE <- "SINGLE_QUALIFIED_MODEL_FALLBACK"
  }
} else if (length(indiv_rasters) == 1) {
  r <- rast(indiv_rasters[1])
  writeRaster(r[[1]], FINAL_RASTER, overwrite=TRUE,
    gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
  log_msg(sprintf("Single model: %s", FINAL_RASTER))
} else {
  stop("No projection rasters found!")
}

# ── QC ──
r <- rast(FINAL_RASTER)
vals <- values(r, na.rm=TRUE)
qc <- list(
  ncell = ncell(r),
  nna   = length(vals),
  min   = min(vals, na.rm=TRUE),
  max   = max(vals, na.rm=TRUE),
  mean  = mean(vals, na.rm=TRUE),
  neg   = sum(vals < 0, na.rm=TRUE),
  inf   = sum(is.infinite(vals))
)
QC_PASS <- qc$nna > 0 && qc$max > qc$min && qc$inf == 0

log_msg(sprintf("QC: cells=%d nna=%d min=%.4f max=%.4f mean=%.4f neg=%d inf=%d PASS=%s",
  qc$ncell, qc$nna, qc$min, qc$max, qc$mean, qc$neg, qc$inf, QC_PASS))

# ── Manifest entry ──
write.csv(data.frame(
  species = SP_NAME,
  tier = tier,
  n_models = length(selected),
  models = paste(selected, collapse=";"),
  product_type = PRODUCT_TYPE,
  final_raster = FINAL_RASTER,
  stack = STACK_PATH,
  n_predictors = length(mod_preds),
  qc_pass = QC_PASS,
  qc_nna = qc$nna,
  qc_min = qc$min,
  qc_max = qc$max,
  qc_mean = qc$mean,
  time_min = round(rc[3]/60, 1),
  timestamp = Sys.time(),
  stringsAsFactors = FALSE
), file.path(OUT_DIR, paste0("manifest_", SP_USC, ".csv")), row.names=FALSE)

# ── Cleanup ──
unlink(task_tmp, recursive=TRUE, force=TRUE)

log_msg(paste0("DONE. Status: ", if(QC_PASS) "SUCCESS" else "QC_FAIL"))

sink(type="output"); sink(type="message")
close(log_con)

cat(paste0("SUCCESS: ", SP_NAME, " | ", PRODUCT_TYPE, " | ", FINAL_RASTER, "\n"))
