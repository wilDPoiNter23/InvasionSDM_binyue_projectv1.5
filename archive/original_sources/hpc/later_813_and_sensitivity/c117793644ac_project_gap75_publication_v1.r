# Original source: project_gap75_publication_v1.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/project_gap75_publication_v1.r
# Original source SHA-256: c117793644ac5d05e04320b324a89a7f254b455dc57c96659857d1093dacef72
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# project_gap75_publication_v1.r — Disk-backed projection for gap75 species
# Usage: Rscript project_gap75_publication_v1.r <species_name> <stack_path> <group_label>
# Frozen protocol: do.stack=FALSE, keep.in.memory=FALSE, nb.cpu=1
# Manual EMmean for ≥2 qualified models

suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) stop("Usage: project_gap75_publication_v1.r <species_name> <stack_path> [group_label]")
SP_NAME <- args[1]
STACK_PATH <- args[2]
GROUP_LABEL <- ifelse(length(args) >= 3, args[3], "gap75")

BASE <- "<WORK_ROOT>"
MODEL_DIR <- file.path(BASE, "models")
OUT_DIR <- file.path(BASE, "results", paste0("gap75_publication_projection_", GROUP_LABEL))
AUDIT_DIR <- file.path(BASE, "publication_release_v1/audits")

SP_DOT <- gsub(" ", ".", SP_NAME)
SP_USCORE <- gsub(" ", "_", SP_NAME)

# ── Disk-backed settings ──
task_tmp <- file.path(BASE, "temp", paste0("proj_gap75_", SP_USCORE, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
dir.create(AUDIT_DIR, recursive=TRUE, showWarnings=FALSE)

# ── Logging ──
log_file <- file.path(OUT_DIR, paste0(SP_USCORE, "_projection.log"))
log_con <- file(log_file, open="wt")
log_msg <- function(msg) { 
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
log_msg(paste0("TMP: ", task_tmp))

# ── Load model ──
model_dir_sp <- file.path(MODEL_DIR, SP_DOT)
mf <- list.files(model_dir_sp, pattern="\\.models\\.out$")
mf <- mf[!grepl("\\.ensemble\\.models\\.out$", mf, ignore.case=TRUE)]
if (length(mf) == 0) stop("No .models.out found for: ", SP_NAME)

log_msg(paste0("Loading: ", mf[1]))
m <- get(load(file.path(model_dir_sp, mf[1])))
mod_preds <- m@expl.var.names
log_msg(paste0("Model predictors (", length(mod_preds), "): ", paste(mod_preds, collapse=", ")))

# ── Model selection with frozen thresholds ──
ev <- m@models.evaluation@val
mod_names <- unique(ev$full.name)

all_tss <- c(); all_roc <- c(); all_mn <- c()
for (mn in mod_names) {
  sub <- ev[ev$full.name == mn,]
  tss_val <- suppressWarnings(max(sub$validation[sub$metric.eval == "TSS"], na.rm=TRUE))
  roc_val <- suppressWarnings(max(sub$validation[sub$metric.eval == "ROC"], na.rm=TRUE))
  if (is.finite(tss_val) && is.finite(roc_val)) {
    all_tss <- c(all_tss, tss_val)
    all_roc <- c(all_roc, roc_val)
    all_mn <- c(all_mn, mn)
  }
}

primary_idx <- which(all_tss > 0.8 & all_roc > 0.8)
fallback_idx <- which(all_tss > 0.7 & all_roc > 0.7)

if (length(primary_idx) > 0) {
  selected_idx <- primary_idx
  tier <- "PRIMARY"
} else if (length(fallback_idx) > 0) {
  selected_idx <- fallback_idx
  tier <- "FALLBACK"
} else {
  stop("No qualified models (TSS>0.7 & ROC>0.7) for: ", SP_NAME)
}

selected_models <- all_mn[selected_idx]
log_msg(sprintf("Tier: %s | Models: %d | TSS range: %.3f-%.3f | ROC range: %.3f-%.3f",
  tier, length(selected_models),
  min(all_tss[selected_idx]), max(all_tss[selected_idx]),
  min(all_roc[selected_idx]), max(all_roc[selected_idx])))

# ── Load and subset stack ──
log_msg("Loading predictor stack...")
full_stack <- rast(STACK_PATH)
log_msg(sprintf("Full stack: %d layers", nlyr(full_stack)))

# Verify all model predictors are in stack
missing_from_stack <- setdiff(mod_preds, names(full_stack))
if (length(missing_from_stack) > 0) {
  stop("Predictors missing from stack: ", paste(missing_from_stack, collapse=", "))
}

# Subset stack to model predictors only
stack <- full_stack[[mod_preds]]
log_msg(sprintf("Subset stack: %d layers", nlyr(stack)))

# ── Project ──
PROJ_NAME <- paste0("proj_gap75_", SP_USCORE)
log_msg(paste0("Projecting ", length(selected_models), " models..."))

proj <- tryCatch({
  BIOMOD_Projection(
    bm.mod = m,
    new.env = stack,
    proj.name = PROJ_NAME,
    models.chosen = selected_models,
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

if (is.null(proj)) stop("Projection failed")

log_msg("Projection complete.")

# ── Find projection rasters ──
proj_dir <- file.path(getwd(), SP_NAME, PROJ_NAME)
# The projection output goes to getwd()/species_name/proj_name/
# Search for the actual output directory
possible_dirs <- c(
  proj_dir,
  file.path(getwd(), SP_NAME, paste0("proj_gap75_", SP_USCORE)),
  file.path(OUT_DIR, SP_NAME, PROJ_NAME)
)

# Find individual projection rasters
indiv_rasters <- c()
for (pd in possible_dirs) {
  if (dir.exists(pd)) {
    individual_dir <- file.path(pd, "individual_projections")
    if (dir.exists(individual_dir)) {
      indiv_rasters <- list.files(individual_dir, pattern="\\.tif$", full.names=TRUE)
      if (length(indiv_rasters) > 0) break
    }
    # Also check top level
    top_tifs <- list.files(pd, pattern="\\.tif$", full.names=TRUE)
    if (length(top_tifs) > 0) {
      indiv_rasters <- top_tifs
      break
    }
  }
}

log_msg(sprintf("Found %d projection rasters", length(indiv_rasters)))

# ── Manual EMmean (if ≥2 models) ──
FINAL_RASTER <- file.path(OUT_DIR, paste0(SP_USCORE, "_ensemble_EMmean.tif"))

if (length(indiv_rasters) >= 2) {
  log_msg("Computing manual EMmean...")
  rast_list <- lapply(indiv_rasters, function(f) {
    r <- tryCatch(rast(f), error=function(e) NULL)
    if (!is.null(r)) return(r[[1]]) else return(NULL)
  })
  rast_list <- rast_list[!sapply(rast_list, is.null)]
  
  if (length(rast_list) >= 2) {
    rast_stack <- rast(rast_list)
    emmean <- mean(rast_stack, na.rm=TRUE)
    writeRaster(emmean, FINAL_RASTER, overwrite=TRUE,
      gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
    log_msg(sprintf("EMmean written: %s (%.2f GB)", FINAL_RASTER, file.info(FINAL_RASTER)$size/1e9))
    product_type <- "ENSEMBLE_EMMEAN_MANUAL"
  } else {
    log_msg("WARNING: Fewer than 2 valid rasters, using first available")
    if (length(rast_list) >= 1) {
      writeRaster(rast_list[[1]], FINAL_RASTER, overwrite=TRUE,
        gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
      product_type <- "SINGLE_QUALIFIED_MODEL_FALLBACK"
    } else {
      stop("No valid projection rasters found")
    }
  }
} else if (length(indiv_rasters) == 1) {
  log_msg("Single model — copying as final product")
  r <- rast(indiv_rasters[1])
  writeRaster(r[[1]], FINAL_RASTER, overwrite=TRUE,
    gdal=c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
  product_type <- "SINGLE_QUALIFIED_MODEL"
  log_msg(sprintf("Single-model raster written: %s", FINAL_RASTER))
} else {
  stop("No projection rasters found!")
}

# ── QC ──
r <- rast(FINAL_RASTER)
vals <- values(r, na.rm=TRUE)
qc_ncell <- ncell(r)
qc_nna <- length(vals)
qc_min <- min(vals, na.rm=TRUE)
qc_max <- max(vals, na.rm=TRUE)
qc_mean <- mean(vals, na.rm=TRUE)
qc_neg <- sum(vals < 0, na.rm=TRUE)
qc_inf <- sum(is.infinite(vals))

log_msg(sprintf("QC: cells=%d non-NA=%d min=%.4f max=%.4f mean=%.4f neg=%d inf=%d",
  qc_ncell, qc_nna, qc_min, qc_max, qc_mean, qc_neg, qc_inf))

QC_PASS <- qc_nna > 0 && qc_max > qc_min && qc_inf == 0
log_msg(sprintf("QC_PASS: %s", QC_PASS))

# ── Cleanup temp ──
unlink(task_tmp, recursive=TRUE, force=TRUE)

# ── Write manifest entry ──
manifest_row <- data.frame(
  species = SP_NAME,
  accepted_taxon = SP_NAME,
  tier = tier,
  n_models_selected = length(selected_models),
  selected_models = paste(selected_models, collapse=";"),
  product_type = product_type,
  final_raster = FINAL_RASTER,
  stack_used = STACK_PATH,
  n_predictors = length(mod_preds),
  qc_ncell = qc_ncell,
  qc_nna = qc_nna,
  qc_min = qc_min,
  qc_max = qc_max,
  qc_mean = qc_mean,
  qc_neg = qc_neg,
  qc_inf = qc_inf,
  qc_pass = QC_PASS,
  projection_status = if(QC_PASS) "SUCCESS" else "QC_FAIL",
  timestamp = Sys.time(),
  stringsAsFactors = FALSE
)

manifest_file <- file.path(OUT_DIR, paste0("manifest_", SP_USCORE, ".csv"))
write.csv(manifest_row, manifest_file, row.names=FALSE)

log_msg(paste0("DONE. Status: ", manifest_row$projection_status))
log_msg(paste0("Final raster: ", FINAL_RASTER))

sink(type="output"); sink(type="message")
close(log_con)

cat(paste0("SUCCESS: ", SP_NAME, " | ", product_type, " | ", FINAL_RASTER, "\n"))
