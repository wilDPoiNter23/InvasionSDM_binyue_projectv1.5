# Original source: project_gap13_repaired.r
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/project_gap13_repaired.r
# Original source SHA-256: 0b6101de9168bfbf1f00d67cf2dc687017e4d7ff00fd5c456a8659e0e3391971
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# project_gap13_repaired.r — Project 13 repaired species using 35-layer stack
suppressMessages({ library(biomod2); library(terra) })

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Usage: project_gap13_repaired.r <species_name>")
SP_NAME <- args[1]

BASE       <- "<WORK_ROOT>"
STACK_PATH <- file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif")
REPAIR_DIR <- file.path(BASE, "results/gap13_model_repair_fullcoverage_v1")
OUT_DIR    <- file.path(BASE, "results/gap13_repaired_projection_fullcoverage_v1")
MANIFEST_DIR <- file.path(BASE, "publication_release_v1/manifests")

SP_USC <- gsub(" ", "_", SP_NAME)
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

log_file <- file.path(OUT_DIR, paste0(SP_USC, "_projection.log"))
log_con  <- file(log_file, open="wt")
log_msg  <- function(msg) { ts <- format(Sys.time(),"%Y-%m-%d %H:%M:%S"); cat(paste0("[",ts,"] ",msg,"\n"), file=log_con, append=TRUE); cat(paste0("[",ts,"] ",msg,"\n")) }
sink(log_con, append=TRUE, split=FALSE, type="output"); sink(log_con, append=TRUE, split=FALSE, type="message")

log_msg(paste0("=== Project repaired: ", SP_NAME, " ==="))

task_tmp <- file.path(BASE, "temp", paste0("proj13_", SP_USC, "_", Sys.getpid()))
dir.create(task_tmp, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TMPDIR=task_tmp, TMP=task_tmp, TEMP=task_tmp)
terra::terraOptions(tempdir=task_tmp, memfrac=0.50, memmax=40, todisk=TRUE, threads=1, progress=0)

# ── Enter species directory (symlinks already set up) ──
spd <- gsub(" ", ".", SP_NAME)
model_sp_dir <- file.path(BASE, "models", spd)
if (!dir.exists(model_sp_dir)) stop("Model directory not found: ", model_sp_dir)
setwd(model_sp_dir)
log_msg(paste0("Working dir: ", getwd()))

# ── Load repair model and selection ──
model_rds <- file.path(REPAIR_DIR, paste0(SP_USC, "_repair_models.rds"))
if (!file.exists(model_rds)) stop("Model RDS not found: ", model_rds)

mod <- readRDS(model_rds)
sel_file <- file.path(REPAIR_DIR, paste0("selection_", SP_USC, ".csv"))
if (!file.exists(sel_file)) stop("Selection file not found")
sel <- read.csv(sel_file, stringsAsFactors=FALSE)

selected_models <- if (sel$models != "") strsplit(sel$models, ";")[[1]] else character(0)
n_sel <- length(selected_models)
log_msg(sprintf("Selected models: %d", n_sel))
for (m in selected_models) log_msg(sprintf("  %s", m))

# ── Predictor stack ──
mod_preds <- mod@expl.var.names  # keep ID if model was built with it
log_msg(sprintf("Predictors: %d", length(mod_preds)))

stack_full <- rast(STACK_PATH)
missing <- setdiff(mod_preds, names(stack_full))

# Add ID layer if model was built with terra extract() ID artifact
if ("ID" %in% missing && "ID" %in% mod_preds) {
  log_msg("Adding dummy ID layer (model built with extract() artifact)")
  id_rast <- stack_full[[1]]  # use first layer as template
  values(id_rast) <- 1:ncell(id_rast)
  names(id_rast) <- "ID"
  stack_full <- c(stack_full, id_rast)
  missing <- setdiff(mod_preds, names(stack_full))
}

if (length(missing) > 0) stop("Missing predictors: ", paste(missing, collapse=", "))
stack <- stack_full[[mod_preds]]
log_msg(sprintf("Stack subset: %d layers", nlyr(stack)))

# ── Project ──
PROJ_NAME <- paste0("gap13_", SP_USC)
log_msg(paste0("Projecting ", n_sel, " models..."))
rc <- system.time({
  proj <- tryCatch({
    BIOMOD_Projection(bm.mod=mod, new.env=stack, proj.name=PROJ_NAME,
      models.chosen=selected_models, binary.meth=NULL, clamping.mask=TRUE,
      build.clamping.mask=TRUE, output.format=".tif", compress=TRUE,
      do.stack=FALSE, keep.in.memory=FALSE, nb.cpu=1, overwrite=TRUE)
  }, error=function(e){ log_msg(paste0("PROJECTION ERROR: ",conditionMessage(e))); NULL })
})
log_msg(sprintf("Projection time: %.1f min", rc[3]/60))
if (is.null(proj)) stop("Projection failed")

# ── Locate individual rasters ──
sp_name_dot <- mod@sp.name
proj_out_dir <- file.path(getwd(), sp_name_dot, paste0("proj_", PROJ_NAME), "individual_projections")
log_msg(paste0("Looking for rasters in: ", proj_out_dir))

indiv_rasters <- if (dir.exists(proj_out_dir)) {
  list.files(proj_out_dir, pattern="\\.tif$", full.names=TRUE)
} else character(0)
log_msg(sprintf("Found %d rasters", length(indiv_rasters)))

# ── Manual EMmean ──
FINAL_RASTER <- file.path(OUT_DIR, paste0(SP_USC, "_ensemble_EMmean.tif"))
if (length(indiv_rasters) >= 2) {
  log_msg("Computing manual EMmean...")
  rast_list <- lapply(indiv_rasters, function(f) tryCatch(rast(f), error=function(e) NULL))
  rast_list <- rast_list[!sapply(rast_list, is.null)]
  if (length(rast_list) >= 2) {
    rast_stack <- rast(rast_list)
    emmean <- mean(rast_stack, na.rm=TRUE)
    writeRaster(emmean, FINAL_RASTER, overwrite=TRUE, gdal=c("COMPRESS=LZW","PREDICTOR=2","TILED=YES"))
    product_type <- "ENSEMBLE_EMMEAN_MANUAL"
  } else {
    writeRaster(rast_list[[1]], FINAL_RASTER, overwrite=TRUE, gdal=c("COMPRESS=LZW","PREDICTOR=2","TILED=YES"))
    product_type <- "SINGLE_QUALIFIED_MODEL_FALLBACK"
  }
} else if (length(indiv_rasters) == 1) {
  r <- rast(indiv_rasters[1])
  writeRaster(r[[1]], FINAL_RASTER, overwrite=TRUE, gdal=c("COMPRESS=LZW","PREDICTOR=2","TILED=YES"))
  product_type <- "SINGLE_QUALIFIED_MODEL"
} else {
  stop("No projection rasters found")
}
log_msg(paste0("Final raster: ", FINAL_RASTER))

# ── QC ──
r <- rast(FINAL_RASTER)
vals <- values(r, na.rm=TRUE)
ref <- rast(file.path(BASE, "data/predictors/full_stack_terrestrial_primary_v1.tif"))
qc <- data.frame(
  species = SP_NAME,
  product_type = product_type,
  raster_path = FINAL_RASTER,
  raster_exists = file.exists(FINAL_RASTER),
  SHA256 = system(sprintf("sha256sum %s | cut -d' ' -f1", FINAL_RASTER), intern=TRUE),
  CRS = crs(r, describe=TRUE)$code,
  extent_match = all(ext(r) == ext(ref)),
  resolution_match = all(res(r) == res(ref)),
  dim_match = all(dim(r)[1:2] == dim(ref)[1:2]),
  n_non_NA = length(vals),
  min = min(vals, na.rm=TRUE),
  mean = mean(vals, na.rm=TRUE),
  max = max(vals, na.rm=TRUE),
  all_NA = length(vals) == 0,
  all_zero = all(vals == 0),
  Inf_count = sum(is.infinite(vals)),
  NaN_count = sum(is.nan(vals)),
  geometry_pass = all(ext(r)==ext(ref)) & all(res(r)==res(ref)) & all(dim(r)[1:2]==dim(ref)[1:2]),
  numeric_QC_pass = length(vals) > 1000 & max(vals,na.rm=T) > min(vals,na.rm=T) & sum(is.infinite(vals))==0,
  final_QC_pass = FALSE,
  stringsAsFactors = FALSE
)
qc$final_QC_pass <- qc$geometry_pass & qc$numeric_QC_pass & qc$raster_exists

write.csv(qc, file.path(OUT_DIR, paste0("QC_", SP_USC, ".csv")), row.names=FALSE)
log_msg(sprintf("QC: geom=%s numeric=%s final=%s", qc$geometry_pass, qc$numeric_QC_pass, qc$final_QC_pass))

# ── Cleanup ──
unlink(task_tmp, recursive=TRUE, force=TRUE)
log_msg("DONE")
sink(type="output"); sink(type="message"); close(log_con)
cat(paste0("PROJ_DONE: ", SP_NAME, " | ", product_type, " | QC=", qc$final_QC_pass, "\n"))
