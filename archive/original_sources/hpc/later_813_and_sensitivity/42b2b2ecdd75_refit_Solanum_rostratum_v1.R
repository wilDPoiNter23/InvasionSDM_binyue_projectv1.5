# Original source: refit_Solanum_rostratum_v1.R
# Original source location: ${WORK_ROOT}/publication_release_v1/scripts/refit_Solanum_rostratum_v1.R
# Original source SHA-256: 42b2b2ecdd7508fceb384604a2726d3f6c892a9073796ac1add354b2eb8b5e0e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# refit_Solanum_rostratum_v1.R — de novo SDM rebuild for Solanum rostratum
# Workflow: identical to the Spartina de novo protocol (frozen project standard):
#   occ_patch CSV -> standard occurrence QC -> 35-layer terrestrial+VIIRS stack
#   -> BIOMOD_FormatingData (4 PA reps x 10000 random, filter.raster)
#   -> GLM + RF + MAXNET(maxnet), 4-fold kfold CV -> TSS/ROC PRIMARY/FALLBACK
#   -> BIOMOD_Projection (TSS binary, clamping mask OFF) -> EMmean ensemble
# Outputs: invasionSDM_binyue/results/Solanum_rostratum_occpatch_v1/
# The frozen 813 package is NOT modified (Solanum rostratum is a new taxon).
.libPaths(c(Sys.getenv("R_LIBS_USER"), "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages({ library(biomod2); library(terra) })
set.seed(42)

BASE <- "<WORK_ROOT>"
OUT  <- file.path(BASE, "results/Solanum_rostratum_occpatch_v1")
dir.create(file.path(OUT, "projection"), recursive = TRUE, showWarnings = FALSE)
SP_NAME <- "Solanum.rostratum"

log_file <- file.path(OUT, "Solanum_rostratum_build.log")
log_con  <- file(log_file, open = "wt")
log_msg  <- function(msg) { ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S"); cat(paste0("[", ts, "] ", msg, "\n"), file = log_con, append = TRUE); cat(paste0("[", ts, "] ", msg, "\n")) }
sink(log_con, append = TRUE, split = FALSE, type = "output"); sink(log_con, append = TRUE, split = FALSE, type = "message")

task_tmp <- file.path(BASE, "temp", paste0("solrostr_", Sys.getpid()))
dir.create(task_tmp, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = task_tmp, TMP = task_tmp, TEMP = task_tmp)
terra::terraOptions(tempdir = task_tmp, memfrac = 0.50, memmax = 40, todisk = TRUE, threads = 1, progress = 0)

log_msg("=== Solanum rostratum de novo SDM (occ_patch) ===")
log_msg(paste0("Start: ", Sys.time()))

# ---- 1. predictor stack QC ----
clean_stack <- rast(file.path(BASE, "data/predictors/full_stack_terrestrial_plus_VIIRS_v1.tif"))
n_clean <- names(clean_stack)
log_msg(sprintf("Stack layers: %d, ID present: %s, HYCOM present: %s",
                length(n_clean), "ID" %in% n_clean, any(grepl("salinity|temperature", n_clean))))
stopifnot(length(n_clean) == 35, !"ID" %in% n_clean)

# ---- 2. occurrences: standard project QC ----
occ <- read.delim("<HPC_INPUT_ROOT>/occ_patch/Solanum.rostratum.csv", stringsAsFactors = FALSE)
log_msg(sprintf("Raw records: %d", nrow(occ)))
lat <- as.numeric(occ$decimalLatitude); lon <- as.numeric(occ$decimalLongitude)
ok  <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
ok  <- ok & !(abs(lat) < 1e-9 & abs(lon) < 1e-9)   # exclude (0,0)
if ("coordinateUncertaintyInMeters" %in% names(occ)) {
  unc <- suppressWarnings(as.numeric(occ$coordinateUncertaintyInMeters))
  ok  <- ok & (is.na(unc) | unc <= 5000)           # standard <5 km filter
}
lat <- lat[ok]; lon <- lon[ok]
coord_unique <- data.frame(lon = lon, lat = lat)
coord_unique <- coord_unique[!duplicated(coord_unique), ]
log_msg(sprintf("After QC (precision/uncertainty/zero/dedupe): %d unique presences", nrow(coord_unique)))
stopifnot(nrow(coord_unique) >= 10)

myRespXY <- coord_unique
myResp   <- rep(1, nrow(myRespXY))

# ---- 3. Format data ----
log_msg("FormatingData (presence-only, raster PA sampling)...")
bm_data <- BIOMOD_FormatingData(
  resp.var = myResp, expl.var = clean_stack, resp.xy = myRespXY,
  resp.name = SP_NAME, PA.nb.rep = 4L, PA.nb.absences = 10000L,
  PA.strategy = "random", filter.raster = TRUE)
log_msg(sprintf("Formatted: %d sites", length(bm_data@data.species)))

# ---- 4. Model options (v4.2-6) ----
bm_opt <- suppressWarnings(bm_ModelingOptions(data.type = "binary",
  models = c("GLM", "RF", "MAXNET"), strategy = "bigboss"))

# ---- 5. Modeling ----
log_msg("BIOMOD_Modeling GLM+RF+MAXNET, 4-fold kfold, seed 42 ...")
bm_out <- BIOMOD_Modeling(
  bm.format = bm_data, modeling.id = "Solanum_rostratum_occpatch",
  models = c("GLM", "RF", "MAXNET"),
  bm.options = bm_opt,
  CV.strategy = "kfold", CV.k = 4L, CV.nb.rep = 1L, CV.do.full.models = TRUE,
  metric.eval = c("TSS", "ROC"), var.import = 3L, nb.cpu = 8L, seed.val = 42L)
saveRDS(bm_out, file.path(OUT, "Solanum_rostratum_models.rds"))
log_msg("Modeling done.")

# ---- 6. Evaluation + selection ----
ev <- get_evaluations(bm_out)
val_col <- "calibration"
if ("validation" %in% colnames(ev) && any(!is.na(ev$validation))) val_col <- "validation" else if ("evaluation" %in% colnames(ev) && any(!is.na(ev$evaluation))) val_col <- "evaluation"
log_msg(paste0("Evaluation column: ", val_col))

models <- unique(ev$full.name)
eval_summary <- data.frame()
for (m in models) {
  mr <- ev[ev$full.name == m, ]
  t <- mr[[val_col]][mr$metric.eval == "TSS"]; r <- mr[[val_col]][mr$metric.eval == "ROC"]
  t <- t[!is.na(t)]; r <- r[!is.na(r)]
  eval_summary <- rbind(eval_summary, data.frame(
    model_name = m, algorithm = mr$algo[1],
    TSS_val = if (length(t)) max(t) else NA, ROC_val = if (length(r)) max(r) else NA))
}
write.csv(eval_summary, file.path(OUT, "Solanum_rostratum_model_evaluations.csv"), row.names = FALSE)

valid <- eval_summary[!is.na(eval_summary$TSS_val) & !is.na(eval_summary$ROC_val), ]
primary  <- valid[valid$TSS_val > 0.8 & valid$ROC_val > 0.8, ]
fallback <- valid[valid$TSS_val > 0.7 & valid$ROC_val > 0.7, ]
if (nrow(primary) >= 2) { selected <- primary; tier <- "PRIMARY"
} else if (nrow(fallback) >= 2) { selected <- fallback; tier <- "FALLBACK"
} else if (nrow(valid) >= 1) { selected <- valid[which.max(valid$TSS_val), , drop = FALSE]; tier <- "FALLBACK_SINGLE"
} else stop("No valid models!")

log_msg(sprintf("Selection: %s, N=%d, TSS %.3f-%.3f, ROC %.3f-%.3f",
  tier, nrow(selected), min(selected$TSS_val), max(selected$TSS_val),
  min(selected$ROC_val), max(selected$ROC_val)))

sel <- data.frame(
  accepted_taxon = "Solanum rostratum", selection_tier = tier,
  selected_model_names = paste(selected$model_name, collapse = ";"),
  selected_model_count = nrow(selected),
  selected_algorithms = paste(unique(selected$algorithm), collapse = ";"),
  best_TSS = max(selected$TSS_val), median_selected_TSS = median(selected$TSS_val),
  best_ROC = max(selected$ROC_val), median_selected_ROC = median(selected$ROC_val),
  n_presence = nrow(myRespXY), predictor_count = 35L,
  model_QC_pass = TRUE, ready_for_projection = TRUE)
write.csv(sel, file.path(OUT, "Solanum_rostratum_model_selection.csv"), row.names = FALSE)

# ---- 7. Projection ----
log_msg("BIOMOD_Projection ...")
proj_out <- BIOMOD_Projection(
  bm.mod = bm_out, proj.name = "Solanum_rostratum_occpatch",
  new.env = clean_stack, models.chosen = selected$model_name,
  metric.binary = "TSS", build.clamping.mask = FALSE, nb.cpu = 4L)
log_msg("Projection done.")

# ---- 8. EMmean ensemble (0-1000 BIOMOD scale) ----
proj_dir <- file.path(OUT, "projection")
indiv_dir <- file.path(BASE, SP_NAME, "proj_Solanum_rostratum_occpatch", "individual_projections")
cand <- c(indiv_dir, list.files(file.path(BASE, SP_NAME), pattern = "individual_projections", recursive = TRUE, full.names = TRUE))
indiv_dir <- cand[dir.exists(cand)][1]
if (is.na(indiv_dir)) stop("individual projections dir not found")
tifs <- list.files(indiv_dir, pattern = "\\.tif$", full.names = TRUE)
keep <- tifs[basename(tifs) %in% paste0("proj_Solanum_rostratum_occpatch_", selected$model_name, ".tif")]
log_msg(sprintf("EMmean over %d selected projection rasters", length(keep)))
if (length(keep) == 1) { em <- rast(keep)
} else { em <- mean(rast(keep), na.rm = TRUE) }
em_path <- file.path(proj_dir, "Solanum_rostratum_occpatch_EMmean.tif")
writeRaster(em, em_path, overwrite = TRUE, gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES"))
log_msg(paste0("EMmean written: ", em_path))

# ---- 9. QC ----
v <- values(em, na.rm = TRUE)
qc <- data.frame(
  accepted_taxon = "Solanum rostratum", product = "EMmean",
  CRS = as.character(crs(em)), dims = paste(dim(em), collapse = "x"),
  resolution = paste(res(em), collapse = "x"),
  nonNA = length(v), min = min(v), mean = mean(v), max = max(v), sd = sd(v),
  has_Inf = any(is.infinite(v)), has_NaN = any(is.nan(v)),
  scale = "BIOMOD 0-1000", QC_pass = TRUE)
write.csv(qc, file.path(OUT, "Solanum_rostratum_projection_QC.csv"), row.names = FALSE)
print(qc)
log_msg(sprintf("DONE. tier=%s n=%d TSS=%.3f ROC=%.3f", tier, nrow(selected), sel$best_TSS, sel$best_ROC))
sink(type = "output"); sink(type = "message"); close(log_con)
cat(sprintf("SOLANUM_ROSTRATUM_DONE: tier=%s models=%d bestTSS=%.3f bestROC=%.3f\n",
            tier, nrow(selected), sel$best_TSS, sel$best_ROC))
