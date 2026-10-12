# Original source: repair_single_species.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/repair_single_species.r
# Original source SHA-256: 4967887bbed59f0b0f59e97b556a6d276cdb8bfc4ec8fae451fe2882e9a9974e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# repair_single_species.r — Reformat + model single species with primary stack
# Usage: Rscript repair_single_species.r "Ailanthus altissima"

args <- commandArgs(trailingOnly = TRUE)
sp <- args[1]

.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))

cat(sprintf("\n========== %s ==========\n", sp))

base_dir <- "<WORK_ROOT>"
fs_primary <- rast(file.path(base_dir, "data/predictors/full_stack_terrestrial_primary_v1.tif"))
occ_base <- file.path(base_dir, "data/species_occ")
out_base <- file.path(base_dir, "results/negative_TSS_repair_pilot_v1")
dir.create(out_base, showWarnings=FALSE, recursive=TRUE)

# Species mapping
sp_map <- list(
  "Ailanthus altissima" = c("Ailanthus_altissima","Ailanthus_altissima_occurrences.csv"),
  "Tamarix aphylla"     = c("Tamarix_aphylla","Tamarix_aphylla_occurrences.csv"),
  "Veronica arvensis"   = c("Veronica_arvensis","Veronica_arvensis_occurrences.csv"),
  "Geranium dissectum"  = c("Geranium_dissectum","Geranium_dissectum_occurrences.csv"),
  "Ageratum houstonianum" = c("Ageratum_houstonianum","Ageratum_houstonianum_occurrences.csv")
)

sp_info <- sp_map[[sp]]
sp_dir <- sp_info[1]
csv_fn <- sp_info[2]
resp_name <- gsub(" ", ".", sp)

# ── 1. Read raw occurrences ──
csv_path <- file.path(occ_base, sp_dir, csv_fn)
occ <- read.csv(csv_path, stringsAsFactors=FALSE)
n_raw <- nrow(occ)
cat(sprintf("Raw: %d\n", n_raw))

# ── 2. Clean coordinates ──
lat <- occ[["decimalLatitude"]]; lon <- occ[["decimalLongitude"]]
valid <- !is.na(lat) & !is.na(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
lat_c <- lat[valid]; lon_c <- lon[valid]
n_valid <- sum(valid)
cat(sprintf("Valid: %d\n", n_valid))
if (n_valid < 5) stop("Too few valid coordinates")

# ── 3. Terrestrial + deduplicate ──
xy <- cbind(lon_c, lat_c)
bio1 <- fs_primary[["wc2.1_2.5m_bio_1"]]
bio1_ext <- extract(bio1, xy)[,1]
is_land <- !is.na(bio1_ext)
xy_land <- xy[is_land, , drop=FALSE]
cells <- cellFromXY(bio1, xy_land)
unique_cells <- as.integer(names(table(cells)))
n_unique <- length(unique_cells)
cat(sprintf("Unique terrestrial cells: %d\n", n_unique))
if (n_unique < 5) stop("Too few unique cells")

# ── 4. Extract primary stack ──
cell_xy <- xyFromCell(bio1, unique_cells)
ext <- extract(fs_primary, cell_xy)
ext_data <- ext[, -1, drop=FALSE]
cc_idx <- which(complete.cases(ext_data))
n_cc <- length(cc_idx)
cat(sprintf("Complete cases: %d/%.0f (%.1f%%)\n", n_cc, n_unique, n_cc/n_unique*100))
if (n_cc < 5) stop("Too few complete cases")

# ── 5. Formatted data ──
resp <- rep(1, n_cc)
env_cc <- ext_data[cc_idx, , drop=FALSE]
coord_cc <- cell_xy[cc_idx, , drop=FALSE]

pa_nb_abs <- min(1000, n_cc * 5)

cat(sprintf("Creating formatted data: %d pres, %d PA\n", n_cc, pa_nb_abs))
fmtd <- tryCatch({
  BIOMOD_FormatingData(
    resp.var = resp, expl.var = env_cc, resp.xy = coord_cc,
    resp.name = resp_name,
    PA.nb.rep = 4, PA.nb.absences = pa_nb_abs, PA.strategy = "random",
    na.rm = TRUE
  )
}, error = function(e) { cat(sprintf("FormatingData ERROR: %s\n", e$message)); return(NULL) })
if (is.null(fmtd)) stop("FormatingData failed")

saveRDS(fmtd, file.path(out_base, paste0(resp_name, "_formatted.rds")))

# ── 6. Modelling ──
cat("Running BIOMOD_Modeling...\n")
model_opts <- BIOMOD_ModelingOptions()

individual_models <- tryCatch({
  BIOMOD_Modeling(
    bm.form = fmtd, modeling.id = "repair_pilot",
    models = c("GLM","RFd","MAXNET"),
    models.options = model_opts,
    CV.nb.rep = 4, CV.perc = 0.80, CV.strategy = "random",
    metric.eval = c("TSS","ROC"), var.import = 3, seed.val = 42
  )
}, error = function(e) { cat(sprintf("Modeling ERROR: %s\n", e$message)); return(NULL) })
if (is.null(individual_models)) stop("Modeling failed")

saveRDS(individual_models, file.path(out_base, paste0(resp_name, "_individual.rds")))

# ── 7. Evaluate ──
eval <- get_evaluations(individual_models)
tss_vals <- eval["TSS","Testing.data",]
roc_vals <- eval["ROC","Testing.data",]
best_tss_idx <- which.max(tss_vals)
best_tss <- tss_vals[best_tss_idx]
best_roc <- roc_vals[best_tss_idx]
best_algo <- names(best_tss_idx)

cat(sprintf("Best TSS: %.4f (%s)  Best ROC: %.4f\n", best_tss, best_algo, best_roc))

# ── 8. Summary ──
summ <- data.frame(
  species = sp, raw_presences = n_raw, valid_coords = n_valid,
  unique_cells = n_unique, new_stack_CC = n_cc,
  best_TSS = round(best_tss, 4), best_ROC = round(best_roc, 4),
  best_algo = best_algo,
  status = if(best_tss > 0) "TSS_POSITIVE" else "TSS_NEGATIVE",
  stringsAsFactors = FALSE
)
write.csv(summ, file.path(out_base, paste0(resp_name, "_summary.csv")), row.names=FALSE)
cat(sprintf("Done: %s\n", sp))
