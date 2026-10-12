# Original source: rebuild_ensemble_v2.r
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/rebuild_ensemble_v2.r
# Original source SHA-256: 435975fdcf60714bfa569499066dbe06e349f1923b282144391838d01b38d097
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# rebuild_ensemble_v2.r
# Clean rewrite: read evaluation from CSV, build proper ensemble, project
# Usage: Rscript rebuild_ensemble_v2.r "Vulpia fasciculata"

args <- commandArgs(trailingOnly = TRUE)
species_name <- args[1]

renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
suppressMessages(library(biomod2))
suppressMessages(library(terra))

cat(sprintf("\n========== %s ==========\n", species_name))

base_dir <- "<WORK_ROOT>"

# ── Step 1: Find ALL models with TSS>0.8 AND ROC>0.8 from CSV ──
# Search across all comprehensive eval CSVs
comp_dir <- file.path(base_dir, "results/evaluations/comprehensive")
csv_files <- list.files(comp_dir, pattern = "_evaluationTotalReport.csv", full.names = TRUE)

tss_data <- list()
roc_data <- list()
for (f in csv_files) {
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df)) next
    sp_rows <- df[df$Species == species_name, ]
    if (nrow(sp_rows) == 0) next
    # Filter TSS and ROC
    tss_rows <- sp_rows[sp_rows$metric.eval == "TSS" & !is.na(sp_rows$validation) & sp_rows$validation != "NA", ]
    roc_rows <- sp_rows[sp_rows$metric.eval == "ROC" & !is.na(sp_rows$validation) & sp_rows$validation != "NA", ]
    if (nrow(tss_rows) > 0) tss_data[[length(tss_data) + 1]] <- tss_rows
    if (nrow(roc_rows) > 0) roc_data[[length(roc_data) + 1]] <- roc_rows
}

if (length(tss_data) == 0) stop("No TSS data found for ", species_name)

tss_all <- do.call(rbind, tss_data)
roc_all <- do.call(rbind, roc_data)
cat(sprintf("TSS rows: %d, ROC rows: %d\n", nrow(tss_all), nrow(roc_all)))

# Merge TSS and ROC by full.name
tss_vals <- setNames(tss_all$validation, tss_all$full.name)
roc_vals <- setNames(roc_all$validation, roc_all$full.name)
common_names <- intersect(names(tss_vals), names(roc_vals))

cat(sprintf("Models with both TSS and ROC: %d\n", length(common_names)))

# Filter: TSS>0.8 AND ROC>0.8
good <- common_names[
    as.numeric(tss_vals[common_names]) > 0.8 &
    as.numeric(roc_vals[common_names]) > 0.8
]
cat(sprintf("Models with TSS>0.8 & ROC>0.8: %d\n", length(good)))
for (m in good) {
    cat(sprintf("  %s: TSS=%.3f ROC=%.3f\n", m, 
        as.numeric(tss_vals[m]), as.numeric(roc_vals[m])))
}

# Fallback to TSS>0.7 if none
if (length(good) == 0) {
    good <- common_names[
        as.numeric(tss_vals[common_names]) > 0.7 &
        as.numeric(roc_vals[common_names]) > 0.7
    ]
    cat(sprintf("Fallback TSS>0.7 & ROC>0.7: %d models\n", length(good)))
}

# If still none, take top by TSS
if (length(good) == 0) {
    top <- common_names[order(as.numeric(tss_vals[common_names]), decreasing = TRUE)]
    good <- head(top, 3)
    cat(sprintf("Using top 3 by TSS: %d models\n", length(good)))
}

# Get unique model names (dedup by base name before _RUN)
good <- unique(good)
cat(sprintf("Unique models: %d\n", length(good)))

# ── Step 2: Build ensemble (if >=2 models) ──
ind_path <- file.path(base_dir, "results/individual_models", paste0(species_name, "_individual.rds"))
individual_models <- readRDS(ind_path)

if (length(good) >= 2) {
    cat(sprintf("\nBuilding ensemble with %d models...\n", length(good)))
    ensemble_models <- tryCatch({
        BIOMOD_EnsembleModeling(
            bm.mod = individual_models,
            models.chosen = good,
            em.by = "algo",
            em.algo = c("EMmean", "EMmedian"),
            metric.select = c("TSS"),
            metric.select.thresh = c(0),
            metric.eval = c("TSS", "ROC"),
            var.import = 3,
            EMci.alpha = 0.05,
            EMwmean.decay = "proportional"
        )
    }, error = function(e) {
        cat(sprintf("ERROR: %s\n", e$message))
        return(NULL)
    })
    
    if (is.null(ensemble_models)) stop("Ensemble build failed")
    
    # Save
    saveRDS(ensemble_models, file.path(base_dir, "results/ensemble_models", 
        paste0(species_name, "_ensemble.rds")))
    saveRDS(good, file.path(base_dir, "results/ensemble_models", 
        paste0(species_name, "_best_ensemble.rds")))
    
    cat(sprintf("Ensemble saved: %d models\n", length(good)))
    
    proj_type <- "ensemble"
    use_ensemble <- TRUE
} else {
    cat(sprintf("\nOnly %d model(s) — individual projection\n", length(good)))
    proj_type <- "fallback"
    use_ensemble <- FALSE
    best_model <- good[1]
}

# ── Step 3: Load env stack ──
env_path <- file.path(base_dir, "data/environmental", paste0(species_name, "_env_stack_final.rds"))
env_stack_final <- readRDS(env_path)
cat(sprintf("Env: %d layers\n", nlyr(env_stack_final)))

# ── Step 4: Project ──
setwd(file.path(base_dir, "models"))

if (use_ensemble) {
    my_proj <- tryCatch({
        BIOMOD_EnsembleForecasting(
            bm.em = ensemble_models,
            proj.name = "ensemble_projection",
            new.env = env_stack_final,
            models.chosen = "EMmeanByTSS",
            metric.binary = "TSS",
            build.clamping.mask = FALSE,
            compress = TRUE,
            nb.cpu = 8
        )
    }, error = function(e) {
        cat(sprintf("ERROR in ensemble forecasting: %s\n", e$message))
        return(NULL)
    })
} else {
    my_proj <- tryCatch({
        BIOMOD_Projection(
            bm.mod = individual_models,
            proj.name = "fallback_projection",
            new.env = env_stack_final,
            models.chosen = best_model,
            metric.binary = "TSS",
            build.clamping.mask = FALSE,
            compress = TRUE,
            nb.cpu = 8
        )
    }, error = function(e) {
        cat(sprintf("ERROR in projection: %s\n", e$message))
        return(NULL)
    })
}

if (is.null(my_proj)) stop("Projection failed")

# ── Step 5: Save ──
rds_out <- file.path(base_dir, "results/projections", 
    paste0(species_name, "_", proj_type, "_projection.rds"))
saveRDS(my_proj, file = rds_out)
cat(sprintf("Saved: %s\n", rds_out))

# ── Step 6: Verify ──
pred <- tryCatch(get_predictions(my_proj, as.raster = TRUE), error = function(e) NULL)
if (!is.null(pred)) {
    mm <- terra::minmax(pred)
    nz <- terra::global(pred > 0, "sum", na.rm = TRUE)[1, 1]
    nc <- terra::ncell(pred)
    cat(sprintf("VERIFY: dim=%dx%d range=[%.4f,%.4f] nonzero=%.2f%%\n",
        dim(pred)[1], dim(pred)[2], mm[1], mm[2], nz / nc * 100))
}

cat(sprintf("Done: %s\n", Sys.time()))
