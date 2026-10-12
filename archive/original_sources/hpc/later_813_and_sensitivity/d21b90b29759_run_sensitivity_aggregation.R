# Original source: run_sensitivity_aggregation.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_sensitivity_aggregation.R
# Original source SHA-256: d21b90b2975923b4f024d60d3a58b5532164b3d4962dcfbb9675bef0c5dde2e2
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Generate sensitivity aggregates: exclude negative-TSS and all-zero species
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(terra)

base_dir <- "<WORK_ROOT>"
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
out_dir <- file.path(base_dir, "results/aggregation_sensitivity_qc_20260729")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

# Problem species
negative_tss <- c("Adiantum latifolium","Ageratum houstonianum","Ailanthus altissima",
  "Cenchrus echinatus","Diodia teres","Geranium dissectum","Glebionis coronaria",
  "Hibiscus trionum","Musa ornata","Tamarix aphylla","Urochloa eminii",
  "Veronica arvensis","Vicia eriocarpa")
all_zero <- c("Lepidium draba","Sonchus oleraceus")

summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

normalize_raster <- function(r) {
  mm <- terra::minmax(r); if (is.na(mm[1])||is.na(mm[2])||mm[1]==mm[2]) return(r*0)
  (r - mm[1]) / (mm[2] - mm[1])
}

find_raster <- function(sp_dot, proj_type) {
  proj_subdir <- file.path(model_dir, sp_dot, paste0("proj_",proj_type,"_projection"))
  if (!dir.exists(proj_subdir)) return(character(0))
  files <- list.files(proj_subdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  suit <- files[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux\\.xml)", files, ignore.case=TRUE)]
  if (length(suit)>0) return(suit[1])
  if (length(files)>0) return(files[1])
  return(character(0))
}

# Build three aggregates
build_agg <- function(exclude_species, out_file, label) {
  r_sum <- NULL; count <- 0
  for (i in seq_len(nrow(summary_df))) {
    sp <- summary_df$species[i]
    if (sp %in% exclude_species) next
    sp_dot <- summary_df$species_dot[i]; proj_type <- summary_df$type[i]
    raster_file <- find_raster(sp_dot, proj_type)
    if (length(raster_file)==0) next
    r <- tryCatch(terra::rast(raster_file), error=function(e) NULL)
    if (is.null(r)) next
    r <- normalize_raster(r); r <- terra::ifel(is.na(r), 0, r)
    if (is.null(r_sum)) { r_sum <- r } else { r_sum <- terra::cover(r_sum+r, r_sum) }
    count <- count + 1
    if (i %% 100 == 0) cat(sprintf("  [%s] %d/%d\n", label, count, i))
  }
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, out_file, overwrite=TRUE)
  cat(sprintf("%s: %d species, output=%s\n", label, count, out_file))
  return(list(raster=r_mean, count=count))
}

cat("Building sensitivity products...\n")

# Product A: exclude negative-TSS only
cat("\n--- Product A: exclude negative-TSS ---\n")
resA <- build_agg(negative_tss, file.path(out_dir, "hotspot_mean_suitability_excluding_negative_TSS.tif"), "A")

# Product B: exclude negative-TSS + all-zero
cat("\n--- Product B: exclude negative-TSS + all-zero ---\n")
resB <- build_agg(c(negative_tss, all_zero), file.path(out_dir, "hotspot_mean_suitability_excluding_negative_TSS_and_all_zero.tif"), "B")

# Load full 813 raster for comparison
cat("\n--- Computing differences ---\n")
full_813 <- rast(file.path(base_dir, "results/aggregation_813_final_20260729/hotspot_mean_suitability_normalized.tif"))

diffA <- full_813 - resA$raster
terra::writeRaster(diffA, file.path(out_dir, "difference_full813_minus_excluding_negative_TSS.tif"), overwrite=TRUE)

diffB <- full_813 - resB$raster
terra::writeRaster(diffB, file.path(out_dir, "difference_full813_minus_excluding_negative_TSS_and_all_zero.tif"), overwrite=TRUE)

# Comparison stats
cat("\n=== Comparison Statistics ===\n")
for (label in c("A (excl negative-TSS)", "B (excl negative-TSS + all-zero)")) {
  diff_rast <- if(label=="A (excl negative-TSS)") diffA else diffB
  excl_count <- if(label=="A (excl negative-TSS)") length(negative_tss) else length(c(negative_tss,all_zero))
  incl_count <- 813 - excl_count
  
  pearson <- global(c(full_813, diff_rast), "pearson", na.rm=TRUE)  # won't work this way
  # Use cell values directly
  vals_full <- values(full_813, na.rm=TRUE)
  if (label=="A (excl negative-TSS)") {
    vals_filt <- values(resA$raster, na.rm=TRUE)
  } else {
    vals_filt <- values(resB$raster, na.rm=TRUE)
  }
  
  # Sample for correlation (full raster is large)
  set.seed(42)
  idx <- sample(length(vals_full), min(100000, length(vals_full)))
  vf <- vals_full[idx]; vfl <- vals_filt[idx]
  
  pearson_r <- cor(vf, vfl, method="pearson")
  spearman_r <- cor(vf, vfl, method="spearman")
  mad <- mean(abs(vf - vfl))
  maxd <- max(abs(vf - vfl))
  rmse <- sqrt(mean((vf - vfl)^2))
  pct_001 <- mean(abs(vf - vfl) > 0.01) * 100
  pct_005 <- mean(abs(vf - vfl) > 0.05) * 100
  
  cat(sprintf("\n%s:\n", label))
  cat(sprintf("  Species: full=813, excluded=%d, included=%d\n", excl_count, incl_count))
  cat(sprintf("  Pearson r: %.6f\n", pearson_r))
  cat(sprintf("  Spearman r: %.6f\n", spearman_r))
  cat(sprintf("  Mean abs diff: %.6f\n", mad))
  cat(sprintf("  Max abs diff: %.6f\n", maxd))
  cat(sprintf("  RMSE: %.6f\n", rmse))
  cat(sprintf("  Pct cells diff > 0.01: %.2f%%\n", pct_001))
  cat(sprintf("  Pct cells diff > 0.05: %.2f%%\n", pct_005))
}

cat("\n=== DONE ===\n")
