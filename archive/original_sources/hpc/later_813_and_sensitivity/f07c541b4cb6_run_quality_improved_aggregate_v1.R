# Original source: run_quality_improved_aggregate_v1.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_quality_improved_aggregate_v1.R
# Original source SHA-256: f07c541b4cb62adde146bfaa1f201fd882f9fc667f1d3419bd8cc43254e95ba9
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Build quality-improved 813 aggregate v1 using corrected projections for 3 species
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(biomod2)
library(terra)

base_dir <- "<WORK_ROOT>"
model_dir <- file.path(base_dir, "models")
out_dir <- file.path(base_dir, "results", "aggregation_813_quality_improved_v1_20260729")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)
cat("Species:", nrow(summary_df), "\n")

# 3 corrected species with their improved RDS paths
corrected <- list(
  "Adiantum latifolium" = list(algo="RFd", rds="results/targeted_model_improvement_20260729/Adiantum latifolium_improved_RFd_projection.rds"),
  "Diodia teres" = list(algo="RFd", rds="results/targeted_model_improvement_20260729/Diodia teres_improved_RFd_projection.rds"),
  "Musa ornata" = list(algo="MAXNET", rds="results/targeted_model_improvement_20260729/Musa ornata_improved_MAXNET_projection.rds")
)

normalize_raster <- function(r) {
  mm <- terra::minmax(r); if (is.na(mm[1])||is.na(mm[2])||mm[1]==mm[2]) return(r*0)
  (r - mm[1]) / (mm[2] - mm[1])
}

find_raster <- function(sp_dot, proj_type) {
  proj_subdir <- file.path(model_dir, sp_dot, paste0("proj_",proj_type,"_projection"))
  if (!dir.exists(proj_subdir)) return(character(0))
  files <- list.files(proj_subdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  suit <- files[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux)", files, ignore.case=TRUE)]
  if (length(suit)>0) return(suit[1])
  if (length(files)>0) return(files[1])
  return(character(0))
}

# Build source table
source_rows <- list()
r_sum <- NULL; count <- 0; fail <- 0

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]; sp_dot <- summary_df$species_dot[i]; proj_type <- summary_df$type[i]
  
  if (i %% 100 == 0) cat(sprintf("[%03d/%03d] %d ok, %d fail\n", i, nrow(summary_df), count, fail))
  
  # Check if this is a corrected species
  if (sp %in% names(corrected)) {
    ci <- corrected[[sp]]
    proj <- tryCatch(readRDS(ci$rds), error=function(e) NULL)
    if (!is.null(proj)) {
      r <- tryCatch(get_predictions(proj), error=function(e) NULL)
      if (!is.null(r) && inherits(r, "SpatRaster")) {
        r <- normalize_raster(r); r <- terra::ifel(is.na(r), 0, r)
        if (is.null(r_sum)) { r_sum <- r } else { r_sum <- terra::cover(r_sum+r, r_sum) }
        count <- count + 1
        source_rows[[length(source_rows)+1]] <- c(sp, "corrected", ci$rds, ci$algo, "corrected_v1", "improved", "yes", "")
        next
      }
    }
    # Fallback to original if corrected fails
    cat(sprintf("WARNING: %s corrected failed, using original\n", sp))
  }
  
  # Use original projection
  raster_file <- find_raster(sp_dot, proj_type)
  if (length(raster_file) == 0) { fail <- fail+1; next }
  r <- tryCatch(terra::rast(raster_file), error=function(e) NULL)
  if (is.null(r)) { fail <- fail+1; next }
  r <- normalize_raster(r); r <- terra::ifel(is.na(r), 0, r)
  if (is.null(r_sum)) { r_sum <- r } else { r_sum <- terra::cover(r_sum+r, r_sum) }
  count <- count + 1
  source_rows[[length(source_rows)+1]] <- c(sp, "original", raster_file, paste(proj_type,"original"), "none", "standard", "yes", "")
}

# Write output
cat(sprintf("\nFinal: %d successful, %d failed\n", count, fail))
r_mean <- r_sum / count
output_file <- file.path(out_dir, "hotspot_mean_suitability_quality_improved_813_v1.tif")
terra::writeRaster(r_mean, output_file, overwrite=TRUE)
cat("Output:", output_file, "\n")
mm <- minmax(r_mean); gm <- global(r_mean,"mean",na.rm=TRUE)
cat(sprintf("Min:%.4f Max:%.4f Mean:%.4f\n", mm[1], mm[2], gm[1,1]))

# Write source table
src_csv <- file.path(out_dir, "quality_improved_813_species_source_table_v1.csv")
write.csv(data.frame(
  species=sapply(source_rows,`[`,1), source=sapply(source_rows,`[`,2),
  path=sapply(source_rows,`[`,3), algorithm=sapply(source_rows,`[`,4),
  correction_status=sapply(source_rows,`[`,5), quality_flag=sapply(source_rows,`[`,6),
  included=sapply(source_rows,`[`,7), notes=sapply(source_rows,`[`,8)
), src_csv, row.names=FALSE)
cat("Source table:", src_csv, "\n")
cat("Done:", format(Sys.time()), "\n")
