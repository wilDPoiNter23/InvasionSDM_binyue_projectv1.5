# Original source: run_strict_rescue_aggregate.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/run_strict_rescue_aggregate.R
# Original source SHA-256: aea95e50374ef4555a78f0fb256c83b7f90c2adfca0b054da3b9ead918d5a0e7
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
#!/usr/bin/env Rscript
# Build STRICT RESCUE 813 aggregate
# Policy: TSS<0 rejected, only CV-validated algos used for allRun
renv_lib <- "<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu"
.libPaths(c(renv_lib, .libPaths()))
library(biomod2); library(terra)

base_dir <- "<WORK_ROOT>"
out_dir <- file.path(base_dir, "results", "aggregation_813_strict_rescue_20260729")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

summary_df <- read.csv(file.path(base_dir, "results/evaluations/projection_summary.csv"))
summary_df$species_dot <- gsub(" ", ".", summary_df$species)
cat("Species:", nrow(summary_df), "\n")

# Strict rescue projections (CV-validated algorithm → allRun projection)
rescued <- list(
  "Urochloa eminii" = list(algo="GLM", quality="standard_pass", tss=0.625, auc=0.812,
    rds="results/targeted_model_improvement_20260729/Urochloa eminii_improved_GLM_projection.rds"),
  "Musa ornata" = list(algo="RFd", quality="weak_positive", tss=0.217, auc=0.622,
    rds="results/targeted_model_improvement_20260729/Musa ornata_improved_RFd_projection.rds"),
  "Vicia eriocarpa" = list(algo="RFd", quality="weak_positive", tss=0.250, auc=0.623,
    rds="results/targeted_model_improvement_20260729/Vicia eriocarpa_improved_RFd_projection.rds")
)

# Species with no acceptable model → retain original, flag unresolved
unresolved <- c("Adiantum latifolium","Ageratum houstonianum","Ailanthus altissima",
  "Cenchrus echinatus","Diodia teres","Geranium dissectum","Glebionis coronaria",
  "Hibiscus trionum","Tamarix aphylla","Veronica arvensis")

normalize_raster <- function(r) {
  mm <- terra::minmax(r); if (is.na(mm[1])||is.na(mm[2])||mm[1]==mm[2]) return(r*0)
  (r - mm[1]) / (mm[2] - mm[1])
}

find_raster <- function(sp_dot, proj_type) {
  proj_subdir <- file.path(base_dir, "models", sp_dot, paste0("proj_",proj_type,"_projection"))
  if (!dir.exists(proj_subdir)) return(character(0))
  files <- list.files(proj_subdir, pattern="\\.(tif|grd)$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  suit <- files[!grepl("(TSSbin|TSSfilt|ClampingMask|\\.aux)", files, ignore.case=TRUE)]
  if (length(suit)>0) return(suit[1])
  if (length(files)>0) return(files[1])
  return(character(0))
}

source_rows <- list()
r_sum <- NULL; count <- 0; fail <- 0

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]; sp_dot <- summary_df$species_dot[i]; proj_type <- summary_df$type[i]
  if (i %% 100 == 0) cat(sprintf("[%03d/%03d] %d ok, %d fail\n", i, nrow(summary_df), count, fail))
  
  if (sp %in% names(rescued)) {
    ri <- rescued[[sp]]
    proj <- tryCatch(readRDS(ri$rds), error=function(e) NULL)
    if (!is.null(proj)) {
      r <- tryCatch(get_predictions(proj), error=function(e) NULL)
      if (!is.null(r) && inherits(r,"SpatRaster")) {
        r <- normalize_raster(r); r <- terra::ifel(is.na(r),0,r)
        if (is.null(r_sum)) { r_sum <- r } else { r_sum <- terra::cover(r_sum+r, r_sum) }
        count <- count+1
        source_rows[[length(source_rows)+1]] <- c(sp, "rescued", ri$rds, ri$algo, ri$quality, "yes", "")
        next
      }
    }
    cat(sprintf("WARNING: %s rescue failed, using original\n", sp))
  }
  
  # Original projection
  raster_file <- find_raster(sp_dot, proj_type)
  if (length(raster_file)==0) { fail<-fail+1; next }
  r <- tryCatch(terra::rast(raster_file), error=function(e) NULL)
  if (is.null(r)) { fail<-fail+1; next }
  r <- normalize_raster(r); r <- terra::ifel(is.na(r),0,r)
  if (is.null(r_sum)) { r_sum <- r } else { r_sum <- terra::cover(r_sum+r, r_sum) }
  count <- count+1
  
  flag <- if(sp %in% unresolved) "unresolved_low_confidence_but_retained" else "standard"
  source_rows[[length(source_rows)+1]] <- c(sp, "original", raster_file, proj_type, flag, "yes", 
    if(sp %in% unresolved) "all CV models TSS<=0; retained for taxonomic completeness" else "")
}

cat(sprintf("\nFinal: %d ok, %d fail\n", count, fail))
r_mean <- r_sum/count
output_file <- file.path(out_dir, "hotspot_mean_suitability_strict_rescue_813.tif")
terra::writeRaster(r_mean, output_file, overwrite=TRUE)
cat("Output:", output_file, "\n")
mm <- minmax(r_mean); gm <- global(r_mean,"mean",na.rm=TRUE)
cat(sprintf("Min:%.4f Max:%.4f Mean:%.4f\n", mm[1], mm[2], gm[1,1]))

src_csv <- file.path(out_dir, "strict_rescue_species_source_table.csv")
write.csv(data.frame(
  species=sapply(source_rows,`[`,1), source=sapply(source_rows,`[`,2),
  path=sapply(source_rows,`[`,3), algorithm=sapply(source_rows,`[`,4),
  quality_flag=sapply(source_rows,`[`,5), included=sapply(source_rows,`[`,6),
  notes=sapply(source_rows,`[`,7)
), src_csv, row.names=FALSE)
cat("Source table:", src_csv, "\n")
cat("Done:", format(Sys.time()), "\n")
