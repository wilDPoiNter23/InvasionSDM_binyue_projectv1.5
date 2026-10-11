# Publication integration: build_multispecies_index.R
# Role: Historical index implementation reference
# Inputs: Projection summary; biomod projection/model objects and associated rasters
# Outputs: hotspot_mean_suitability_normalized.tif; failure ledger
# Source SHA256: e407cad2267fe34e5a62e10ef6aa9437ab0b36a084d010c507dccde5787a1d32
# Scope: implementation reference; submitted-use certainty is in metadata.
# Scientific expressions/settings retained. Only paths and setup were cleaned.
# Load config/bootstrap.R before sourcing, or use code/run_stage.R.
if (!exists("publication_path", mode = "function")) stop("Load config/bootstrap.R first")
library(terra)
library(openxlsx)

# Path settings
base_dir <- publication_path("WORK_ROOT")
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_mean <- file.path(base_dir, "results/hotspot_mean_suitability_normalized.tif")
log_file <- file.path(base_dir, "results/stack_failed_species_log_normalized.xlsx")

# Read summary table
summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

# Initialize
fail_log <- data.frame(species = character(), reason = character())
r_sum <- NULL
count <- 0
normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]
  rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) {
    return(r * 0)  # Avoid edge cases
  }
  return((r - rmin) / (rmax - rmin))
}


for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  cat(sprintf("[%03d/%03d] Processing %s (%s)\n", i, nrow(summary_df), sp, proj_type))

  # RDS path
  rds_path <- file.path(proj_dir, paste0(sp, "_", proj_type, "_projection.rds"))
  rds <- tryCatch(readRDS(rds_path), error = function(e) NULL)

  if (is.null(rds) || !inherits(rds, "BIOMOD.projection.out")) {
    cat("❌ Failed:", sp, "(Invalid RDS)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Invalid RDS"))
    next
  }

  model_name <- rds@models.projected[1]
  search_dir <- file.path(model_dir, sp_dot)

  # Recursively find matching .tif or .grd files
  tif_file <- list.files(
    search_dir,
    pattern = paste0(model_name, ".*\\.(tif|grd)$"),
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(tif_file) == 0) {
    cat("❌ Failed:", sp, "(Prediction raster not found)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Prediction raster not found"))
    next
  }

  r <- tryCatch(terra::rast(tif_file[1]), error = function(e) NULL)
  if (is.null(r)) {
    cat("❌ Failed:", sp, "(Raster read error)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Raster read error"))
    next
  }

  # Min-Max normalization
  r <- normalize_raster(r)
  
  # Convert all rasters to double type
r <- terra::ifel(is.na(r), 0, r)

if (is.null(r_sum)) {
  r_sum <- r
} else {
  tryCatch({
    r_sum <- terra::cover(r_sum + r, r_sum)  # NA-safe accumulation
  }, error = function(e) {
    cat("❌ Failed:", sp, "(Dimension mismatch)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Dimension mismatch"))
  })
}

  count <- count + 1
  cat("✅ Success:", sp, "\n")
}

# Write out mean layer
if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, output_mean, overwrite = TRUE)
  cat("✅ Mean suitability map saved to:", output_mean, "\n")
} else {
  cat("⚠️ No valid rasters to average.\n")
}

# Write out failure log
if (nrow(fail_log) > 0) {
  openxlsx::write.xlsx(fail_log, file = log_file, overwrite = TRUE)
  cat("⚠️ Failures logged to:", log_file, "\n")
}
