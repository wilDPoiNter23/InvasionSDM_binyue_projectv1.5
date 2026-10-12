# Multispecies invasion suitability
# Input: Species projections and projection summary
# Output: Normalized mean suitability raster

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(terra)
library(openxlsx)

# Path settings.
base_dir <- publication_path("WORK_ROOT")
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_mean <- file.path(base_dir, "results/hotspot_mean_suitability_normalized.tif")
log_file <- file.path(base_dir, "results/stack_failed_species_log_normalized.xlsx")

# Read the summary table.
summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

# Initialise objects.
fail_log <- data.frame(species = character(), reason = character())
r_sum <- NULL
count <- 0
normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]
  rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) {
    return(r * 0)  # Handle exceptional cases.
  }
  return((r - rmin) / (rmax - rmin))
}


for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  cat(sprintf("[%03d/%03d] Processing %s (%s)\n", i, nrow(summary_df), sp, proj_type))

  # RDS path.
  rds_path <- file.path(proj_dir, paste0(sp, "_", proj_type, "_projection.rds"))
  rds <- tryCatch(readRDS(rds_path), error = function(e) NULL)

  if (is.null(rds) || !inherits(rds, "BIOMOD.projection.out")) {
    cat("❌ Failed:", sp, "(Invalid RDS)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Invalid RDS"))
    next
  }

  model_name <- rds@models.projected[1]
  search_dir <- file.path(model_dir, sp_dot)

  # Recursively find matching .tif or .grd files.
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

  # Min-max normalisation.
  r <- normalize_raster(r)

  # Convert all rasters to double precision.
r <- terra::ifel(is.na(r), 0, r)

if (is.null(r_sum)) {
  r_sum <- r
} else {
  tryCatch({
    r_sum <- terra::cover(r_sum + r, r_sum)  # Accumulate values while handling NA values.
  }, error = function(e) {
    cat("❌ Failed:", sp, "(Dimension mismatch)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Dimension mismatch"))
  })
}

  count <- count + 1
  cat("✅ Success:", sp, "\n")
}

# Write the mean raster layer.
if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, output_mean, overwrite = TRUE)
  cat("✅ Mean suitability map saved to:", output_mean, "\n")
} else {
  cat("⚠️ No valid rasters to average.\n")
}

# Write the failure log.
if (nrow(fail_log) > 0) {
  openxlsx::write.xlsx(fail_log, file = log_file, overwrite = TRUE)
  cat("⚠️ Failures logged to:", log_file, "\n")
}
