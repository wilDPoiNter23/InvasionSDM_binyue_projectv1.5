# Marine raster alignment
# Input: Marine rasters and climate grid template
# Output: Aligned marine rasters

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(terra)
library(sf)

setwd(publication_path("HPC_INPUT_ROOT"))

# -------------------------------
# Configuration Parameters
# -------------------------------
# Input/Output paths
input_dir <- publication_path("HPC_INPUT_ROOT", "climate/SST_SSS2025_2.5mins")
output_dir <- publication_path("HPC_INPUT_ROOT", "climate/ocean")


r2_5m_template <- rast('climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif')

# -------------------------------
# File List to Process
# -------------------------------
target_files <- list.files(path = input_dir, pattern = ".tif$", full.names = TRUE)
# -------------------------------
# Logging Function
# Processing Function
# -------------------------------
process_raster <- function(input_path, output_path, template) {
  tryCatch({
    cat("\n--- Processing:", basename(input_path), "---\n")

    # Step 1: Load raster
    cat("1/5 Loading raster...")
    raw_raster <- rast(input_path)
    cat(" Done\n")

    # Step 2: Coordinate system alignment
    cat("2/5 Checking coordinate system...")
    if (!identical(crs(raw_raster), crs(template))) {
      cat(" Reprojecting...")
      projected <- project(raw_raster, template)
      rm(raw_raster)
      gc(full = TRUE)
    } else {
      projected <- raw_raster
      rm(raw_raster)
      gc(full = TRUE)
    }
    cat(" Done\n")

    # Step 3: Resolution resampling
    cat("3/5 Checking resolution...")
    if (!all(res(projected) == res(template))) {
      cat(" Resampling...")
      resampled <- resample(projected, template, method = "bilinear")
      rm(projected)
      gc(full = TRUE)
    } else {
      resampled <- projected
      rm(projected)
      gc(full = TRUE)
    }
    cat(" Done\n")

    # Step 4: Spatial extent alignment
    cat("4/5 Aligning extent...")
    cropped <- crop(resampled, template)
    final_raster <- extend(cropped, template, fill = NA)
    rm(resampled, cropped)
    gc(full = TRUE)
    cat(" Done\n")

    # Step 5: Save result
    cat("5/5 Writing output...")
    writeRaster(
      final_raster,
      filename = output_path,
      overwrite = TRUE,
      datatype = "FLT4S",
      NAflag = -9999
    )
    rm(final_raster)
    gc(full = TRUE)
    cat(" Done\n")

    cat("+++ Successfully processed", basename(input_path), "+++\n")

  }, error = function(e) {
    cat("\n!!! Error processing", basename(input_path), ":", conditionMessage(e), "\n")
  })
}

# -------------------------------
# Main Processing
# -------------------------------
cat("===== STARTING OCEAN DATA PROCESSING =====\n")
for (f in target_files) {
  output_path <- file.path(output_dir, gsub("\\.tif$", "_aligned.tif", basename(f)))
  process_raster(f, output_path, r2_5m_template)
}
cat("\n===== PROCESSING COMPLETED =====\n")
