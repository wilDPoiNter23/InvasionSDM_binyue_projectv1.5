library(terra)
library(pbapply)

# Set paths
proj_dir <- "E:/k_bc_data/r_code/biomod/projections"
rds_files <- list.files(proj_dir, pattern = "\\.rds$", full.names = TRUE)
rds_files
str(readRDS("E:/k_bc_data/r_code/biomod/projections/Veronica.persica_ensemble_projection.rds"))
proj_obj <- readRDS("E:/k_bc_data/r_code/biomod/projections/Veronica.persica_ensemble_projection.rds")


ras <- rast(proj_obj@proj.out@link)


rds_files <- list.files(proj_dir, pattern = "\\.rds$", full.names = TRUE)

# Initialize empty list and progress bar
raster_list <- list()
total <- length(rds_files)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in seq_along(rds_files)) {
  rds_file <- rds_files[i]
  message("🔄 Processing: ", basename(rds_file))
  
  proj_obj <- tryCatch(readRDS(rds_file), error = function(e) NULL)
  
  if (!is.null(proj_obj) && "BIOMOD.Projection" %in% class(proj_obj)) {
    raster_layers <- tryCatch({
      # proj@proj@val stores raster (note structure may differ)
      ras <- rast(proj_obj@proj@val)
      readAll(ras)  # 💡 Preload to memory
    }, error = function(e) {
      message("⚠️ Raster conversion failed: ", basename(rds_file))
      NULL
    })
    
    if (!is.null(raster_layers)) {
      sp_name <- gsub("\\.rds$", "", basename(rds_file))
      names(raster_layers) <- paste0(sp_name, "_", seq_len(nlyr(raster_layers)))
      raster_list[[sp_name]] <- raster_layers
    }
  } else {
    message("⚠️ Cannot read RDS or object structure abnormal: ", basename(rds_file))
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Merge all SpatRasters
if (length(raster_list) > 0) {
  all_species_stack <- do.call(c, raster_list)
  
  # Save as GeoTIFF raster stack
  output_file <- file.path(proj_dir, "all_species_projection_stack.tif")
  writeRaster(
    all_species_stack,
    output_file,
    overwrite = TRUE,
    datatype = "FLT4S"  # 32-bit float
  )
  
  message("✅ All species projections merged and saved as: ", output_file)
} else {
  message("❌ No projection rasters loaded successfully")
}