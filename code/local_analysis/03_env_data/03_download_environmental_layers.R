setwd('E:/k_PhD_NKU/202407biomod/data')
library(devtools)
library(pbapply)
library(geodata)
library(terra)
library(sf)
pboptions(type = "timer")

# Download soil data
target_vars <- c(
  "bdod",     # Soil bulk density
  "cfvo",     # Coarse fragments volume
  "clay",     # Clay content
  "nitrogen", # Nitrogen content
  "ocd",      # Organic carbon density
  "phh2o",    # pH value
  "sand",     # Sand content
  "silt",     # Silt content
  "soc"       # Soil organic carbon
)
output_dir <- "data/"

download_soil_fill <- function(var) {
  tryCatch({
    # Download data (default includes filled mean values)
    soil_data <- soil_world(
      var = var,          # Soil variable
      depth = 15,   # Specified depth
      path = output_dir,  # Save path
      download = TRUE     # Force download latest data
    )
    
    # Check if file was generated
    expected_file <- file.path(output_dir, paste0(var, "_5-15cm_fill.tif"))
    if (file.exists(expected_file)) {
      message("Successfully downloaded: ", var)
      return(TRUE)
    } else {
      message("File not generated: ", var)
      return(FALSE)
    }
  }, error = function(e) {
    message("Download failed: ", var, " - ", e$message)
    return(FALSE)
  })
}

results <- sapply(target_vars, download_soil_fill)

# Print summary
cat("\nDownload complete! Successfully downloaded files:", sum(results), "/", length(target_vars))



# Download climate data
climate_vars <- c('tmin', 'tmax', 'prec')
climate_resolutions <- c(2.5)

worldclim_data <- pblapply(climate_vars, function(var) {
  pblapply(climate_resolutions, function(res) {
    getData('worldclim', var = var, res = res, path = 'data/')
  })
})

elev <- rast("data/Topo/wc2.1_2.5m_elev.tif")
aspect <- terrain(elev, v = "aspect", unit = "degrees")
writeRaster(aspect, "data/Topo/aspect.tif", overwrite = TRUE)
slope <- terrain(elev, v = "slope", unit = "degrees")
writeRaster(slope, "data/Topo/slope.tif", overwrite = TRUE)
ext(aspect)
res(elev)
elev_global <- elevation_global(res = 2.5, path = "data/Topo")
aspect <- terrain(elev_global, v = "aspect", unit = "degrees")



# resample population data from 30 arc seconds to 2.5 mins
r30s <- rast('data/humanbeing/buildup2025/BUILT_S_E2025_30ss.tif')
r2_5m_template <- rast('data/Topo/wc2.1_2.5m_elev.tif')

# Resample using bilinear interpolation (for continuous data)
r2_5m_resampled <- resample(r30s, r2_5m_template, method = "bilinear")
print(names(r2_5m_resampled))
 res(r2_5m_resampled)
names(r2_5m_resampled) <- "BUILT_S_E2025_2.5min"
writeRaster(r2_5m_resampled, "data/humanbeing/buildup2025/BUILT_S_E2025_2.5min.tif", overwrite = TRUE)

# Set paths
input_dir <- "data/soil_world"  # Replace with input directory
output_dir <- "data/soilworld0-15cm_2.5mins"  # Replace with output directory

depth1_files <- list.files(
  path = input_dir,
  pattern = "_0-5cm_mean_30s\\.tif$",
  full.names = TRUE
)
var_names <- gsub("_0-5cm_mean_30s\\.tif$", "", basename(depth1_files))

for (var in var_names) {
  # Build paths to two depth layers
  depth1_file <- file.path(input_dir, paste0(var, "_0-5cm_mean_30s.tif"))
  depth2_file <- file.path(input_dir, paste0(var, "_5-15cm_mean_30s.tif"))
  
  # Check if files exist
  if (!all(file.exists(c(depth1_file, depth2_file)))) {
    message("Skipping ", var, ": missing depth layer files")
    next
  } # Read raster data
  depth1 <- rast(depth1_file)
  depth2 <- rast(depth2_file)
  
  # Calculate 0-15cm mean (simple average)
  combined <- mean(depth1, depth2)
  
  # Resample to 2.5 minute resolution (bilinear interpolation)

  resampled <- resample(combined, r2_5m_template, method = "bilinear")
  
  # Build output filename
  output_file <- file.path(
    output_dir,
    paste0(var, "_0-15cm_mean_2.5mins.tif")
  )
  
  # Save results
  writeRaster(
    resampled,
    filename = output_file,
    overwrite = TRUE,
    datatype = "FLT4S" )
  
  message("Processed: ", var)
}

message("All processing complete! Output directory: ", output_dir)


## build factors stack

env_dir <- 'data/climate/wc2.1_2.5m'
env_files <- list.files(path = env_dir, pattern = ".tif$", full.names = TRUE)

ocean_dir <- 'data/climate/ocean'
ocean_files <- list.files(path = ocean_dir, pattern = ".tif$", full.names = TRUE)

soil_dir <- 'data/soilworld0-15cm_2.5mins'
soil_files <- list.files(path = soil_dir, pattern = ".tif$", full.names = TRUE)

topo_dir <- 'data/TOPO'
topo_files <- list.files(path = topo_dir, pattern = ".tif$", full.names = TRUE)

human_dir <- 'data/humanbeing/InputLayer_SDM'
human_files <- list.files(path = human_dir, pattern = ".tif$", full.names = TRUE)


# Define safe raster loading function (corrected loop logic)
safe_stack <- function(file_paths) {
  if (length(file_paths) == 0) return(NULL)  # Skip empty directories
  
  # Read rasters
  rst <- rast(file_paths)
  
  # Check CRS consistency
  ref_crs <- crs(rst)
  for (i in 1:nlyr(rst)) {  # Check each layer by index
    if (!identical(crs(rst[[i]]), ref_crs)) {
      stop("CRS inconsistency: Layer ", i, " (", names(rst[[i]]), ") vs reference (", ref_crs, ")")
    }
  }
  
  # Check resolution consistency
  ref_res <- res(rst)
  tolerance <- 1e-5
  for (i in 1:nlyr(rst)) {
    layer_res <- res(rst[[i]])
    if (any(abs(layer_res - ref_res) > tolerance)) {
      warning("Resolution inconsistency: Layer ", i, " (", names(rst[[i]]), ") resolution: ", paste(layer_res, collapse = ", "))
    }
  }
  
  return(rst)
}
# Load datasets step by step
env_stack <- safe_stack(env_files)
ocean_stack <- safe_stack(ocean_files)
soil_stack <- safe_stack(soil_files)
topo_stack <- safe_stack(topo_files)
human_stack <- rast(human_files)

print(names(human_stack))
# Print all raster extents and resolutions
check_extents <- function(files) {
  for (f in files) {
    r <- rast(f)
    cat("File:", basename(f), "\n")
    cat("Extent:", as.character(ext(r)), "\n")
    cat("Resolution:", res(r), "\n")
    cat("CRS:", crs(r), "\n\n")
  }
}

# Check human activity data rasters
check_extents(ocean_files)

full_stack <- c(
  env_stack,
  soil_stack,
  human_stack,
  topo_stack,
  ocean_stack
)
cat("最终堆栈范围:", as.character(ext(full_stack)), "\n")
cat("分辨率:", res(full_stack[[1]]), "\n")
cat("层数:", nlyr(full_stack))

writeRaster(
  full_stack,
  filename = "full_stack.tif",  # Output filename
  overwrite = TRUE,             # Overwrite existing file
  datatype = "FLT4S"        # Data type (Float32)

)
