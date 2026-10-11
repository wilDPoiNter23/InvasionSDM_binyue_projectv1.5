# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 2501b734c4f91a340f39207c602c3a8c8a1c0a5e6cc459d8853a0f90e09e6b78
library(terra)
library(sf)

setwd('${HPC_INPUT_ROOT}/')

## build factors stack

env_dir <- 'climate/wc2.1_2.5m'
env_files <- list.files(path = env_dir, pattern = ".tif$", full.names = TRUE)

ocean_dir <- 'climate/ocean'
ocean_files <- list.files(path = ocean_dir, pattern = ".tif$", full.names = TRUE)

soil_dir <- 'climate/soilworld0-15cm_2.5mins'
soil_files <- list.files(path = soil_dir, pattern = ".tif$", full.names = TRUE)

topo_dir <- 'climate/Topo'
topo_files <- list.files(path = topo_dir, pattern = ".tif$", full.names = TRUE)

human_dir <- 'climate/InputLayer_SDM'
human_files <- list.files(path = human_dir, pattern = ".tif$", full.names = TRUE)







# 3. rast() to stack
env_stack   <- rast(env_files)
ocean_stack <- rast(ocean_files)
soil_stack  <- rast(soil_files)
topo_stack  <- rast(topo_files)
human_stack <- rast(human_files)

# change names for soil_stack
print(names(soil_stack))
orig    <- names(soil_stack)
prefix  <- sub("_.*$", "", orig) 
suffix  <- "_mean_2024_2_5min_aligned"
newnames <- paste0(prefix, suffix)
#  Assign back to the stack
names(soil_stack) <- newnames

#  Verify
print(names(soil_stack))

# change names for human_stack
print(names(human_stack))
new_names <- names(human_stack)
# overwrite only layers 2 through 5
new_names[2:5] <- c(
  "GDP2020_2.5min",
  "Pop2020_2.5min",
  "reclamation_raster",
  "VIIRS_2023_2.5min"
)

#  assign them back
names(human_stack) <- new_names

#  check
print(names(human_stack))

full_stack <- c(
  env_stack,
  soil_stack,
  human_stack,
  topo_stack,
  ocean_stack
)
cat("Final stack extent:", as.character(ext(full_stack)), "\n")
cat("Resolution:", res(full_stack[[1]]), "\n")
cat("Number of layers:", nlyr(full_stack))

print(names(full_stack))

writeRaster(
  full_stack,
  filename = "climate/full_stack_final.tif",  # Output filename
  overwrite = TRUE,             # Overwrite existing file
  datatype = "FLT4S"        # Data type (Float32)

)
