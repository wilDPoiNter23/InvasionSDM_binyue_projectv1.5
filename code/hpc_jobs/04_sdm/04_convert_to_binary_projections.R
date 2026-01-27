library(terra)
library(biomod2)
library(stringr)

base_dir <- "/thfs1/home/qs_songsj1/invasionSDM_binyue"
proj_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
model_dir <- file.path(base_dir, "models")

# Read fallback model information
proj_df <- read.csv(proj_csv)
fallback_species <- proj_df[proj_df$type == "fallback", "species"]
species_dot_names <- gsub(" ", ".", fallback_species)

# Process each fallback species
for (i in seq_along(fallback_species)) {
  species <- fallback_species[i]
  species_dir <- file.path(model_dir, species_dot_names[i])
  proj_dir <- file.path(species_dir, "proj_fallback_projection", "individual_projections")

  # Find unique .tif prediction file
  tif_file <- list.files(proj_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_file) != 1) {
    cat("Skipping species", species, "- found", length(tif_file), "prediction files\n")
    next
  }

  # Retrieve model object
  model_rds_path <- file.path(model_dir, species_dot_names[i], paste0(species_dot_names[i], ".biomod2.models.out"))
  if (!file.exists(model_rds_path)) {
    cat("Skipping", species, "- cannot find biomod2 model object\n")
    next
  }

  model_obj <- readRDS(model_rds_path)

  # Get best model name (tif filename contains model name)
  tif_basename <- basename(tif_file)
  model_match <- str_extract(tif_basename, paste(get_built_models(model_obj), collapse = "|"))
  if (is.na(model_match)) {
    cat("Skipping", species, "- cannot identify model name in tif file\n")
    next
  }

  # Get TSS cutoff for corresponding model
  evals <- get_evaluations(model_obj)
  tss_cutoff <- evals["TSS", "Testing.data", model_match]
  if (is.na(tss_cutoff)) {
    cat("Skipping", species, "- cannot find model TSS cutoff\n")
    next
  }

  # Generate binary raster
  pred_raster <- rast(tif_file)
  binary_raster <- pred_raster > tss_cutoff
  bin_path <- sub("\\.tif$", "_bin.tif", tif_file)
  writeRaster(binary_raster, bin_path, overwrite = TRUE)
  cat("Completed fallback binary prediction:", bin_path, "\n")
}
