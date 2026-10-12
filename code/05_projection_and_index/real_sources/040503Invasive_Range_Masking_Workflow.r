# Original source: 040503Invasive_Range_Masking_Workflow.r
# Original source location: <LOCAL_CODE_ROOT>/040503Invasive_Range_Masking_Workflow.r
# Original source SHA-256: 9c823aa41a53c7e3924abeb2551512dc2d3a27f888a03eaed9594647453c603f
# Public-safe original snapshot: archive/original_sources/local/040503Invasive_Range_Masking_Workflow.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
# Invasive Range Masking Workflow
# Purpose: Clip SDM predictions to known invasive ranges using WCVP data
library(sf)
library(terra) # using terra instead of raster for better performance
library(dplyr)
# Ensure this is installed: devtools::install_github("matildabrown/rWCVPdata")
devtools::install_github("matildabrown/rWCVPdata")
install.packages("rWCVPdata")
library(rWCVPdata)
# 1. Load Data
# Invasive range table (created in previous steps)
native_data <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/native_invasion_region/species_native_ranges.csv"))
invasive_data <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/native_invasion_region/species_invasive_ranges.csv"))
# Load WCVP map (Level 3)
# data(wgsrpd3, package = "rWCVPdata")
# wgsrpd3_sf <- st_as_sf(wgsrpd3)
native_summary <- native_data %>%
  filter(!is.na(continent)) %>%
  group_by(species_name) %>%
  summarise(
    native_continents = paste(unique(continent), collapse = ", "),
    native_regions = paste(unique(area), collapse = ", "),
    .groups = "drop"
  )
# summary invasive area
invasive_summary <- invasive_data %>%
  filter(!is.na(continent)) %>%
  group_by(species_name) %>%
  summarise(
    invasive_continents = paste(unique(continent), collapse = ", "),
    invasive_regions = paste(unique(area), collapse = ", "),
    .groups = "drop"
  )
dim(native_summary)
dim(invasive_summary)
species_table <- native_summary %>%
  full_join(invasive_summary, by = "species_name") %>%
  arrange(species_name)
write.csv(species_table, publication_path("LOCAL_ANALYSIS_ROOT", "data/native_invasion_region/species_native_invasive_ranges.csv"))
# compare with accepted list
accepted_list <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/00_accepeted_species_list.csv"))
all_species <- accepted_list$species
table_species <- species_table$species_name
# find the difference
missing_in_table <- setdiff(all_species, table_species)
missing_in_accepted <- setdiff(table_species, all_species)
missing_in_table
missing_in_accepted
cat("总物种数 (accepted list):", length(all_species), "\n")
cat("表格中的物种数:", length(table_species), "\n")
cat("缺失物种数:", length(missing_in_table), "\n\n")
cat("缺失的物种:\n")
print(missing_in_table)
# 2. Define SDM Folder (Placeholder - User needs to confirm path)
# UPDATE PATH: Correct folder where your SDM .tif files are stored
sdm_folder <- publication_path("LOCAL_ANALYSIS_ROOT", "output/current_projection")
output_folder <- publication_path("LOCAL_ANALYSIS_ROOT", "output/invasive_masked")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
# 3. Loop through species
species_list <- unique(invasive_data$species_name)
for (sp in species_list) {
  cat("\nProcessing:", sp)

  # Clean species name for file matching (spaces to underscores usually)
  sp_clean <- gsub(" ", "_", sp)

  # A. Load SDM Raster for this species
  # Note: Adjust filename pattern as needed to match your SDM output
  # Example pattern: "Genus_species.*.tif"
  raster_files <- list.files(sdm_folder, pattern = paste0(sp_clean, ".*.tif$"), full.names = TRUE)

  if (length(raster_files) == 0) {
    cat(" - No SDM raster found (skipping)")
    next
  }

  # Load the first matching raster (e.g., mean projection)
  sdm_raster <- rast(raster_files[1])

  # B. Get Invasive Range Polygons
  # Filter invasive data for this species
  sp_range_data <- invasive_data %>% filter(species_name == sp)

  # Get TDWG Level 3 codes (area column)
  sp_range_codes <- sp_range_data$area

  if (length(sp_range_codes) == 0) {
    cat(" - No invasive range codes found (skipping)")
    next
  }

  # Filter map polygons
  # sp_invasive_poly <- wgsrpd3_sf %>%
  #   filter(LEVEL3_COD %in% sp_range_codes)

  # if(nrow(sp_invasive_poly) == 0) {
  #   cat(" - No matching polygons found for codes:", paste(sp_range_codes, collapse=","))
  #   next
  # }

  # C. Masking
  # Ensure CRS matches
  # if(st_crs(sp_invasive_poly) != crs(sdm_raster)) {
  #   sp_invasive_poly <- st_transform(sp_invasive_poly, crs(sdm_raster))
  # }

  # Rasterize polygon to mask (1 for invasive, NA for others)
  # invasive_mask <- rasterize(vec(sp_invasive_poly), sdm_raster, field = 1, background = NA)

  # Apply mask: SDM value * mask (keeps only values inside polygon)
  # masked_sdm <- mask(sdm_raster, invasive_mask)

  # D. Save Output
  out_name <- file.path(output_folder, paste0(sp_clean, "_invasive_masked.tif"))
  # writeRaster(masked_sdm, out_name, overwrite = TRUE)

  cat(" - Saved masked raster:", out_name)
}
