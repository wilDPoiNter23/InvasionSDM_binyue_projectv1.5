# Original source: 0208.R
# Original source location: <LOCAL_CODE_ROOT>/0208.R
# Original source SHA-256: e9a8141c99c18697c313ea4463c0930a0c0d25640851ed1b88b9920bada1779c
# Public-safe original snapshot: archive/original_sources/local/0208.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# auth : kby
# time : 26/11/2024

library(DBI)
library(duckdb)
library(sf)
library(dplyr)
library(parallel)
library(foreach)
library(future.apply)

setwd("<HPC_INPUT_ROOT>/script/output/")
# LINK TO THE DUCKDB DATABASE
con <- dbConnect(duckdb::duckdb(), dbdir = "<HPC_INPUT_ROOT>/script/speciesocc_data.duckdb", read_only = FALSE)

# INQUIRE NEEDED DATASET
data_query <- "SELECT decimalLongitude, decimalLatitude, species FROM speciesocc_data;"
data <- dbGetQuery(con, data_query)

# CLOSE THE DB LINE
dbDisconnect(con, shutdown = TRUE)

grid_resolution <- 0.1

# Split data by species
species_list <- unique(data$species)
num_chunks <- 80
species_chunks <- split(species_list, cut(seq_along(species_list), num_chunks, labels = FALSE))


# Calculate longitude and latitude ranges
longitude_min <- floor(min(data$decimalLongitude, na.rm = TRUE) / grid_resolution) * grid_resolution
longitude_max <- ceiling(max(data$decimalLongitude, na.rm = TRUE) / grid_resolution) * grid_resolution
latitude_min <- floor(min(data$decimalLatitude, na.rm = TRUE) / grid_resolution) * grid_resolution
latitude_max <- ceiling(max(data$decimalLatitude, na.rm = TRUE) / grid_resolution) * grid_resolution

# Set the output file path
output_csv <- 'species_stats_0.1degree.csv'

# Remove the output file if it already exists
if (file.exists(output_csv)) {
  file.remove(output_csv)
}

# Loop over each chunk
for (i in seq_along(species_chunks)) {
  # Get the species in the current chunk
  species_subset <- species_chunks[[i]]
  
  # Extract data for the current chunk
  data_subset <- data %>% filter(species %in% species_subset)
  
  # Calculate grid indices
  data_subset <- data_subset %>%
    mutate(
      col_index = floor((decimalLongitude - longitude_min) / grid_resolution),
      row_index = floor((decimalLatitude - latitude_min) / grid_resolution),
      grid_id = paste0("grid_", row_index, "_", col_index)
    ) %>%
    filter(!is.na(col_index), !is.na(row_index), !is.na(species))
  
  # Compute the number of unique grids for each species
  species_stats <- data_subset %>%
    group_by(species) %>%
    summarise(
      grid_count = n_distinct(grid_id),
      .groups = "drop"
    )
  
  # Write the results to the CSV file in append mode
  write.table(
    species_stats,
    file = output_csv,
    sep = ",",
    row.names = FALSE,
    col.names = ifelse(i == 1, TRUE, FALSE),  # Write column names only once
    append = TRUE
  )
  
  # Remove the data subset to free up memory
  rm(data_subset, species_stats)
  gc()
  
  # Optional: Print progress
  cat("Completed chunk", i, "of", length(species_chunks), "\n")
}
