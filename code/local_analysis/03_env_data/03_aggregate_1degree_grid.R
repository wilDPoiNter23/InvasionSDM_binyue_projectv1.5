library(DBI)
library(duckdb)
library(sf)
library(dplyr)
library(foreach)
library(pbapply)
library(future.apply)
setwd("/thfs1/home/qs_songsj1/data/script/output/")
# LINK TO THE DUCKDB DATABASE
con <- dbConnect(duckdb::duckdb(), dbdir = "/thfs1/home/qs_songsj1/data/script/speciesocc_data.duckdb", read_only = FALSE)

# INQUIRE NEEDED DATASET
species_list_query <- "SELECT DISTINCT species FROM speciesocc_data;"
species_list <- dbGetQuery(con, species_list_query)$species

set.seed(123)
sampled_species <- sample(species_list, size = 10, replace = FALSE)

species_str <- paste0("'", sampled_species, "'", collapse = ", ")
data_query <- paste0("SELECT decimalLongitude, decimalLatitude, species FROM speciesocc_data WHERE species IN (", species_str, ");")
data <- dbGetQuery(con, data_query)
# CLOSE THE DB LINE
dbDisconnect(con, shutdown = TRUE)

grid_resolution <- 1

# Split data by species
data_split_species <- split(data, data$species)
# Calculate longitude and latitude ranges
longitude_min <- floor(min(data$decimalLongitude, na.rm = TRUE) / grid_resolution) * grid_resolution
longitude_max <- ceiling(max(data$decimalLongitude, na.rm = TRUE) / grid_resolution) * grid_resolution
latitude_min <- floor(min(data$decimalLatitude, na.rm = TRUE) / grid_resolution) * grid_resolution
latitude_max <- ceiling(max(data$decimalLatitude, na.rm = TRUE) / grid_resolution) * grid_resolution

library(future)
plan(multicore, workers = parallel::detectCores() - 1)

# Function to process each species
process_species <- function(species_data) {
  species_data <- species_data %>%
    mutate(
      col_index = floor((decimalLongitude - longitude_min) / grid_resolution),
      row_index = floor((decimalLatitude - latitude_min) / grid_resolution),
      grid_id = paste0("grid_", row_index, "_", col_index)
    ) %>%
    filter(!is.na(col_index), !is.na(row_index))
    
  grid_count <- n_distinct(species_data$grid_id)
  
  data.frame(species = unique(species_data$species), grid_count = grid_count)
}

# Use future_lapply to process in parallel
species_stats_list <- future_lapply(data_split_species, process_species)

# Combine results
species_stats <- do.call(rbind, species_stats_list)

# Save the results
write.csv(species_stats, "/thfs1/home/qs_songsj1/data/script/output/2511nonparell_species_stats_1degree.csv", row.names = FALSE)