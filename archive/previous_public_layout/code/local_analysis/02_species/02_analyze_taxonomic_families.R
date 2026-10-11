# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 238683ff638eb4e9bd2dcd964f1ae411f37aedfa23328c3684ac633ba6ee5f7a
library(purrr)
library(duckdb)
library(dplyr)
library(arrow)
library(zip)
library(pbapply)
library(progress)
library(data.table)
library(parallel)
library(sf)
library(ggplot2)
library(CoordinateCleaner)
# library(rgbif)
#library(ggthemr)

setwd('${HPC_INPUT_ROOT}/script')
key_list <- read.csv('${HPC_INPUT_ROOT}/species_keys.csv')
key_list <- key_list$usageKeys
con <- dbConnect(duckdb::duckdb(), dbdir = "mydatabase.duckdb")

query <- "SELECT species, decimalLatitude, decimalLongitude, family,speciesKey, taxonKey,	genus, countryCode, stateProvince, year
          FROM allocc_gbif WHERE decimalLatitude IS NOT NULL AND decimalLongitude IS NOT NULL"



family_geodatapoint <- dbGetQuery(con, query)

family_geodatapoint <- family_geodatapoint %>% filter(speciesKey %in% usageKey_list)

dbDisconnect(con, shutdown = TRUE)


total_rows <- nrow(family_geodatapoint)
chunk_size <- 1e6  # Process 1 million rows per chunk
num_chunks <- ceiling(total_rows / chunk_size)

# clean coordinate
results <- vector("list", num_chunks)

for (i in 1:num_chunks) {
  
  start_row <- ((i - 1) * chunk_size) + 1
  end_row <- min(i * chunk_size, total_rows)
  
  # extract every chunk
  data_chunk <- family_geodatapoint[start_row:end_row, ]
  
  # clean every chunk
  cleaned_chunk <- clean_coordinates(
    x = data_chunk,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "species",
    tests = c("centroids", "equal", "gbif", "institutions", "zeros"),  
    value = "clean"
  )
  
  # Store cleaned data
  results[[i]] <- cleaned_chunk
  
  # Optional: clean memory
  rm(data_chunk, cleaned_chunk)
  gc()
}
# Merge all cleaned data
family_geodatapoint_cleaned <- rbindlist(results)
write.csv(species_geodatapoint,'output/family_geodatapoint_cleaned.csv')
