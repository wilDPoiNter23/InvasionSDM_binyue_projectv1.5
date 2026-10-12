# Original source: 0204linux_family_analysis.R
# Original source location: <LOCAL_CODE_ROOT>/0204linux_family_analysis.R
# Original source SHA-256: e0e89df4870ea8adce2c3c614f0273fbb045f4641294ad7eaa7747af676a18cf
# Public-safe original snapshot: archive/original_sources/local/0204linux_family_analysis.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
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

setwd("<HPC_INPUT_ROOT>/script")
key_list <- read.csv("<HPC_INPUT_ROOT>/species_keys.csv")
key_list <- key_list$usageKeys
con <- dbConnect(duckdb::duckdb(), dbdir = "mydatabase.duckdb")

query <- "SELECT species, decimalLatitude, decimalLongitude, family,speciesKey, taxonKey,	genus, countryCode, stateProvince, year
          FROM allocc_gbif WHERE decimalLatitude IS NOT NULL AND decimalLongitude IS NOT NULL"



family_geodatapoint <- dbGetQuery(con, query)

family_geodatapoint <- family_geodatapoint %>% filter(speciesKey %in% usageKey_list)

dbDisconnect(con, shutdown = TRUE)


total_rows <- nrow(family_geodatapoint)
chunk_size <- 1e6  # 每块处理100万行
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
  
  # 存储清理后的数据
  results[[i]] <- cleaned_chunk
  
  # 可选：清理内存
  rm(data_chunk, cleaned_chunk)
  gc()
}
# 合并所有清理后的数据
family_geodatapoint_cleaned <- rbindlist(results)
write.csv(species_geodatapoint,'output/family_geodatapoint_cleaned.csv')
