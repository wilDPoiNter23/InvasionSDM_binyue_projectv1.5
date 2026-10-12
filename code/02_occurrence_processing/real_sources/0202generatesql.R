# Original source: 0202generatesql.R
# Original source location: <LOCAL_CODE_ROOT>/0202generatesql.R
# Original source SHA-256: 30bf0ba51d31f4b43e2eda95a2dee68ad100212e1094f5bc3e5b7a10eac7599d
# Public-safe original snapshot: archive/original_sources/local/0202generatesql.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
# Load necessary libraries
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
library(rgbif)
library(ggthemr)
setwd(publication_path("LOCAL_AUX_ROOT"))

temp_dir <- tempdir()
print(temp_dir)
cache_dir <- "cache_directory"

cache_file <- file.path(cache_dir, "cached_data.rds")

folder_path <- "speciesocc/135_279"

output_dir <- "speciesocc/output_135_279"

csv_dir <- "speciesocc/output_135_279/"

con <- dbConnect(duckdb::duckdb(), dbdir = "mydatabase.duckdb")


tables <- dbListTables(con)

dbDisconnect(base, shutdown = TRUE)
# List all zip files in the folder
zip_files <- list.files(folder_path, pattern = "\\.zip$", full.names = TRUE)


pblapply(zip_files, function(zip_file) {
  unzip(zip_file, exdir = output_dir)
})

csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

cleaned_data_list <- list()

pb <- progress_bar$new(
  format = "  cleaning [:bar] :percent in :elapsed",
  total = length(csv_files),
  clear = FALSE,
  width = 60
)
# read all the csv file
for (file in csv_files) {
  # using fread to read csv
  df <- fread(file, sep = "\t", stringsAsFactors = FALSE)
  
  # cleaning data and remove NA
  df_clean <- df %>%
    distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
   filter(!is.na(decimalLatitude))
  
  # save data in the list
  cleaned_data_list[[file]] <- df_clean
  
  pb$tick()
}
# combine all the list
combined_data <- rbindlist(cleaned_data_list)
# save list as rds file & this is list version
saveRDS(cleaned_data_list, 'cache_directory/cached_data134_279.rds')
# save list as rds file & this is dataframe version
saveRDS(combined_data, "data/134_279.rds")
#read rds
data1 <- readRDS('data/134_279.rds')
# write in the duckdb
dbWriteTable(con, "allocc_gbif", data1)
# append two data rds 
dbWriteTable(con, "allocc_gbif", data1, append = TRUE)

table_structure <- dbGetQuery(con, "PRAGMA table_info('allocc_gbif')")


query <- "SELECT species, decimalLatitude, decimalLongitude, family,speciesKey, taxonKey,	genus, countryCode, stateProvince, year
          FROM allocc_gbif WHERE decimalLatitude IS NOT NULL AND decimalLongitude IS NOT NULL"

family_geodatapoint <- dbGetQuery(con, query)



write.csv(family_geodatapoint,'data/family_geodatapoint.csv')


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
species_geodatapoint_cleaned <- rbindlist(results)
write.csv(species_geodatapoint,'data/species_geodatapoint_cleaned.csv')


######## count every countries R_iaps richness
richness_by_country <- dbGetQuery(con, "SELECT countryCode, COUNT(DISTINCT species) as species_richness FROM allocc_gbif GROUP BY countryCode")
richness_by_country

species_counts <- dbGetQuery(con, "SELECT speciesKey, COUNT(DISTINCT speciesKey) as species_counts FROM allocc_gbif GROUP BY speciesKey")
species_counts



# count every years species number
records_by_year <- dbGetQuery(con, "SELECT year, COUNT(*) as record_count FROM allocc_gbif GROUP BY year")
records_by_year <- records_by_year%>%
  filter(!is.na(year))%>%
  filter(year > 1900)

ggthemr('flat')
ggrecords_year <- ggplot(records_by_year, aes(x = year, y = record_count)) +
  geom_line(size = 1) +
  labs(title = "Record Count Over the Years",
       x = "Year",
       y = "Record Count")

ggsave("records_by_year.png", plot = ggrecords_year,  width = 10, height = 5.89, dpi = 300)
