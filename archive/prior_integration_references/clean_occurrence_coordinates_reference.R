# Publication integration: clean_occurrence_coordinates_reference.R
# Role: Historical occurrence cleaning reference; result-sensitive defects preserved
# Inputs: GBIF ZIP downloads; DuckDB; undefined base and output objects
# Outputs: CoordinateCleaner records and occurrence tables; writer unresolved
# Source SHA256: 585a21042342e12ff707dde6a217396076e0ada6a41e9a8266a67e91bc3086b9
# Scope: implementation reference; submitted-use certainty is in metadata.
# Scientific expressions/settings retained. Only paths and setup were cleaned.
# Load config/bootstrap.R before sourcing, or use code/run_stage.R.
if (!exists("publication_path", mode = "function")) stop("Load config/bootstrap.R first")
publication_require_workspace(c("base", "species_geodatapoint"))
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
setwd(publication_path("OCCURRENCE_ROOT"))

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
