# Original source: 0106debug_finddiff.R
# Original source location: <LOCAL_CODE_ROOT>/0106debug_finddiff.R
# Original source SHA-256: 7559277de6b5de4f020544c075d72823003b6d85d5126bae9f29c6603cca70d5
# Public-safe original snapshot: archive/original_sources/local/0106debug_finddiff.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(data.table)
library(ggplot2)
library(sf)
library(parallel)
library(doParallel)
library(pbapply)

con <- dbConnect(duckdb::duckdb(), dbdir = publication_path("LOCAL_DATABASE_ROOT", "speciesocc_data.duckdb"))

dbExecute(con, "CREATE INDEX idx_species ON speciesocc_data(species);")


setwd(publication_path("HPC_INPUT_ROOT", "script"))
family_geodatapoint_cleaned <- read.csv(publication_path("HPC_INPUT_ROOT", "script/output/family_geodatapoint_cleaned.csv"))
key_list <- read.csv('data/species_keys.csv')
usageKey_list <- key_list$usageKeys
accepted_speices_list <- read.csv('data/taxa_accepted.csv') %>% as.list()
accepted_speices_list[[1]]
family_geodatapoint_filter <- family_geodatapoint %>% filter(speciesKey %in% usageKey_list)

species_counts <- family_geodatapoint_filter %>%
  group_by(species,speciesKey) %>%
  summarise(num_occurrences = n()) %>%
  arrange(desc(num_occurrences))

write.csv(species_counts,'data/species_counts.csv')
write.csv(family_geodatapoint_filter,'data/family_geodatapoint_filter.csv')

countryspecies_counts <- family_geodatapoint_filter %>%
  group_by( species,countryCode) %>%
  summarise(num_occurrences = n()) %>%
  arrange(desc(num_occurrences))
write.csv(countryspecies_counts,'data/countryspecies_counts.csv')



filterspecies_list <-  species_counts$species
filterkey_list <- species_counts$speciesKey
missing_species <- setdiff(filterspecies_list, accepted_speices_list[[1]])

missing_species <- setdiff(usageKey_list, filterkey_list )
common_species <- intersect(usageKey_list, filterkey_list)
filterspecies_list
species_list
usageKey_list
missing_species
class(usageKey_list)
class(family_geodatapoint$speciesKey)


result_1 <- read.csv(publication_path("HPC_INPUT_ROOT", "script/output/family_geodatapoint_filter.csv"))

glimpse(result)


