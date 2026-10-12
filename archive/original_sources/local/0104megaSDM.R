# Original source: 0104megaSDM.R
# Original source location: <LOCAL_CODE_ROOT>/0104megaSDM.R
# Original source SHA-256: ff6d143a85dbe55381632a88f2b7900294482e4a8a9bc5897996bceb75b0c689
# Public-safe original snapshot: archive/original_sources/local/0104megaSDM.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(megaSDM)
library(rgbif)
library(BIEN)
env_files <- list.files(path = "<LOCAL_ANALYSIS_ROOT>/data/data/climate/wc2.1_2.5m", pattern = ".tif$", full.names = TRUE)
env_stack <- raster::stack(env_files)
species_data <- read.csv("<LOCAL_ANALYSIS_ROOT>/data/Spartina_alterniflora_gbif.csv")
model_list <- TrainStudyEnv(occurrence_data = species_data, 
                       env_data = env_stack, 
                       method = "Maxent", 
                       output_dir = "<LOCAL_ANALYSIS_ROOT>/output")


IAPlist<- in_taxa_count[1]

IAPlist[[1]]

gbif_data <- lapply(IAPlist[[1]], function(species) {
  occ_search(scientificName = species, limit = 5000) # Adjust the limit as necessary
})

gbif_occurrences <- do.call(rbind, lapply(gbif_data, function(x) x$data))