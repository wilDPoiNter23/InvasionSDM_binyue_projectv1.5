# Original source: 0201downloadgbif.R
# Original source location: <LOCAL_CODE_ROOT>/0201downloadgbif.R
# Original source SHA-256: b0ef91c25803c29c3a21e139cc6f523454e2701328c005ab28c005653b9d53d4
# Public-safe original snapshot: archive/original_sources/local/0201downloadgbif.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(rgbif)
library(dplyr)
library(parallel)
library(pbapply)
library(usethis)
library(CoordinateCleaner)
library(ggplot2)
library(geosphere)
getwd()
setwd("<LOCAL_AUX_ROOT>/speciesocc")
usethis::edit_r_environ()

species_list <- read.csv('data/taxa_accepted.csv') %>% as.list()

# search for key
species_key_map <- species_list[[1]]%>% 
  name_backbone_checklist() %>%
  filter(!matchType == "NONE") %>%  # Exclude unmatched names
  select(scientificName, usageKey) %>%  # Select relevant columns
  group_by(scientificName) %>%  # Group by species name
  summarize(usageKeys = paste(usageKey, collapse = ", "))  # Collapse multiple keys into a single string

write.csv(species_key_map,'data/species_keys.csv')
########start here
key_list <- read.csv('data/species_keys.csv')


species_batches <- split(key_list, ceiling(seq_along(key_list) / 3)) # Splitting into batches of 100
half_batch <- species_batches[1:134]


download_keys <- list()


for (i in seq_along(half_batch)) {
  batch <- half_batch[[i]]
  success <- FALSE
  attempt <- 1
  
  while (!success && attempt <= 5) {  # Retry up to 5 times
    tryCatch({
      print(paste("Processing batch", i, "of", length(species_batches), "- Attempt", attempt))
      
      key <- occ_download(
        pred_in("taxonKey", batch),
        format = "SIMPLE_CSV",
        curlopts = list(timeout = 120)
      )
      
      download_keys <- append(download_keys, key)
      success <- TRUE  # Mark success if no error occurs
    }, error = function(e) {
      message("Error occurred during download of batch ", i, ": ", e$message)
      attempt <- attempt + 1
      Sys.sleep(5)  # Wait 5 seconds before retrying
    })
  }
  
  if (!success) {
    message("Failed to download batch ", i, " after 5 attempts.")
  }
}

# Create a directory to store the downloaded data
dir.create("speciesocc")


keyall <- occ_download_list(limit = 500)
download_keys <- keyall$results$key
download_keys <- download_keys[134:1]
write.csv(download_keys,'data/download_keys.csv')
download_keys <- read.csv("<LOCAL_AUX_ROOT>/data/download_keys.csv") 
download_keys <-download_keys[[1]]
# Function to download and import data, with retry logic
download_with_retry <- function(keys, sleep_time = 10) {
  download_data <- vector("list", length(keys))
  pb <- txtProgressBar(min = 0, max = length(keys), style = 3)
  
  for (i in seq_along(keys)) {
    attempt <- 1
    repeat {
      try({
        download_data[[i]] <- occ_download_get(keys[[i]],overwrite = TRUE,curlopts = list(timeout = 120))%>%
        occ_download_import()
        setTxtProgressBar(pb, i)
        break
      }, silent = TRUE)
      cat("Error occurred during download, retrying in", sleep_time, "seconds... (attempt", attempt, ")\n")
      Sys.sleep(sleep_time)
      attempt <- attempt + 1
    }
  }
  close(pb)
  return(do.call(rbind, download_data))
}

combined_data <- download_with_retry(download_keys[64:134])

