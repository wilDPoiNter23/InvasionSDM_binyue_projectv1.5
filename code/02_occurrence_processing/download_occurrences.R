# Occurrence download
# Input: Accepted species list and private GBIF credentials
# Output: GBIF download keys and occurrence files

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(rgbif)
library(dplyr)
library(parallel)
library(pbapply)
library(usethis)
library(CoordinateCleaner)
library(ggplot2)
library(geosphere)
download_dir <- publication_path("GBIF_DOWNLOADS")
dir.create(download_dir, recursive=TRUE, showWarnings=FALSE)
setwd(download_dir)

species_list <- read.csv(publication_path("SPECIES_LIST"), check.names=FALSE) %>% as.list()

# search for key
species_key_map <- species_list[[1]]%>%
  name_backbone_checklist() %>%
  filter(!matchType == "NONE") %>%  # Exclude unmatched names
  select(scientificName, usageKey) %>%  # Select relevant columns
  group_by(scientificName) %>%  # Group by species name
  summarize(usageKeys = paste(usageKey, collapse = ", "))  # Collapse multiple keys into a single string

write.csv(species_key_map, "species_keys.csv", row.names=FALSE)
taxon_keys <- unique(as.integer(trimws(unlist(strsplit(species_key_map$usageKeys, ",")))))
taxon_keys <- taxon_keys[!is.na(taxon_keys)]
if (!length(taxon_keys)) stop("No GBIF taxon keys matched the species list.")
species_batches <- split(taxon_keys, ceiling(seq_along(taxon_keys) / 3))
half_batch <- species_batches

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

download_keys <- unlist(download_keys, use.names=FALSE)
write.csv(data.frame(download_key=download_keys), "download_keys.csv", row.names=FALSE)
if (!length(download_keys)) stop("No GBIF download requests succeeded.")
for (key in download_keys) occ_download_wait(key)
# Function to download and import data, with retry logic
download_with_retry <- function(keys, sleep_time = 10) {
  download_data <- vector("list", length(keys))
  pb <- txtProgressBar(min = 0, max = length(keys), style = 3)

  for (i in seq_along(keys)) {
    attempt <- 1
    while (attempt <= 5) {
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
    if (attempt > 5) stop("Download failed after five attempts: ", keys[[i]])
  }
  close(pb)
  return(do.call(rbind, download_data))
}

combined_data <- download_with_retry(download_keys)
