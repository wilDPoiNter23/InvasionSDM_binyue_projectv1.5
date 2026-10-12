# Original source: 0208linux_test.R
# Original source location: <LOCAL_CODE_ROOT>/0208linux_test.R
# Original source SHA-256: 99de4369c0847a6b66f5c6426492fd1654cf13ac2e1e7fdc5a14ef20d796621b
# Public-safe original snapshot: archive/original_sources/local/0208linux_test.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
# auth : Binyue
# time : 14/11/2024

library(DBI)
library(duckdb)
library(sf)
library(dplyr)
library(foreach)
library(pbapply)

setwd("<HPC_INPUT_ROOT>/script/output/")
# LINK TO THE DUCKDB DATABASE
con <- dbConnect(duckdb::duckdb(), dbdir = "<HPC_INPUT_ROOT>/script/speciesocc_data.duckdb", read_only = FALSE)

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

# Get the range of the position data
longitude_range <- range(data$decimalLongitude, na.rm = TRUE)
latitude_range <- range(data$decimalLatitude, na.rm = TRUE)

left_bottom_longitude <- longitude_range[1]
left_bottom_latitude <- latitude_range[1]
right_top_longitude <- longitude_range[2]
right_top_latitude <- latitude_range[2]

# Create a rectangle use sf function
vertex_mat <- matrix(
    c(
        left_bottom_longitude, left_bottom_latitude,
        left_bottom_longitude, right_top_latitude,
        right_top_longitude, right_top_latitude,
        right_top_longitude, left_bottom_latitude,
        left_bottom_longitude, left_bottom_latitude
    ),
    ncol = 2, byrow = TRUE
)

rectangle <- st_polygon(list(vertex_mat))
rectangle_sf <- st_sfc(rectangle, crs = 4326)

grid_resolution <- 0.1
# build the grid
grid <- st_make_grid(rectangle_sf, cellsize = c(grid_resolution, grid_resolution))

#convert grid to sf
grid_sf <- st_sf(geometry = grid)
grid_sf$id <- seq_len(length(grid_sf))

saveRDS(grid_sf, file = "grid_sf.rds")

grid <- st_transform(grid, crs = 4326)

data_sf <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Split data into multi data frame by species
data_split_species <- split(data_sf, data_sf$species)


# ============================= Process data concurrently ===============================



results_list <- pblapply(data_split_species, function(species_data) {
    tryCatch({
    library(sf)
    library(dplyr)

    grid_sf <- readRDS("grid_sf.rds")
    
   # make sure
    species_data <- st_transform(species_data, crs = st_crs(grid_sf))
    # Check each position in which cell
    data_in_grid <- st_join(species_data, grid_sf, join = st_within)

   # Ensure the join worked correctly and filter out NA results
    data_in_grid <- data_in_grid %>%
      filter(!is.na(id))

    # Count 
    species_grid_count <- data_in_grid %>%
        distinct(species, id) %>%
        group_by(species) %>%
        summarise(grid_count = n(),.groups = "drop")

    return(species_grid_count)
}, error = function(e) {
        message("Error processing species: ", unique(species_data$species))
        message("Error message: ", e$message)
        return(NULL)
    })
})
results_list <- Filter(Negate(is.null), results_list)
results <- do.call(rbind, results_list)
# Save the data in csv format
write.csv(results, "<HPC_INPUT_ROOT>/script/output/species_statstest.csv")
saveRDS(results,"<HPC_INPUT_ROOT>/script/output/species_statstest.rds")