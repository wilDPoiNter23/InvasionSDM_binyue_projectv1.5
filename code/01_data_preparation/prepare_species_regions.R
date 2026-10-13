# Species and regional inventory
# Input: GloNAF tables and coastal city inventory
# Output: Species and region tables

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
packages <- c(
  "devtools", "duckdb", "rnaturalearth", "sf",
  "readr", "tidyverse", "dplyr", "GIFT", "ggplot2",
  "httr", "jsonlite", "pbapply", "maps", "svglite",
  "rnaturalearthdata", "RColorBrewer", "viridis", "ggspatial"
)
lapply(packages, library, character.only = TRUE)
library(tidyverse)



sf::sf_use_s2(FALSE)
duck_glonaf <- dbConnect(duckdb(),
  dbdir = publication_path("LOCAL_ANALYSIS_ROOT", "data/data/glonaf_wcvp.duckdb")
)

# using duckdb to query glonaf information
dbWriteTable(duck_glonaf, "list",
  read_csv(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/GloNAF_v_2_01/csv/list.csv")),
  overwrite = TRUE
)

dbWriteTable(duck_glonaf, "region",
  read_csv(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/GloNAF_v_2_01/csv/region.csv")),
  overwrite = TRUE
)

dbWriteTable(duck_glonaf, "flora2",
  read_csv(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/GloNAF_v_2_01/csv/flora2.csv"), locale = locale(encoding = "latin1")),
  overwrite = TRUE
)

dbWriteTable(duck_glonaf, "taxon",
  read_csv(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/GloNAF_v_2_01/csv/taxon.csv"), locale = locale(encoding = "latin1")),
  overwrite = TRUE
)
# %>%
# slice_sample(n = 20)
my_species_list <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/00_accepeted_species_list.csv"))
my_species_list <- my_species_list[, 2, drop = FALSE]
colnames(my_species_list) <- "taxa_accepted"
my_species_list
dbWriteTable(duck_glonaf, "my_species", my_species_list, overwrite = TRUE)
# register wcvp data
dbWriteTable(duck_glonaf, "wcvp_names",
  read_delim(publication_path("LOCAL_AUX_ROOT", "202311reclamation/wcvp/wcvp_names.csv"),
    delim = "|"
  ),
  overwrite = TRUE
)
dbWriteTable(duck_glonaf, "wcvp_distribution",
  read_delim(publication_path("LOCAL_AUX_ROOT", "202311reclamation/wcvp/wcvp_distribution.csv"),
    delim = "|"
  ),
  overwrite = TRUE
)
# Verify the outputs after completion.
dbListTables(duck_glonaf)

# read reclamation and region data
reclamation_data <- read_csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/reclamation_data_Final.csv")) %>%
  mutate(City = iconv(City, to = "UTF-8")) %>%
  mutate(City = tolower(City)) %>%
  mutate(across(everything(), ~ iconv(.x, from = "latin1", to = "UTF-8"))) %>%
  mutate(across(everything(), ~ gsub("[^\x20-\x7E]", "", .x))) %>%
  mutate(Province = tolower(Province))
region_data <- read_csv(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/GloNAF_v_2_01/csv/region.csv")) %>%
  mutate(across(everything(), ~ iconv(.x, from = "latin1", to = "UTF-8"))) %>%
  mutate(across(everything(), ~ gsub("[^\x20-\x7E]", "", .x))) %>%
  mutate(name = tolower(name))

city_data <- reclamation_data %>%
  select(City) %>%
  drop_na()
Province_data <- reclamation_data %>%
  select(Province) %>%
  drop_na()
# multi match
matched_data <- reclamation_data %>%
  inner_join(region_data, by = c("Province" = "name"))

write.csv(matched_data, publication_path("LOCAL_ANALYSIS_ROOT", "data/invasion_reclamation_cities.csv"))

# Your GeoNames username
username <- Sys.getenv("GEONAMES_USERNAME")
if (!nzchar(username)) stop("Set GEONAMES_USERNAME before using the GeoNames service.")
matched_data$City <- as.character(matched_data$City)
# Function to get coordinates from GeoNames API
# Function to get coordinates using the GeoNames API
get_coordinates <- function(city_name, username) {
  coords <- c(NA, NA)

  # Encode the city name to handle spaces and special characters
  city_name_encoded <- URLencode(city_name)

  # Construct the API URL
  url <- paste0("http://api.geonames.org/searchJSON?q=", city_name_encoded, "&maxRows=1&username=", username)

  # Try fetching data from the API
  response <- tryCatch(
    {
      httr::GET(url)
    },
    error = function(e) {
      print(paste("Error in API call for city:", city_name, "-", e))
      return(NULL)
    }
  )

  if (!is.null(response) && httr::status_code(response) == 200) {
    location <- httr::content(response)
    if (length(location$geonames) > 0) {
      coords[1] <- as.numeric(location$geonames[[1]]$lat)
      coords[2] <- as.numeric(location$geonames[[1]]$lng)
    }
  } else {
    print(paste("No valid response for city:", city_name))
  }

  return(coords)
}




# Loop through the data and update missing coordinates
for (i in 1:nrow(matched_data)) {
  if (is.na(matched_data$Latitude[i]) || is.na(matched_data$Longitude[i])) {
    coords <- get_coordinates(matched_data$City[i], username)
    matched_data$Latitude[i] <- coords[[1]]
    matched_data$Longitude[i] <- coords[[2]]
  }
}
# Convert Longitude and Latitude to numeric if they are not already
matched_data$Longitude <- as.numeric(matched_data$Longitude)
matched_data$Latitude <- as.numeric(matched_data$Latitude)
colnames(matched_data)[8] <- "reclamation_area"


write.csv(matched_data, publication_path("LOCAL_ANALYSIS_ROOT", "data/invasion_reclamation_citieswithlatitude.csv"))

duckdb_register(duck_glonaf, "city", matched_data)
# dbExecute(duck_glonaf, "DROP VIEW IF EXISTS city;")
# dbExecute(duck_glonaf, "DROP TABLE IF EXISTS city;")

# construct query
query <- "
  SELECT
    city.id AS city_id,
    city.City,
    city.Province,
    city.Country,
    city.Latitude,
    city.Longitude,
    city.reclamation_area,
    city.code,
    flora2.taxon_wcvp_id,
    flora2.region_id AS flora_region_id,
    flora2.status,
    taxon.id AS taxon_id,
    taxon.taxa_accepted,
    taxon.family_wcvp
  FROM city
  JOIN flora2 ON city.id = flora2.region_id
  JOIN taxon ON flora2.taxon_wcvp_id = taxon.id
"

# query and convert to tibble
invasioncity_data <- dbGetQuery(duck_glonaf, query) %>%
  as_tibble() %>%
  drop_na()

# static analysis
taxa_count_per_city <- invasioncity_data %>%
  group_by(City, Province, Country, Latitude, Longitude, reclamation_area) %>%
  summarize(taxon_count = n(), .groups = "drop")
write.csv(taxa_count_per_city, publication_path("LOCAL_ANALYSIS_ROOT", "data/03taxa_count_per_city.csv"))

# Create a world map base
world_map <- borders("world", colour = "gray50", fill = "gray50")

# Plot the data on the world map with a heatmap
global <- ggplot(taxa_count_per_city, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") + # Add world map background
  geom_point(aes(size = taxon_count, alpha = taxon_count), color = "#96C37D") + # Add points with size based on 'Value'
  scale_size_continuous(range = c(1, 10)) + # Adjust size range
  scale_color_viridis_c(option = "A") +
  scale_alpha_continuous(range = c(0.5, 1)) + # Use a color scale for the values
  theme_minimal() + # Apply minimal theme
  labs(
    title = "Location Points with Value Size",
    x = "Longitude", y = "Latitude", alpha = "taxon_count"
  )

# Display the plot
ggsave("world_map_heatmap.png", plot = global, width = 10, height = 5.08, dpi = 300)

# To filter out all rows in the reclamation_area column that do not have the value "negligible,"
filtered_data <- taxa_count_per_city %>%
  filter(reclamation_area != "Negligible")

write.csv(filtered_data, publication_path("LOCAL_ANALYSIS_ROOT", "data/04onlycity_duodu.csv"))
# Convert the filtered reclamation_area column to numeric
filtered_data$reclamation_area <- as.numeric(filtered_data$reclamation_area)

filtered_global <- ggplot(filtered_data, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") + # Add world map background
  geom_point(aes(size = taxon_count, alpha = taxon_count), color = "#9DC3E7") + # Add points with size based on 'Value'
  scale_size_continuous(range = c(1, 10)) + # Adjust size range
  scale_color_viridis_c(option = "A") +
  scale_alpha_continuous(range = c(0.5, 1)) + # Use a color scale for the values
  theme_minimal() + # Apply minimal theme
  labs(
    title = "Location Points with Value Size",
    x = "Longitude", y = "Latitude", alpha = "taxon_count"
  )
filtered_global

reclamation_global <- ggplot(filtered_data, aes(x = Longitude, y = Latitude)) +
  borders("world", colour = "gray85", fill = "gray80") + # Add world map background
  geom_point(aes(size = reclamation_area, alpha = reclamation_area), color = "#9DC3E7") + # Add points with size based on 'Value'
  scale_size_continuous(range = c(1, 10)) + # Adjust size range
  scale_color_viridis_c(option = "A") +
  scale_alpha_continuous(range = c(0.5, 1)) + # Use a color scale for the values
  theme_minimal() + # Apply minimal theme
  labs(
    title = "Location Points with Value Size",
    x = "Longitude", y = "Latitude", alpha = "reclamation_area"
  )
reclamation_global

glonaf2024 <- st_read(publication_path("LOCAL_AUX_ROOT", "202311reclamation/GloNAF/glonaf_2024_nol/glonaf_2024_nol/glonaf_2024_nol.shp"))
# Load a base map of the world
world <- ne_countries(scale = "medium", returnclass = "sf")
breaks <- unname(quantile(glonaf2024$SRichness, probs = seq(0, 1, length.out = 10))) %>% round()
# Plot the base map with the shapefile data as a heatmap
glonaf2024heatmap <- ggplot(data = world) +
  geom_sf(fill = "gray80", color = "gray80") +
  geom_sf(data = glonaf2024, aes(fill = SRichness), color = NA) +
  scale_fill_gradientn(
    colors = c("#e5e4c5", "#4a7b4d", "#3e6e5a", "#525E52", "#244965", "#4A5DAD", "#4B3E8C", "#4B1F6C"),
    breaks = breaks, na.value = "grey80",
    limits = c(1, 2491),
    labels = scales::label_comma()
  ) +
  theme_minimal() +
  coord_sf(crs = st_crs("+proj=eck4")) +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(2, "cm"), # Stretch the legend,
    panel.grid.major = element_line(linetype = "dashed", color = "grey80", size = 0.2), # Dashed grid lines
    panel.grid.minor = element_blank(), # Optionally remove minor grid lines
  )
ggsave("glonaf2024heatmap.png", plot = glonaf2024heatmap, width = 10, height = 9.51, dpi = 300)
################### China#####################
China_glonaf2024 <- glonaf2024 %>% filter(ISOcountry %in% c("CHN", "TWN"))

China <- st_read("https://geojson.cn/api/data/china.json")

ggplot(China) +
  geom_sf(fill = "gray80", color = "black", size = 6000)
# plot
China_glonaf2024heatmap <- ggplot(China) +
  geom_sf(fill = "gray80", color = "black", size = 10000) +
  geom_sf(data = China_glonaf2024, aes(fill = SRichness), color = NA) +
  scale_fill_gradientn(colors = c("#e5e4c5", "#E6F86F", "#72B468", "#4a7b4d", "#2D8C63", "#244965", "#4A5DAD", "#4B3E8C", "#4B1F6C")) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104") +
  annotation_scale(location = "bl", text_face = "bold", ) +
  # spatial-aware automagic north arrow
  annotation_north_arrow(
    location = "tl", which_north = "false",
    style = north_arrow_fancy_orienteering
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times_New_Roman", size = 18, face = "bold"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey80", size = .2),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )
China_glonaf2024heatmap
ggsave("Chinaglonaf2024heatmap.png", plot = China_glonaf2024heatmap, width = 10, height = 10.19, dpi = 300)
########## GBR#############
GBR_glonaf2024 <- glonaf2024 %>% filter(grepl("GBR.", IDregion) & ISOcountry == "GBR")
gbr <- world %>% filter(name %in% c("United Kingdom"))

GBR_glonaf2024heatmap <- ggplot(gbr) +
  geom_sf(fill = "gray80", color = "black", size = 10000) +
  geom_sf(data = GBR_glonaf2024, aes(fill = SRichness), color = NA) +
  scale_fill_gradientn(colors = c("#e5e4c5", "#E6F86F", "#72B468", "#4a7b4d", "#2D8C63", "#244965", "#4A5DAD", "#4B3E8C", "#4B1F6C")) +
  coord_sf(crs = "+proj=laea +lat_0=54 +lon_0=-2") +
  annotation_scale(location = "bl", text_face = "bold", ) +
  # spatial-aware automagic north arrow
  annotation_north_arrow(
    location = "tl", which_north = "false",
    style = north_arrow_fancy_orienteering
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times_New_Roman", size = 18, face = "bold"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey80", size = .2),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

GBR_glonaf2024heatmap
ggsave("GBRglonaf2024heatmap.png", plot = GBR_glonaf2024heatmap, width = 10, height = 10.93, dpi = 300)
########## invasion only############

# static analysis
in_taxa_count_per_city <- invasioncity_data %>%
  filter(status == "Invasive") %>%
  group_by(City, Province, Country, Latitude, Longitude, reclamation_area) %>%
  summarize(taxon_count = n(), .groups = "drop") %>%
  filter(reclamation_area != "Negligible")

write.csv(in_taxa_count_per_city, publication_path("LOCAL_ANALYSIS_ROOT", "data/05onlyinvasion_city_duodu.csv"))

in_taxa_count <- invasioncity_data %>%
  filter(status == "Invasive") %>%
  group_by(taxa_accepted, family_wcvp) %>%
  summarize(taxon_count = n(), .groups = "drop")
write.csv(in_taxa_count, publication_path("LOCAL_ANALYSIS_ROOT", "data/06invasion_species_count.csv"))

CN_in_taxa <- invasioncity_data %>%
  filter(status == "Invasive") %>%
  filter(reclamation_area != "Negligible") %>%
  filter(grepl("CHN|TWN", code))

CN_in_taxa_count <- CN_in_taxa %>%
  group_by(taxa_accepted) %>%
  summarize(taxon_count = n(), .groups = "drop")

CN_in_taxa_count_per_city <- CN_in_taxa %>%
  group_by(City, Province, Latitude, Longitude, reclamation_area) %>%
  summarize(taxon_count = n(), .groups = "drop") %>%
  filter(reclamation_area != "Negligible")

CN_city <- st_read(publication_path("LOCAL_ANALYSIS_ROOT", "data/data/ChinaAdminDivisonSHP-24.02.06/3_City/city.shp"))

# List of city names as a single pattern string for grepl
cities_pattern <- "\u53f0\u6e7e|\u510b\u5dde|\u4e09\u4e9a|\u5927\u8fde|\u4e1c\u8425|\u5e7f\u5dde|\u6d77\u53e3|\u5609\u5174|\u5b81\u6ce2|\u8386\u7530|\u9752\u5c9b|\u79e6\u7687\u5c9b|\u6cc9\u5dde|\u65e5\u7167|\u4e0a\u6d77|\u6c55\u5934|\u6df1\u5733|\u5510\u5c71|\u5929\u6d25|\u5a01\u6d77|\u6e29\u5dde|\u53a6\u95e8|\u70df\u53f0|\u8425\u53e3|\u6e5b\u6c5f|\u821f\u5c71|\u73e0\u6d77"

recaLCN_SHP <- CN_city %>%
  filter(grepl(cities_pattern, ct_name))

translation <- c(
  "\u5929\u6d25\u5e02" = "Tianjin", "\u6cb3\u5317\u7701" = "Hebei", "\u8fbd\u5b81\u7701" = "Liaoning",
  "\u4e0a\u6d77\u5e02" = "Shanghai", "\u6d59\u6c5f\u7701" = "Zhejiang", "\u798f\u5efa\u7701" = "Fujian",
  "\u5c71\u4e1c\u7701" = "Shandong", "\u5e7f\u4e1c\u7701" = "Guangdong", "\u6d77\u5357\u7701" = "Hainan",
  "\u53f0\u6e7e\u7701" = "Taiwan"
)

# Use recode to replace values in pr_name.
recaLCN_SHP$pr_name <- recode(recaLCN_SHP$pr_name, !!!translation) %>% tolower()

prrichness_unique <- CN_in_taxa_count_per_city %>% distinct(Province, taxon_count, .keep_all = F)


recalCN_richness <- recaLCN_SHP %>%
  left_join(prrichness_unique, by = c("pr_name" = "Province"))

# Convert CN_in_taxa_count_per_city to an sf object
CN_in_taxa_count_per_city_sf <- st_as_sf(CN_in_taxa_count_per_city, coords = c("Longitude", "Latitude"), crs = 4326) # Assuming WGS84 CRS
Chinacostal <- st_transform(China, crs = "+proj=laea +lat_0=40 +lon_0=109")
# Transform the CRS of the points to match the map projection
CN_in_taxa_count_per_city_sf <- st_transform(CN_in_taxa_count_per_city_sf, crs = "+proj=laea +lat_0=40 +lon_0=109")
CN_in_taxa_count_per_city_sf$reclamation_area <- as.numeric(CN_in_taxa_count_per_city_sf$reclamation_area)

xlim_proj <- c(st_bbox(CN_in_taxa_count_per_city_sf)$xmin, st_bbox(CN_in_taxa_count_per_city_sf)$xmax)
ylim_proj <- c(st_bbox(CN_in_taxa_count_per_city_sf)$ymin, st_bbox(CN_in_taxa_count_per_city_sf)$ymax)

CN_in_recla <- ggplot() +
  geom_sf(data = China, fill = "grey95", color = "black") +
  geom_sf(data = coastal, fill = "#9DC3E7", color = "#9DC3E7") +
  geom_sf(data = recalCN_richness, aes(fill = taxon_count), color = NA) +
  scale_fill_gradient(low = "#BEEDC7", high = "#FA7F6F") +
  geom_sf(
    data = CN_in_taxa_count_per_city_sf, aes(size = reclamation_area),
    shape = 21, stroke = 1, color = "grey30", fill = "grey50"
  ) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=109", xlim = xlim_proj, ylim = ylim_proj) + # Longitude limit+
  scale_size_continuous(range = c(1, 10)) + # Control the size range
  theme_minimal() +
  annotation_scale(location = "bl", text_face = "bold", ) +
  # spatial-aware automagic north arrow
  annotation_north_arrow(
    location = "tl", which_north = "false",
    style = north_arrow_fancy_orienteering
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times_New_Roman", size = 13, face = "bold"),
    panel.background = element_rect(fill = "NA"),
    panel.grid.major = element_line(colour = "grey80", size = .3),
  )
CN_in_recla
ggsave("CN_in_recla.png", plot = CN_in_recla, width = 10, height = 11.5, dpi = 300)

##### plot study area################
coastal <- read_sf(publication_path("COASTLINE_FILE")) %>% st_transform(crs = "+proj=laea +lat_0=40 +lon_0=109")

study_area <- ggplot() +
  geom_sf(data = China, fill = NA, color = "black") +
  geom_sf(data = coastal, fill = "blue", color = "black") +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=109", xlim = xlim_proj, ylim = ylim_proj) + # Longitude limit+
  scale_size_continuous(range = c(1, 10)) + # Control the size range
  theme_minimal() +
  annotation_scale(location = "bl", text_face = "bold", ) +
  # spatial-aware automagic north arrow
  annotation_north_arrow(
    location = "tl", which_north = "false",
    style = north_arrow_fancy_orienteering
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times_New_Roman", size = 13, face = "bold"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey80", size = .3),
  )
study_area

ggplot(in_taxa_count_per_city, aes(x = reorder(City, -taxon_count), y = taxon_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of Taxa in Each City", x = "City", y = "Number of Taxa")

api <- "https://gift.uni-goettingen.de/api/extended/" # Check the provider requirements for this endpoint.
GIFT_version <- "latest" # or 'beta'
ref <- GIFT_references(GIFT_version = GIFT_version, api = api)

dbListTables(duck_glonaf)
# 4. match my species list with glonaf and find the id   (my_species -> taxon -> wcvp_names -> wcvp_distribution)
# Due to naming differences, we first map my_species to wcvp_names robustly

# A. Extract all species and WCVP names
my_spp <- dbGetQuery(duck_glonaf, "SELECT taxa_accepted FROM my_species")$taxa_accepted
wcvp_n <- dbGetQuery(duck_glonaf, "SELECT plant_name_id, taxon_name, accepted_plant_name_id FROM wcvp_names")

# B. Exact Match
exact_matches <- wcvp_n %>%
  filter(taxon_name %in% my_spp) %>%
  group_by(taxon_name) %>%
  slice(1) %>%
  ungroup() %>%
  select(taxa_accepted = taxon_name, wcvp_plant_name_id = plant_name_id, wcvp_accepted_id = accepted_plant_name_id)

missing_spp <- setdiff(my_spp, exact_matches$taxa_accepted)

# C. Fuzzy Match (handle hybrids " x " and small spelling differences)
fuzzy_matches <- list()
for (sp in missing_spp) {
  # Add exact hybrid check
  hybrid_sp1 <- gsub(" ", " × ", sp)
  hybrid_sp2 <- gsub(" ", " x ", sp)
  m_hyb <- wcvp_n %>%
    filter(taxon_name == hybrid_sp1 | taxon_name == hybrid_sp2) %>%
    slice(1)

  if (nrow(m_hyb) > 0) {
    fuzzy_matches[[sp]] <- data.frame(taxa_accepted = sp, wcvp_plant_name_id = m_hyb$plant_name_id[1], wcvp_accepted_id = m_hyb$accepted_plant_name_id[1])
  } else {
    # Distance matching using adist
    dists <- adist(sp, wcvp_n$taxon_name, partial = FALSE, ignore.case = TRUE)[1, ]
    idx <- which.min(dists)
    if (dists[idx] <= 3) { # Allow small typos like colona vs colonum
      fuzzy_matches[[sp]] <- data.frame(taxa_accepted = sp, wcvp_plant_name_id = wcvp_n$plant_name_id[idx], wcvp_accepted_id = wcvp_n$accepted_plant_name_id[idx])
    }
  }
}

fuzzy_df <- bind_rows(fuzzy_matches)
all_matches <- bind_rows(exact_matches, fuzzy_df) %>%
  mutate(final_plant_name_id = coalesce(wcvp_accepted_id, wcvp_plant_name_id))

# D. Write mapping table to DB
dbWriteTable(duck_glonaf, "my_species_wcvp_map", all_matches, overwrite = TRUE)


### native range(introduced = 0)
query_native_range <- "
  SELECT DISTINCT
    m.taxa_accepted AS species_name,
    t.id AS glonaf_taxon_id,
    map.wcvp_plant_name_id,
    wd.continent,
    wd.region,
    wd.area,
    wd.introduced,
    wd.extinct
  FROM my_species m
  LEFT JOIN taxon t ON m.taxa_accepted = t.taxa_accepted
  LEFT JOIN my_species_wcvp_map map ON m.taxa_accepted = map.taxa_accepted
  JOIN wcvp_distribution wd ON map.final_plant_name_id = wd.plant_name_id
  WHERE wd.introduced = 0 OR wd.introduced IS NULL
  ORDER BY m.taxa_accepted, wd.continent
"
species_native_ranges <- dbGetQuery(duck_glonaf, query_native_range)
write.csv(species_native_ranges,
  publication_path("LOCAL_ANALYSIS_ROOT", "data/native_invasion_region/species_native_ranges.csv"),
  row.names = FALSE
)

cat("\n[Native Ranges Summary]\n")
cat("Total records:", nrow(species_native_ranges), "\n")
cat("Unique species:", length(unique(species_native_ranges$species_name)), "/", length(my_spp), "\n")


### invasive range(introduced = 1)
query_invasive_range <- "
  SELECT DISTINCT
    m.taxa_accepted AS species_name,
    t.id AS glonaf_taxon_id,
    map.wcvp_plant_name_id,
    wd.continent,
    wd.region,
    wd.area,
    wd.introduced,
    wd.extinct
  FROM my_species m
  LEFT JOIN taxon t ON m.taxa_accepted = t.taxa_accepted
  LEFT JOIN my_species_wcvp_map map ON m.taxa_accepted = map.taxa_accepted
  JOIN wcvp_distribution wd ON map.final_plant_name_id = wd.plant_name_id
  WHERE wd.introduced = 1 OR wd.introduced IS NULL
  ORDER BY m.taxa_accepted, wd.continent
"
species_invasive_ranges <- dbGetQuery(duck_glonaf, query_invasive_range)
write.csv(species_invasive_ranges,
  publication_path("LOCAL_ANALYSIS_ROOT", "data/native_invasion_region/species_invasive_ranges.csv"),
  row.names = FALSE
)

cat("\n[Invasive Ranges Summary]\n")
cat("Total records:", nrow(species_invasive_ranges), "\n")
cat("Unique species:", length(unique(species_invasive_ranges$species_name)), "/", length(my_spp), "\n")

# Check remaining completely missing
final_extracted <- unique(c(species_native_ranges$species_name, species_invasive_ranges$species_name))
cat("\nSpecies with ZERO distribution data (Native or Invasive):\n")
print(setdiff(my_spp, final_extracted))

dbDisconnect(duck_glonaf)
