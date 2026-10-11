# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: da0e6f871d9c19105afbbf3b578482dad20328361971d3a8e2e22e59a8083854

library(remotes)
install_github("Rdatatable/data.table")

install_github("tidyverse/ggplot2")

install_github("r-spatial/sf")

install_github("cran/doParallel")

install_github("rspatial/terra")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
  library(parallel)
  library(doParallel)
  library(pbapply)
})
setwd("${PRIVATE_EXTERNAL_PATH}")
library(scales)
library(ggrepel)
library(ggnewscale)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(svglite)
library(systemfonts)
library(Cairo)
library(ggspatial)
library(forcats)
library(stringr)
packageVersion("systemfonts")
package_version()

install.packages("Cairo")

input_file <- "data/species_geodatapoint_cleaned.csv" 
output_file <- "data/species_richness.csv"           
heatmap_file <- "data/species_richness_heatmap.png"


grid_resolution <- 0.1

num_cores <- detectCores() - 1 

species_geodatapoint_cleaned <- fread('${PRIVATE_EXTERNAL_PATH}')
city_df <- fread('${LOCAL_ANALYSIS_ROOT}/data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv')

### create_grid 

species_geodatapoint_cleaned[, lon_bin := floor(decimalLongitude / grid_resolution) * grid_resolution + grid_resolution / 2]
species_geodatapoint_cleaned[, lat_bin := floor(decimalLatitude / grid_resolution) * grid_resolution + grid_resolution / 2]

unique_grids <- unique(species_geodatapoint_cleaned[, .(lon_bin, lat_bin)])
unique_grids[1, lon_bin]
richness <- uniqueN(species_geodatapoint_cleaned[lon_bin == unique_grids[2, lon_bin] & lat_bin ==unique_grids[2, lat_bin], species])

richness_results <- pblapply(seq_len(nrow(unique_grids)), function(i) {
  grid_lon <- unique_grids[i, lon_bin]  
  grid_lat <- unique_grids[i, lat_bin] 
  
  # Select data for current grid and calculate richness
  richness <- uniqueN(species_geodatapoint_cleaned[lon_bin ==  grid_lon & lat_bin == grid_lat, species])
  
  
  return(data.table(lon_bin = grid_lon, lat_bin = grid_lat, species_richness = richness))
  
  rm(grid_lon)
  rm(grid_lat)
  gc() 
})

# Merge results
richness_results <- rbindlist(richness_results)

saveRDS(richness_results, '${LOCAL_DATABASE_ROOT}/RDS/richness_results.rds')

richness_results <- readRDS('${LOCAL_DATABASE_ROOT}/RDS/richness_results.rds')
  
# Color: low-medium-high (consistent with previous)
cols_risk <- c("1" = "#66BD63", "2" = "#FEC44F", "3" = "#D73027")

# —— Clean city point data (rename according to your headers to avoid case/spelling issues) ——
# World boundaries (sf)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Organize city table
city_df <- city_df |>
  rename(lon = Longitude, lat = Latitude, reclam_km2 = reclamation_area) |>
  mutate(
    risk_class = factor(risk_class),
    reclam_km2 = as.numeric(reclam_km2)
  ) |>
  filter(!is.na(lon), !is.na(lat), !is.na(reclam_km2))
# —— Overlay plotting —— 
city_df$reclamation_level <- cut(city_df$reclam_km2,
                                 breaks = c(-Inf, 550, 1700, 6300, 12800, Inf),
                                 labels = c("1 - 550", "550 - 1700", "1700 - 6300", "6300 - 12800", ">12800"),
                                 right = TRUE)

top_cities <- city_df |> 
  dplyr::arrange(desc(reclam_km2)) |> 
  dplyr::slice(1:30)
top_cities
CairoPDF("${LOCAL_ANALYSIS_ROOT}/figure/study_area.pdf", width = 12, height = 8)

# Carefully selected advanced color scheme (low value light purple to high value orange-red)
rich_cols <- c(
  "#455F9E",  # Blue-gray
  "#6266b3",  # Blue-gray to warm brown transition
  "#64a3b6",  # Neutral gray-brown
  "#B6AF64",  # Warm brown transition
  "#CF8E4A",  # Warm brown
  "#e2592f"   # Red
)
# x vector is the continuous variable used for coloring, e.g., richness_results$species_richness
x <- richness_results$species_richness

# Construct a sample-based ECDF transformation
make_ecdf_trans <- function(x) {
  x <- x[is.finite(x)]
  Fwd <- stats::ecdf(x)
  Inv <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 7)
  scales::trans_new("ecdf", transform = function(z) Fwd(z), inverse = Inv,
                    domain = range(x, na.rm = TRUE))
}

ecdf_trans <- make_ecdf_trans(x)

p <- ggplot() +
  # —— Heatmap (continuous fill) ——
  geom_tile(data = richness_results,
            aes(x = lon_bin, y = lat_bin, fill = species_richness)) +  scale_fill_gradientn(
    colours = rich_cols,
    trans   = ecdf_trans,         # Key: map data to uniform distribution
    limits  = range(x, na.rm = TRUE),
    oob     = squish,
    breaks  = quantile(x, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE),
    labels  = label_number(accuracy = 1),
    name    = "Species richness"
  ) +

  # —— Country borders (thinner, lighter) ——
  geom_sf(data = world, fill = NA, color = "#5a5c6d", linewidth = 0.2) +

  # —— Start second fill scale for point plot —— 
  ggnewscale::new_scale_fill() +

  # —— City bubbles (more prominent) ——
  geom_point(data = city_df,
           aes(x = lon, y = lat, size = reclamation_level),
           shape = 21, colour = "#423e3e", fill = "#E1C59D", stroke = 0.2, alpha = 0.75) +
scale_size_manual(name = "Total reclaimed land 2000–2020 by city (ha)",
                  values = c(2, 4, 6, 8, 10)) + # Adjust based on visual effect
  scale_fill_brewer(palette = "Set2", name = "Risk class") +
  # geom_text(data = top_cities,
  #         aes(x = lon, y = lat, label = city),
  #         size = 3.5, family = "", fontface = "bold", color = "#f40707", check_overlap = TRUE)+


  # —— Coordinate system —— 
  coord_sf(crs = sf::st_crs(4326), xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +

  theme_minimal(base_family = "") +
  labs(title = "Species Richness with Reclamation Cities",
       x = "Longitude", y = "Latitude") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(size = 10, color = "black"),         # Axis text
    axis.title = element_blank(),        # Remove axis titles
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Black border
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )
# —— Axis tick label functions (new) ——
lab_lon <- function(x) paste0(abs(x), "°", ifelse(x >= 0, "E", "W"))
lab_lat <- function(y) paste0(abs(y), "°", ifelse(y >= 0, "N", "S"))

lon_breaks <- seq(105, 130, by = 15)   # Longitude lines (can be changed to 10/15)
lat_breaks <- seq(15,  45,  by = 10)   # Latitude lines

p2 <- ggplot() +
  # —— Heatmap (continuous fill) ——
  geom_tile(data = richness_results,
            aes(x = lon_bin, y = lat_bin, fill = species_richness)) +   scale_fill_gradientn(
    colours = rich_cols,
    trans   = ecdf_trans,         # Key: map data to uniform distribution
    limits  = range(x, na.rm = TRUE),
    oob     = squish,
    breaks  = quantile(x, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE),
    labels  = label_number(accuracy = 1),
    name    = "Species richness"
  )+

  # —— Country borders (thinner, lighter) ——
  geom_sf(data = world, fill = NA, color = "#5a5c6d", linewidth = 0.2) +
  # —— Lat/lon grid lines (new, placed after country borders, before point plot) ——
  geom_vline(xintercept = lon_breaks, colour = "grey85", linewidth = 0.25) +
  geom_hline(yintercept = lat_breaks, colour = "grey85", linewidth = 0.25) +

  # —— Start second fill scale for point plot ——
  ggnewscale::new_scale_fill() +

  # —— City bubbles (more prominent) ——
  geom_point(data = city_df,
           aes(x = lon, y = lat, size = reclamation_level),
           shape = 21,  colour = "#423e3e", fill = "#E1C59D", stroke = 0.2, alpha = 0.75) +
scale_size_manual(name = "Total reclaimed land 2000–2020 by city (ha)",
                  values = c(2, 4, 6, 8, 10)) + # Adjust based on visual effect

  # geom_text(data = top_cities,
  #         aes(x = lon, y = lat, label = city),
  #         size = 3.5, family = "", fontface = "bold", color = "#f40707",, check_overlap = TRUE)+


  # —— Coordinate system ——
  coord_sf(crs = sf::st_crs(4326), xlim = c(105, 130), ylim = c(15, 45), expand = FALSE) +

  # —— Display axis ticks as °E/°N (modified) ——
  scale_x_continuous(breaks = lon_breaks, labels = lab_lon, expand = c(0,0)) +
  scale_y_continuous(breaks = lat_breaks, labels = lab_lat, expand = c(0,0)) +


  theme_minimal(base_family = "") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),        # Remove axis titles
    panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),  # Black border
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )




# Combine two plots and merge legends
combined_plot <- p + p2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")  # Set unified legend position

combined_plot
print(combined_plot)
dev.off()

  # Save heatmap
  ggsave('${LOCAL_DATABASE_ROOT}/evaluations/Species_Richness_Heatmap.png', plot = combined_plot, width = 12, height = 8, dpi = 300)
  

ggsave("${LOCAL_ANALYSIS_ROOT}/figure/study_area_nontext.pdf", combined_plot, width = 12, height =8, device = cairo_pdf)

ggsave("${LOCAL_ANALYSIS_ROOT}/figure/study_area.svg",
       plot = combined_plot,
       device = "svg",
       width = 12, height = 8,
       units = "in",
       dpi = 300)

#=====study_area_barchart ===============

# ---- 5) Colors (left: city count=deep blue; right: area=amber orange) ----
col_cities <- "#d1b7d3"  # Deep blue you liked before
col_area   <- "#E1C59D"  # Advanced orange that matches better
#=======1) Data cleaning: remove no continent/Antarctica/high seas, remove area<=0  =====

recode_continent <- function(x){
  x0 <- tolower(trimws(x))
  dplyr::case_when(
    grepl("africa", x0) ~ "Africa",
    grepl("europe", x0) ~ "Europe",
    grepl("oceania|australia|zealand", x0) ~ "Oceania",
    grepl("south\\s*america", x0) ~ "South America",
    grepl("north\\s*america|caribbean|central\\s*america", x0) ~ "North America",
    grepl("asia|east asia|south asia|southeast|west asia|central asia|middle east", x0) ~ "Asia",
    TRUE ~ NA_character_
  )
}
CONT_LEVELS <- c("Africa","Asia","Europe","North America","South America","Oceania")

# city_df should contain: city, Regions, reclamation_area(hm²)
df <- city_df %>%
  mutate(Continent = recode_continent(Regions)) %>%
  filter(!is.na(Continent)) %>%
  mutate(Continent = factor(Continent, levels = CONT_LEVELS),
         reclamation_area = as.numeric(reclamation_area)) %>%
  filter(is.finite(reclamation_area), reclamation_area >= 0)

# ---- 2) Left bar: city count by continent (consistent with yours) ----
count_by_cont <- df %>% distinct(city, Continent) %>% count(Continent, name = "n_cities")


# ---- 3) Right bar: total reclaimed area by continent (hm²), no longer stacked ----
area_total <- df %>%
  group_by(Continent) %>%
  summarise(total_hm2 = sum(reclamation_area), .groups = "drop")

# ---- 4) Scaling and right axis (quantile control upper limit, avoid extreme values) ----
# —— Scaling factor: compress area to same main axis range as city count ——
max_cities <- max(count_by_cont$n_cities, na.rm = TRUE)
max_area   <- max(area_total$total_hm2,    na.rm = TRUE)
s <- (max_cities * 0.95) / (max_area * 1.05)   # Leave 5% margin at top; right axis starts from 0

# —— Plotting —— 

p1 <- ggplot() +
  geom_col(
    data = area_total,
    aes(x = Continent, y = total_hm2 * s, fill = "Reclaimed area"),
    width = 0.42, position = position_nudge(x = +0.22),
    colour = "#00000000", linewidth = 0.35, alpha = 0.95
  ) +
  geom_text(
    data = area_total,
    aes(x = Continent, y = total_hm2 * s,
        label = scales::label_number(accuracy = 1, big.mark = ",")(total_hm2)),
    position = position_nudge(x = +0.22),
    vjust = -0.55, size = 3.3, colour = col_area, fontface = "bold"
  ) +
  geom_col(
    data = count_by_cont,
    aes(x = Continent, y = n_cities, fill = "Cities"),
    width = 0.42, position = position_nudge(x = -0.22),
    colour = "#00000000", linewidth = 0.35
  ) +
  geom_text(
    data = count_by_cont,
    aes(x = Continent, y = n_cities, label = n_cities),
    position = position_nudge(x = -0.22),
    vjust = -0.55, size = 3.3, colour = col_cities, fontface = "bold"
  ) +
  scale_y_continuous(
    name = "Cities",
    expand = expansion(mult = c(0, .12)),
    sec.axis = sec_axis(
      ~ . / s,
      name   = "Reclaimed area (hm\u00B2)",
      labels = function(x) scales::label_number(big.mark = ",", accuracy = 1)(x)
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Cities" = col_cities, "Reclaimed area" = col_area)
  ) +
  labs(x = NULL, title = "Cities vs. reclaimed area by continent (hm\u00B2)") +
  theme_classic(base_family = "") +
  theme(
    legend.position      = "top",
    panel.border         = element_rect(colour = "black", fill = NA, linewidth = .6),
    axis.title.y.left    = element_text(color = col_cities, face = "bold"),
    axis.title.y.right   = element_text(color = col_area,   face = "bold"),
    plot.title           = element_text(size = 13, face = "bold", hjust = 0)
  ) +
  coord_cartesian(clip = "off")


p1
# ggsave("con

sessionInfo()
find.package("scales"); packageVersion("scales")
getAnywhere(scales::train_continuous)
