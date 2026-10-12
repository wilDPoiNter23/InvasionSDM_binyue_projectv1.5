# City analysis table and GAM scores
# Input: City boundaries, suitability raster, predictor stack and reference city table
# Output: Updated 108-city means and PCA/GAM input table

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(sf)
library(terra)
library(dplyr)
library(tools)

final_root <- publication_path("OUTPUT_ROOT")
data_dir <- file.path(final_root, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

updated_city_path <- publication_path("CITY_BOUNDARIES")
original_city_path <- publication_path("CITY_BOUNDARIES_ORIGINAL")
recheck_dir <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/research_cities/recheck")
old_mean_path <- publication_path("LOCAL_ANALYSIS_ROOT", "data/10_03_108city_mean_variables.csv")
old_gam_path <- publication_path("PCA_REFERENCE_INPUT")
risk_raster_path <- publication_path("MAIN_RESPONSE_RASTER")
stack_raster_path <- publication_path("PREDICTOR_STACK")

updated_mean_path <- file.path(data_dir, "10_03_108city_mean_variables_recheck.csv")
updated_gam_path <- file.path(data_dir, "gamtest_withpca_recheck.csv")
boundary_change_path <- file.path(data_dir, "boundary_change_summary.csv")
city_compare_path <- file.path(data_dir, "updated_city_mean_comparison.csv")

normalize_key <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

make_pc <- function(df, cols, ncomp = 1) {
  cols <- intersect(cols, names(df))
  X <- df[, cols, drop = FALSE]
  keep <- sapply(X, function(v) stats::sd(v, na.rm = TRUE) > 1e-8)
  X <- X[, keep, drop = FALSE]
  cc <- complete.cases(X)
  pc <- prcomp(scale(X[cc, , drop = FALSE]), center = TRUE, scale. = TRUE)
  scores <- rep(NA_real_, nrow(df))
  scores[cc] <- pc$x[, 1:ncomp, drop = FALSE] %*% rep(1, ncomp)
  list(
    score = as.numeric(scores),
    loadings = pc$rotation[, 1:ncomp, drop = FALSE]
  )
}

align_pc_sign <- function(new_score, ref_score) {
  ok <- is.finite(new_score) & is.finite(ref_score)
  if (sum(ok) < 3) {
    return(new_score)
  }
  if (cor(new_score[ok], ref_score[ok], use = "pairwise.complete.obs") < 0) {
    return(-new_score)
  }
  new_score
}

recheck_files <- list.files(recheck_dir, pattern = "\\.shp$", full.names = TRUE)
recheck_index <- data.frame(
  recheck_path = recheck_files,
  shapeName = file_path_sans_ext(basename(recheck_files)),
  city_key = normalize_key(file_path_sans_ext(basename(recheck_files))),
  stringsAsFactors = FALSE
) |>
  arrange(shapeName)

if (!nrow(recheck_index)) {
  stop("No shapefiles found in the recheck directory.")
}

old_cities <- st_read(original_city_path, quiet = TRUE) |>
  st_make_valid() |>
  mutate(city_key = normalize_key(shapeName))
new_cities <- st_read(updated_city_path, quiet = TRUE) |>
  st_make_valid() |>
  mutate(city_key = normalize_key(shapeName))

changed_old <- old_cities |>
  filter(city_key %in% recheck_index$city_key)
changed_new <- new_cities |>
  filter(city_key %in% recheck_index$city_key)

if (nrow(changed_new) != nrow(recheck_index)) {
  stop("Not all recheck cities were found in 108finalCities_recheck.shp.")
}

moll_crs <- "+proj=moll +datum=WGS84"
old_area_tbl <- changed_old |>
  st_transform(moll_crs) |>
  mutate(old_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  select(city_key, old_shapeName = shapeName, old_area_km2)
new_area_tbl <- changed_new |>
  st_transform(moll_crs) |>
  mutate(new_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  select(city_key, new_shapeName = shapeName, new_area_km2, shapeType)

boundary_change_tbl <- recheck_index |>
  left_join(old_area_tbl, by = "city_key") |>
  left_join(new_area_tbl, by = "city_key") |>
  mutate(area_change_km2 = new_area_km2 - old_area_km2)
write.csv(boundary_change_tbl, boundary_change_path, row.names = FALSE)

risk_rast <- rast(risk_raster_path)
names(risk_rast) <- "risk"
factor_stack <- rast(stack_raster_path)
full_stack <- c(risk_rast, factor_stack)

changed_vect <- vect(changed_new)
changed_vect <- project(changed_vect, crs(full_stack))
extract_df <- as.data.frame(extract(full_stack, changed_vect, fun = mean, na.rm = TRUE))
extract_df$city_key <- changed_new$city_key
extract_df$shapeName <- changed_new$shapeName
extract_df$ID <- NULL

coord_tbl <- changed_new |>
  st_transform(4326) |>
  st_point_on_surface() |>
  mutate(
    Longitude = st_coordinates(geometry)[, 1],
    Latitude = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(city_key, Longitude, Latitude)

old_mean <- read.csv(old_mean_path, stringsAsFactors = FALSE)
old_mean$city_key <- normalize_key(old_mean$city)

changed_rows <- old_mean |>
  filter(city_key %in% recheck_index$city_key)

new_rows <- changed_rows |>
  select(-any_of(setdiff(c(names(extract_df), "Latitude", "Longitude", "area_km2", "risk_class", "climate_zone", "area_ratio"), "city_key"))) |>
  left_join(extract_df, by = "city_key") |>
  left_join(coord_tbl, by = "city_key") |>
  left_join(new_area_tbl |> select(city_key, area_km2 = new_area_km2), by = "city_key") |>
  mutate(
    city = tolower(shapeName),
    area_ratio = reclamation_area / pmax(area_km2, .Machine$double.eps) / 10000,
    risk_class = case_when(
      risk <= 0.28 ~ 1L,
      risk <= 0.62 ~ 2L,
      risk <= 0.97 ~ 3L,
      TRUE ~ NA_integer_
    ),
    abs_lat = abs(Latitude),
    climate_zone = case_when(
      abs_lat <= 23.5 ~ "Tropical",
      abs_lat <= 35 ~ "Subtropical",
      abs_lat <= 66.5 ~ "Temperate",
      abs_lat > 66.5 ~ "Polar",
      TRUE ~ NA_character_
    ),
    reclamation_level_manual = case_when(
      reclamation_area < 3000 ~ "Low",
      reclamation_area < 10000 ~ "Medium",
      TRUE ~ "High"
    )
  ) |>
  select(names(old_mean))

updated_mean <- old_mean |>
  filter(!city_key %in% recheck_index$city_key) |>
  bind_rows(new_rows) |>
  arrange(match(city_key, old_mean$city_key))

comparison_tbl <- changed_rows |>
  select(city_key, city, old_risk = risk, old_area_km2 = area_km2, old_lat = Latitude, old_lon = Longitude) |>
  left_join(
    new_rows |>
      select(city_key, new_risk = risk, new_area_km2 = area_km2, new_lat = Latitude, new_lon = Longitude),
    by = "city_key"
  ) |>
  mutate(
    delta_risk = new_risk - old_risk,
    delta_area_km2 = new_area_km2 - old_area_km2
  )
write.csv(comparison_tbl, city_compare_path, row.names = FALSE)
write.csv(updated_mean |> select(-city_key), updated_mean_path, row.names = FALSE)

old_gam <- read.csv(old_gam_path, stringsAsFactors = FALSE)
old_gam$city_key <- normalize_key(old_gam$city)
updated_mean$city_key <- normalize_key(updated_mean$city)

extra_cols <- setdiff(names(old_gam), names(updated_mean))
extra_cols <- setdiff(extra_cols, c("anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1",
                                    "log_city_area", "recl_frac", "log_recl"))

gam_df <- updated_mean |>
  left_join(old_gam[, c("city_key", extra_cols)], by = "city_key") |>
  mutate(
    log_city_area = log1p(area_km2),
    recl_frac = reclamation_area / pmax(area_km2, .Machine$double.eps),
    log_recl = log1p(reclamation_area)
  )

clim_cols <- grep("^wc2\\.1_2\\.5m_bio_", names(gam_df), value = TRUE)
soil_cols <- c(
  "bdod_mean_2024_2_5min_aligned", "phh2o_mean_2024_2_5min_aligned",
  "soc_mean_2024_2_5min_aligned", "sand_mean_2024_2_5min_aligned",
  "silt_mean_2024_2_5min_aligned", "ocd_mean_2024_2_5min_aligned",
  "nitrogen_mean_2024_2_5min_aligned", "cfvo_mean_2024_2_5min_aligned"
)
anth_cols <- c("BUILT_S_E2025_2.5min", "VIIRS_2023_2.5min", "GDP2020_2.5min", "Pop2020_2.5min")
marine_cols <- c("salinity_mean", "salinity_range", "temperature_mean", "temperature_range")
geom_cols <- c("slope", "wc2.1_2.5m_elev")

pc_clim <- make_pc(gam_df, clim_cols)
pc_soil <- make_pc(gam_df, soil_cols)
pc_marine <- make_pc(gam_df, marine_cols)
pc_geom <- make_pc(gam_df, geom_cols)
pc_anth <- make_pc(gam_df, anth_cols)

ref_pc <- old_gam[, c("city_key", "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1")]
names(ref_pc) <- c("city_key", "anthro_pc1_ref", "climate_pc1_ref", "soil_pc1_ref", "marine_pc1_ref", "geom_pc1_ref")
gam_df <- gam_df |>
  left_join(ref_pc, by = "city_key") |>
  mutate(
    anthro_pc1 = align_pc_sign(pc_anth$score, anthro_pc1_ref),
    climate_pc1 = align_pc_sign(pc_clim$score, climate_pc1_ref),
    soil_pc1 = align_pc_sign(pc_soil$score, soil_pc1_ref),
    marine_pc1 = align_pc_sign(pc_marine$score, marine_pc1_ref),
    geom_pc1 = align_pc_sign(pc_geom$score, geom_pc1_ref)
  ) |>
  select(-ends_with("_ref"))

for (nm in names(old_gam)) {
  if (!nm %in% names(gam_df)) {
    gam_df[[nm]] <- NA
  }
}

ordered_cols <- c(names(old_gam), setdiff(names(gam_df), names(old_gam)))
gam_df <- gam_df[, ordered_cols]
gam_df <- gam_df |>
  arrange(match(city_key, old_gam$city_key))
write.csv(gam_df |> select(-city_key), updated_gam_path, row.names = FALSE)
