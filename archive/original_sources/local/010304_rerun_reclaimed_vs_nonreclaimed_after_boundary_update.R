# Original source: 010304_rerun_reclaimed_vs_nonreclaimed_after_boundary_update.R
# Original source location: <LOCAL_CODE_ROOT>/010304_rerun_reclaimed_vs_nonreclaimed_after_boundary_update.R
# Original source SHA-256: 8e5e05f1516bb92db7f94c6b38a958bf5f7f146b704299e3a130e209fee85757
# Public-safe original snapshot: archive/original_sources/local/010304_rerun_reclaimed_vs_nonreclaimed_after_boundary_update.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(tools)

sf_use_s2(FALSE)

final_root <- "<LOCAL_ANALYSIS_ROOT>/FinalResults"
out_dir <- file.path(final_root, "0603reclaim_analysis_recheck")
data_dir <- file.path(final_root, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

updated_city_path <- "<LOCAL_ANALYSIS_ROOT>/data/data/108finalCities_recheck.shp"
recheck_dir <- "<LOCAL_ANALYSIS_ROOT>/data/data/research_cities/recheck"
risk_raster_path <- "<LOCAL_ANALYSIS_ROOT>/data/data/hotspot_mean_suitability_normalized2.0.tif"
reclamation_path <- "<LOCAL_ANALYSIS_ROOT>/data/data/globalreclamation/reclamation.shp"
old_zone_mean_path <- "<LOCAL_ANALYSIS_ROOT>/data/cityRisk_nonORrecalim/city_risk_mean_by_zone_fromGIS.csv"
updated_mean_path <- file.path(data_dir, "10_03_108city_mean_variables_recheck.csv")

updated_zone_mean_path <- file.path(data_dir, "city_risk_mean_by_zone_fromGIS_recheck.csv")
updated_zone_summary_path <- file.path(data_dir, "10_04analysis_summary_city_mean_by_zone_recheck.csv")
changed_city_zone_stats_path <- file.path(data_dir, "changed_city_reclaimed_vs_nonreclaimed_stats.csv")
changed_zone_geom_path <- file.path(data_dir, "recheck_city_reclaim_nonreclaim_zones.gpkg")
updated_dist_summary_path <- file.path(data_dir, "10_04_108city_pressure_distribution_summary_recheck.csv")

normalize_key <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0.001")
  formatC(p, digits = 3, format = "f")
}

theme_nature <- function(base_size = 11) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.4, colour = "black"),
      axis.ticks = element_line(linewidth = 0.4, colour = "black"),
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      plot.title.position = "plot"
    )
}

save_both <- function(plot_obj, base_name, width, height) {
  pdf_path <- file.path(out_dir, paste0(base_name, ".pdf"))
  jpg_path <- file.path(out_dir, paste0(base_name, ".jpg"))

  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf)

  jpg_saved <- FALSE
  if (requireNamespace("ragg", quietly = TRUE)) {
    jpg_saved <- tryCatch({
      ggsave(jpg_path, plot_obj, width = width, height = height, dpi = 500, device = ragg::agg_jpeg)
      TRUE
    }, error = function(e) {
      message("JPG export via ragg failed for ", base_name, ": ", conditionMessage(e))
      FALSE
    })
  }

  if (!jpg_saved) {
    tryCatch({
      jpeg(jpg_path, width = width, height = height, units = "in", res = 500, quality = 100)
      print(plot_obj)
      dev.off()
    }, error = function(e) {
      if (dev.cur() > 1) {
        try(dev.off(), silent = TRUE)
      }
      message("JPG export skipped for ", base_name, ": ", conditionMessage(e))
    })
  }
}

extract_zone_stats <- function(rast_obj, geom_sf, city_name, zone_name) {
  if (is.null(geom_sf) || nrow(geom_sf) == 0) {
    return(data.frame(
      city = tolower(city_name),
      city_key = normalize_key(city_name),
      zone = zone_name,
      n_pixels = 0L,
      mean_pressure = NA_real_,
      median_pressure = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  v <- terra::vect(geom_sf)
  vals <- terra::extract(rast_obj, v, na.rm = TRUE)
  if (is.null(vals) || nrow(vals) == 0) {
    n_pixels <- 0L
    mean_pressure <- NA_real_
    median_pressure <- NA_real_
  } else {
    value_col <- names(rast_obj)[1]
    x <- vals[[value_col]]
    x <- x[is.finite(x)]
    n_pixels <- length(x)
    mean_pressure <- if (n_pixels > 0) mean(x) else NA_real_
    median_pressure <- if (n_pixels > 0) median(x) else NA_real_
  }

  data.frame(
    city = tolower(city_name),
    city_key = normalize_key(city_name),
    zone = zone_name,
    n_pixels = as.integer(n_pixels),
    mean_pressure = mean_pressure,
    median_pressure = median_pressure,
    stringsAsFactors = FALSE
  )
}

recheck_files <- list.files(recheck_dir, pattern = "\\.shp$", full.names = TRUE)
recheck_index <- data.frame(
  city_name = file_path_sans_ext(basename(recheck_files)),
  city_key = normalize_key(file_path_sans_ext(basename(recheck_files))),
  stringsAsFactors = FALSE
) |>
  arrange(city_name)

cities_sf <- st_read(updated_city_path, quiet = TRUE) |>
  st_make_valid() |>
  mutate(city_key = normalize_key(shapeName))

changed_cities <- cities_sf |>
  filter(city_key %in% recheck_index$city_key)

if (nrow(changed_cities) != nrow(recheck_index)) {
  stop("Some recheck cities could not be matched in 108finalCities_recheck.shp.")
}

risk_rast <- rast(risk_raster_path)
names(risk_rast) <- "pressure"
target_crs <- crs(risk_rast)

reclaim_sf <- st_read(reclamation_path, quiet = TRUE)
reclaim_crs <- st_crs(reclaim_sf)
changed_cities_geo <- st_transform(changed_cities, reclaim_crs)

# Split the global reclamation multipolygon once, then keep only local pieces
# for each changed city. This avoids repeating expensive global intersections.
reclaim_parts <- suppressWarnings(st_cast(reclaim_sf, "POLYGON"))

zone_stats_list <- vector("list", nrow(changed_cities_geo) * 2)
zone_geom_list <- vector("list", nrow(changed_cities_geo) * 2)
idx <- 1L

for (i in seq_len(nrow(changed_cities_geo))) {
  city_i <- changed_cities_geo[i, ]
  city_name <- as.character(city_i$shapeName)
  city_geom <- st_make_valid(city_i)
  cat(sprintf("[%d/%d] %s\n", i, nrow(changed_cities_geo), city_name))

  cand_idx <- lengths(st_intersects(reclaim_parts, city_geom)) > 0
  rec_candidates <- reclaim_parts[cand_idx, ]

  if (nrow(rec_candidates) > 0) {
    rec_i <- suppressWarnings(st_intersection(city_geom, rec_candidates))
    rec_i <- rec_i |>
      st_make_valid() |>
      st_collection_extract("POLYGON", warn = FALSE)
  } else {
    rec_i <- city_geom[0, ]
  }

  if (nrow(rec_i) > 0) {
    rec_union <- st_union(rec_i)
    rec_zone <- st_sf(shapeName = city_name, zone = "Reclamation", geometry = st_sfc(rec_union, crs = st_crs(city_geom)))
    non_geom <- suppressWarnings(st_difference(st_geometry(city_geom), st_geometry(rec_zone)))
    non_zone <- st_sf(shapeName = city_name, zone = "NonReclamation", geometry = non_geom) |>
      st_make_valid() |>
      st_collection_extract("POLYGON", warn = FALSE)
  } else {
    rec_zone <- NULL
    non_zone <- city_geom |>
      mutate(zone = "NonReclamation") |>
      st_make_valid() |>
      st_collection_extract("POLYGON", warn = FALSE)
  }

  rec_zone_rast <- if (!is.null(rec_zone) && nrow(rec_zone) > 0) st_transform(rec_zone, target_crs) else NULL
  non_zone_rast <- if (!is.null(non_zone) && nrow(non_zone) > 0) st_transform(non_zone, target_crs) else NULL

  zone_stats_list[[idx]] <- extract_zone_stats(risk_rast, rec_zone_rast, city_name, "Reclamation")
  zone_geom_list[[idx]] <- if (!is.null(rec_zone) && nrow(rec_zone) > 0) rec_zone else NULL
  idx <- idx + 1L

  zone_stats_list[[idx]] <- extract_zone_stats(risk_rast, non_zone_rast, city_name, "NonReclamation")
  zone_geom_list[[idx]] <- if (!is.null(non_zone) && nrow(non_zone) > 0) non_zone else NULL
  idx <- idx + 1L
}

changed_zone_stats <- bind_rows(zone_stats_list) |>
  mutate(
    mean_pressure = round(mean_pressure, 6),
    median_pressure = round(median_pressure, 6)
  )
write.csv(changed_zone_stats, changed_city_zone_stats_path, row.names = FALSE)

zone_geom_sf <- bind_rows(zone_geom_list)
if (file.exists(changed_zone_geom_path)) {
  unlink(changed_zone_geom_path, force = TRUE)
}
st_write(zone_geom_sf, changed_zone_geom_path, layer = "recheck_city_zones", driver = "GPKG", quiet = TRUE)

old_zone_mean <- read.csv(old_zone_mean_path, stringsAsFactors = FALSE)
old_zone_mean$city_key <- normalize_key(old_zone_mean$city)

updated_mean <- read.csv(updated_mean_path, stringsAsFactors = FALSE)
updated_mean$city_key <- normalize_key(updated_mean$city)

changed_wide <- changed_zone_stats |>
  select(city_key, city, zone, mean_pressure) |>
  mutate(zone = ifelse(zone == "Reclamation", "mean_risk_reclamation", "mean_risk_non_reclamation")) |>
  tidyr::pivot_wider(names_from = zone, values_from = mean_pressure)

replacement_rows <- old_zone_mean |>
  filter(city_key %in% recheck_index$city_key) |>
  select(-mean_risk_non_reclamation, -mean_risk_reclamation, -risk, -area_km2) |>
  left_join(changed_wide, by = "city_key") |>
  left_join(updated_mean |> select(city_key, risk, area_km2), by = "city_key")

updated_zone_mean <- old_zone_mean |>
  filter(!city_key %in% recheck_index$city_key) |>
  bind_rows(replacement_rows) |>
  select(-city_key)

write.csv(updated_zone_mean, updated_zone_mean_path, row.names = FALSE)

df_pair <- updated_zone_mean |>
  filter(!is.na(mean_risk_reclamation), !is.na(mean_risk_non_reclamation)) |>
  mutate(diff = mean_risk_reclamation - mean_risk_non_reclamation)

tt <- t.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE)
wt <- suppressWarnings(wilcox.test(df_pair$mean_risk_reclamation, df_pair$mean_risk_non_reclamation, paired = TRUE, exact = FALSE))
cohens_d <- mean(df_pair$diff, na.rm = TRUE) / sd(df_pair$diff, na.rm = TRUE)

summary_tbl <- tibble::tibble(
  n_cities_paired = nrow(df_pair),
  mean_diff = mean(df_pair$diff, na.rm = TRUE),
  sd_diff = sd(df_pair$diff, na.rm = TRUE),
  t_stat = unname(tt$statistic),
  df_t = unname(tt$parameter),
  p_t = tt$p.value,
  ci_low = tt$conf.int[1],
  ci_high = tt$conf.int[2],
  p_wilcox = wt$p.value,
  cohens_d = cohens_d,
  prop_reclaimed_lower = mean(df_pair$diff < 0, na.rm = TRUE),
  prop_reclaimed_higher = mean(df_pair$diff > 0, na.rm = TRUE)
)
write.csv(summary_tbl, updated_zone_summary_path, row.names = FALSE)

distribution_summary_tbl <- tibble::tibble(
  measure = c("mean_pressure_reclaimed", "mean_pressure_non_reclaimed", "pressure_difference"),
  mean = c(
    mean(df_pair$mean_risk_reclamation, na.rm = TRUE),
    mean(df_pair$mean_risk_non_reclamation, na.rm = TRUE),
    mean(df_pair$diff, na.rm = TRUE)
  ),
  median = c(
    median(df_pair$mean_risk_reclamation, na.rm = TRUE),
    median(df_pair$mean_risk_non_reclamation, na.rm = TRUE),
    median(df_pair$diff, na.rm = TRUE)
  ),
  sd = c(
    sd(df_pair$mean_risk_reclamation, na.rm = TRUE),
    sd(df_pair$mean_risk_non_reclamation, na.rm = TRUE),
    sd(df_pair$diff, na.rm = TRUE)
  ),
  p_value = c(NA_real_, NA_real_, tt$p.value)
)
write.csv(distribution_summary_tbl, updated_dist_summary_path, row.names = FALSE)

plot_df <- df_pair |>
  select(city, mean_risk_reclamation, mean_risk_non_reclamation) |>
  tidyr::pivot_longer(
    cols = c(mean_risk_non_reclamation, mean_risk_reclamation),
    names_to = "zone",
    values_to = "pressure"
  ) |>
  mutate(
    zone = factor(
      zone,
      levels = c("mean_risk_non_reclamation", "mean_risk_reclamation"),
      labels = c("Non-reclaimed", "Reclaimed")
    )
  )

annotation_text <- paste0(
  "Paired t-test P ", fmt_p(tt$p.value),
  "\nMean difference = ", formatC(summary_tbl$mean_diff, digits = 3, format = "f"),
  "\nCohen's d = ", formatC(cohens_d, digits = 2, format = "f")
)

paired_plot <- ggplot(plot_df, aes(x = zone, y = pressure, fill = zone)) +
  geom_line(
    data = df_pair |>
      transmute(city, `Non-reclaimed` = mean_risk_non_reclamation, Reclaimed = mean_risk_reclamation) |>
      tidyr::pivot_longer(-city, names_to = "zone", values_to = "pressure"),
    aes(x = zone, y = pressure, group = city),
    colour = "grey70",
    linewidth = 0.25,
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  geom_boxplot(width = 0.5, linewidth = 0.35, outlier.shape = NA, alpha = 0.9) +
  geom_jitter(shape = 21, stroke = 0.2, size = 1.8, alpha = 0.75, width = 0.08) +
  scale_fill_manual(values = c("Non-reclaimed" = "#c9d6df", "Reclaimed" = "#d98f5c")) +
  labs(
    x = NULL,
    y = "Mean invasion pressure",
    title = "Within-city pressure in reclaimed vs non-reclaimed areas"
  ) +
  annotate("text", x = 1.05, y = max(plot_df$pressure, na.rm = TRUE), label = annotation_text, hjust = 0, vjust = 1, size = 3.2) +
  theme_nature()

scatter_plot <- ggplot(df_pair, aes(x = mean_risk_non_reclamation, y = mean_risk_reclamation)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey50") +
  geom_point(shape = 21, size = 2.1, stroke = 0.25, fill = "#d98f5c", colour = "black", alpha = 0.8) +
  labs(
    x = "Non-reclaimed mean pressure",
    y = "Reclaimed mean pressure",
    title = "City-level comparison of reclaimed and non-reclaimed pressure"
  ) +
  theme_nature()

save_both(paired_plot, "nature_reclaimed_vs_nonreclaimed_paired_recheck", width = 5.0, height = 4.4)
save_both(scatter_plot, "nature_reclaimed_vs_nonreclaimed_scatter_recheck", width = 4.8, height = 4.4)

cat("Updated reclaimed vs non-reclaimed analysis completed.\n")
cat("Updated zone table: ", updated_zone_mean_path, "\n", sep = "")
cat("Summary table: ", updated_zone_summary_path, "\n", sep = "")
