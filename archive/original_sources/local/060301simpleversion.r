# Original source: 060301simpleversion.r
# Original source location: <LOCAL_CODE_ROOT>/060301simpleversion.r
# Original source SHA-256: 725f0685c3a845332fe5ac9614f2cfa5c4307b5e261bf1f3e6e742481e91b489
# Public-safe original snapshot: archive/original_sources/local/060301simpleversion.r
# Classification: EXPLORATORY
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
library(data.table)

#====1. 加载数据 =====
cat("Loading data...\n")

risk_raster <- terra::rast("<WORK_ROOT>/results/hotspot_mean_suitability_normalized2.0.tif")  # or .tif
city_boundaries_shp <- st_read("<WORK_ROOT>/data/analysis/108finalCities.shp")
city_reclamation_shp <- st_read("<WORK_ROOT>/data/analysis/reclaimed_clippedbyQgis.shp")
#==== 2. 确保CRS一致===
cat("Aligning CRS...\n")
target_crs <- crs(risk_raster)
city_reclamation_shp <- st_transform(city_reclamation_shp, target_crs)
city_boundaries_shp <- st_transform(city_boundaries_shp, target_crs)


#=====3. 获取所有城市名称=====
all_cities <- unique(city_boundaries_shp$shapeName)
cat(sprintf("Found %d cities to process\n", length(all_cities)))
# 预先：按城市 dissolve（每城仅一个几何），后面不再 st_union
sf::sf_use_s2(FALSE)  # 常更稳更快（可保留）
rec_by_city <- city_reclamation_shp |>
  dplyr::group_by(shapeName) |>
  dplyr::summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()



val_col <- names(risk_raster)[1]

# ==== 4) 稳健版提取函数（变量名不变）====
extract_city_risks <- function(city_name) {
  # ---- 辅助：安全提取 ----
  safe_extract_vals <- function(poly) {
    if (is.null(poly) || length(poly) == 0 || sf::st_is_empty(poly)) return(numeric(0))
    poly <- sf::st_collection_extract(sf::st_make_valid(poly), "POLYGON")
    if (length(poly) == 0 || sf::st_is_empty(poly)) return(numeric(0))

    df <- try(terra::extract(risk_raster, terra::vect(poly)), silent = TRUE)
    if (inherits(df, "try-error") || is.null(df) || !is.data.frame(df) || ncol(df) < 2) return(numeric(0))

    vals <- if (!is.null(val_col) && val_col %in% names(df)) df[[val_col]] else unlist(df[, -1, drop = FALSE], use.names = FALSE)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) numeric(0) else as.numeric(vals)
  }

  cat(sprintf("Processing: %s\n", city_name))

  # 城市边界
  city_boundary <- city_boundaries_shp[city_boundaries_shp$shapeName == city_name, ]
  if (nrow(city_boundary) == 0) { cat("  ! No boundary found\n"); return(NULL) }
  city_boundary <- st_collection_extract(st_make_valid(city_boundary), "POLYGON")
  if (length(city_boundary) == 0 || st_is_empty(city_boundary)) { cat("  ! Boundary invalid/empty\n"); return(NULL) }

  # 预处理后的围填海几何（每城1个 geometry）
  rec_city <- rec_by_city[rec_by_city$shapeName == city_name, ]
  has_rec <- nrow(rec_city) > 0 && !st_is_empty(rec_city)

  # 先裁后差（避免拓扑坑）
  reclamation_risks <- numeric(0)
  non_reclamation_risks <- numeric(0)

  if (has_rec) {
    rec_union <- rec_city$geometry

    # 围填海：城市内的围填海
    rec_in_city <- try(st_intersection(city_boundary, rec_union), silent = TRUE)
    if (!inherits(rec_in_city, "try-error") && !st_is_empty(rec_in_city)) {
      rec_in_city <- st_collection_extract(st_make_valid(rec_in_city), "POLYGON")
      reclamation_risks <- safe_extract_vals(rec_in_city)
    }

    # 非围填海：城市 - (城市内的围填海)
    non_rec_area <- try(
      st_difference(st_buffer(city_boundary, 0), st_buffer(st_union(rec_in_city), 0)),
      silent = TRUE
    )
    if (!inherits(non_rec_area, "try-error") && !is.null(non_rec_area) && !st_is_empty(non_rec_area)) {
      non_rec_area <- st_collection_extract(st_make_valid(non_rec_area), "POLYGON")
      non_reclamation_risks <- safe_extract_vals(non_rec_area)
    }
  } else {
    # 无围填海：全为 NonReclamation
    non_reclamation_risks <- safe_extract_vals(city_boundary)
  }

  # 诊断（可选）
  # diag_city(city_name, city_boundary, if (has_rec) rec_city else st_sfc(crs = st_crs(city_boundary)),
  #           if (exists("non_rec_area")) non_rec_area else st_sfc(crs = st_crs(city_boundary)))

  res <- data.table::data.table(
    city = city_name,
    zone = c(rep("Reclamation", length(reclamation_risks)),
             rep("NonReclamation", length(non_reclamation_risks))),
    risk = c(reclamation_risks, non_reclamation_risks)
  )

  cat(sprintf("  ✓ Extracted %d reclamation + %d non-reclamation pixels\n",
              sum(res$zone == "Reclamation"), sum(res$zone == "NonReclamation")))
  res
}

# ==== 5) 循环处理 ====
cat("Starting extraction...\n")
start_time <- Sys.time()
res_list <- vector("list", length(all_cities))

for (i in seq_along(all_cities)) {
  city_name <- all_cities[i]
  cat(sprintf("[%d/%d] ", i, length(all_cities)))
  ans <- try(extract_city_risks(city_name), silent = TRUE)
  if (inherits(ans, "try-error") || is.null(ans)) {
    warning(sprintf(" -> failed: %s", city_name))
    res_list[[i]] <- data.table(city = city_name, zone = NA_character_, risk = NA_real_)
  } else {
    res_list[[i]] <- ans
  }
}
final_data <- rbindlist(res_list, use.names = TRUE, fill = TRUE)

# ==== 6) 城市级统计（含“均值”）====
# 1) 每个城市整体均值（所有像元）
city_mean_all <- final_data[, .(mean_risk = round(mean(risk, na.rm = TRUE), 4)), by = city]

# 2) 每城分区均值（围填海 / 非围填海）
city_mean_by_zone <- final_data[, .(mean_risk = round(mean(risk, na.rm = TRUE), 4)), by = .(city, zone)]
city_mean_wide <- dcast(city_mean_by_zone, city ~ zone, value.var = "mean_risk")

# 3) 汇总到一个总表（像素计数、比例、中位数等可保留）
city_stats <- final_data[, .(
  total_pixels = sum(!is.na(risk)),
  reclamation_pixels = sum(zone == "Reclamation", na.rm = TRUE),
  non_reclamation_pixels = sum(zone == "NonReclamation", na.rm = TRUE),
  reclamation_percentage = round(sum(zone == "Reclamation", na.rm = TRUE) / sum(!is.na(risk)) * 100, 2),
  mean_risk = round(mean(risk, na.rm = TRUE), 4),
  median_risk = round(median(risk, na.rm = TRUE), 4),
  mean_risk_reclamation = round(mean(risk[zone == "Reclamation"], na.rm = TRUE), 4),
  mean_risk_non_reclamation = round(mean(risk[zone == "NonReclamation"], na.rm = TRUE), 4),
  risk_difference = round(mean(risk[zone == "Reclamation"], na.rm = TRUE) - 
                          mean(risk[zone == "NonReclamation"], na.rm = TRUE), 4)
), by = city]

# 4) 把“整体均值”和“分区均值”合并进 city_stats（可选，便于一表用）
city_stats <- merge(city_stats, city_mean_wide, by = "city", all.x = TRUE, suffixes = c("", "_zone_mean"))
# 为避免同名冲突，给分区列起更清晰的名字
setnames(city_stats, old = c("NonReclamation", "Reclamation"),
                   new = c("mean_risk_non_reclamation_zone", "mean_risk_reclamation_zone"), skip_absent = TRUE)

# ==== 7) 输出与简报 ====
end_time <- Sys.time()
elapsed <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

cat("\n=== RESULTS ===\n")
cat(sprintf("Processing time: %s minutes\n", elapsed))
cat(sprintf("Total pixels extracted: %d\n", sum(!is.na(final_data$risk))))
cat(sprintf("Cities processed: %d\n", length(unique(final_data$city))))
cat(sprintf("Reclamation pixels: %d (%.1f%%)\n",
            sum(final_data$zone == "Reclamation", na.rm = TRUE),
            100 * mean(final_data$zone == "Reclamation", na.rm = TRUE)))

# 保存
fwrite(final_data, "<WORK_ROOT>/data/analysis/city_risk_pixel_data.csv")
fwrite(city_stats, "<WORK_ROOT>/data/analysis/city_risk_summary.csv")
fwrite(city_mean_all, "<WORK_ROOT>/data/analysis/city_risk_mean_overall.csv")
fwrite(city_mean_wide, "<WORK_ROOT>/data/analysis/city_risk_mean_by_zone.csv")

cat("Files saved:\n")
cat("  - city_risk_pixel_data.csv (pixel-level data)\n")
cat("  - city_risk_summary.csv (city-level statistics + 分区均值)\n")
cat("  - city_risk_mean_overall.csv (每城整体均值)\n")
cat("  - city_risk_mean_by_zone.csv (每城分区均值，宽表)\n")
cat("\nDone! 🎉\n")