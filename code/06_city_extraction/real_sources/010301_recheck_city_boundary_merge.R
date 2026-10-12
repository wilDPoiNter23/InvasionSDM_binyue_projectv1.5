# Original source: 010301_recheck_city_boundary_merge.R
# Original source location: <LOCAL_CODE_ROOT>/010301_recheck_city_boundary_merge.R
# Original source SHA-256: 26a3c35e32f5bbae47ae9c6cd59b524efb91709e4109a52450b01f0f6f8d7361
# Public-safe original snapshot: archive/original_sources/local/010301_recheck_city_boundary_merge.R
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(sf)
library(dplyr)
library(tools)

adm1_dir <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/research_cities/adm1")
older_dir <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/research_cities/olderVersion")
out_dir <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/research_cities/recheck")
template_path <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities.shp")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

normalize_key <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

list_city_shps <- function(folder, source_label) {
  shp_files <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  data.frame(
    source = source_label,
    path = shp_files,
    file_name = basename(shp_files),
    city_name = file_path_sans_ext(basename(shp_files)),
    city_key = normalize_key(file_path_sans_ext(basename(shp_files))),
    stringsAsFactors = FALSE
  )
}

template_name_candidates <- function(x) {
  candidates <- c(
    if ("shapeName" %in% names(x)) as.character(x$shapeName) else NA_character_,
    if ("city" %in% names(x)) as.character(x$city) else NA_character_
  )
  candidates <- candidates[!is.na(candidates) & nzchar(trimws(candidates))]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[1]
}

delete_existing_shapefile <- function(shp_path) {
  stem <- file_path_sans_ext(shp_path)
  sidecars <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix")
  existing <- paste0(stem, sidecars)
  file.remove(existing[file.exists(existing)])
}

prepare_one_source <- function(path, source_label, city_name, target_crs = NULL) {
  x <- st_read(path, quiet = TRUE)
  x <- st_make_valid(x)

  if (!is.null(target_crs) && !is.na(target_crs) && !is.na(st_crs(x))) {
    x <- st_transform(x, target_crs)
  }

  x |>
    transmute(
      city_name = city_name,
      source = source_label,
      geometry = geometry
    )
}

build_blank_template_row <- function(template_sf, city_name, geom_sfc, out_path) {
  blank <- template_sf[1, ]
  for (nm in setdiff(names(blank), attr(blank, "sf_column"))) {
    blank[[nm]] <- NA
  }

  if ("shapeName" %in% names(blank)) {
    blank$shapeName <- city_name
  }
  if ("path" %in% names(blank)) {
    blank$path <- out_path
  }

  st_geometry(blank) <- geom_sfc
  blank
}

adm1_index <- list_city_shps(adm1_dir, "adm1")
older_index <- list_city_shps(older_dir, "olderVersion")
template_sf <- st_read(template_path, quiet = TRUE)
template_sf <- st_make_valid(template_sf)
template_keep_fields <- intersect(
  c("shapeName", "shapeID", "shapeGroup", "shapeType", "layer", "path", "city", "country"),
  names(template_sf)
)
template_sf <- template_sf[, c(template_keep_fields, attr(template_sf, "sf_column"))]
template_index <- data.frame(
  template_row = seq_len(nrow(template_sf)),
  template_name = vapply(seq_len(nrow(template_sf)), function(i) {
    template_name_candidates(template_sf[i, ])
  }, character(1)),
  stringsAsFactors = FALSE
) |>
  mutate(city_key = normalize_key(template_name))

if (any(duplicated(adm1_index$city_key))) {
  stop("Duplicate city names detected in adm1 directory after normalization.")
}

if (any(duplicated(older_index$city_key))) {
  stop("Duplicate city names detected in olderVersion directory after normalization.")
}

if (any(duplicated(template_index$city_key[!is.na(template_index$city_key)]))) {
  stop("Duplicate city names detected in 108finalCities.shp after normalization.")
}

city_index <- full_join(
  adm1_index |> rename_with(~ paste0(., "_adm1"), -city_key),
  older_index |> rename_with(~ paste0(., "_old"), -city_key),
  by = "city_key"
) |>
  left_join(template_index |> select(city_key, template_row), by = "city_key") |>
  mutate(
    output_name = coalesce(city_name_adm1, city_name_old),
    merge_status = case_when(
      !is.na(path_adm1) & !is.na(path_old) ~ "paired_merge",
      !is.na(path_adm1) & is.na(path_old) ~ "adm1_only",
      is.na(path_adm1) & !is.na(path_old) ~ "older_only",
      TRUE ~ "missing"
    )
  ) |>
  filter(merge_status != "missing") |>
  arrange(output_name)

merge_log <- vector("list", nrow(city_index))

for (i in seq_len(nrow(city_index))) {
  row_i <- city_index[i, ]
  city_name <- row_i$output_name
  message(sprintf("[%02d/%02d] Processing %s (%s)", i, nrow(city_index), city_name, row_i$merge_status))

  source_parts <- list()

  if (!is.na(row_i$path_adm1)) {
    sf_adm1 <- prepare_one_source(row_i$path_adm1, "adm1", city_name)
    target_crs <- st_crs(sf_adm1)
    source_parts[["adm1"]] <- sf_adm1
  } else {
    target_crs <- NULL
  }

  if (!is.na(row_i$path_old)) {
    sf_old <- prepare_one_source(row_i$path_old, "olderVersion", city_name, target_crs = target_crs)
    source_parts[["olderVersion"]] <- sf_old
  }

  merged_sf <- bind_rows(source_parts)

  out_path <- file.path(out_dir, paste0(city_name, ".shp"))
  source_combo <- paste(sort(unique(merged_sf$source)), collapse = "+")
  input_features <- nrow(merged_sf)
  merged_geom <- st_sfc(st_union(st_geometry(merged_sf)), crs = st_crs(merged_sf))

  if (!is.na(row_i$template_row)) {
    aligned_one <- template_sf[row_i$template_row, ]
    st_geometry(aligned_one) <- merged_geom
    if ("path" %in% names(aligned_one)) {
      aligned_one$path <- out_path
    }
  } else {
    aligned_one <- build_blank_template_row(template_sf, city_name, merged_geom, out_path)
  }

  out_path <- file.path(out_dir, paste0(city_name, ".shp"))
  delete_existing_shapefile(out_path)
  Sys.setenv(SHAPE_RESTORE_SHX = "YES")
  st_write(
    aligned_one,
    dsn = out_dir,
    layer = city_name,
    driver = "ESRI Shapefile",
    delete_layer = TRUE,
    quiet = TRUE
  )

  merge_log[[i]] <- data.frame(
    city_name = city_name,
    city_key = row_i$city_key,
    merge_status = row_i$merge_status,
    source_combo = source_combo,
    input_features = input_features,
    template_matched = !is.na(row_i$template_row),
    path_adm1 = row_i$path_adm1,
    path_old = row_i$path_old,
    output_path = out_path,
    stringsAsFactors = FALSE
  )
}

merge_log_df <- bind_rows(merge_log)
write.csv(merge_log_df, file.path(out_dir, "recheck_merge_log.csv"), row.names = FALSE)

message("Done. Output folder: ", out_dir)
message("Merge log written to: ", file.path(out_dir, "recheck_merge_log.csv"))
