# Original source: 010302_apply_recheck_to_108finalCities.R
# Original source location: <LOCAL_CODE_ROOT>/010302_apply_recheck_to_108finalCities.R
# Original source SHA-256: 2603b29cadaf8b32e3382c60d836db0559ed45c302ad00e9bde6943bc9717f49
# Public-safe original snapshot: archive/original_sources/local/010302_apply_recheck_to_108finalCities.R
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

recheck_dir <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/research_cities/recheck")
master_path <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities.shp")
out_gpkg <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities_recheck.gpkg")
out_shp <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities_recheck.shp")
out_log <- publication_path("LOCAL_ANALYSIS_ROOT", "data/data/108finalCities_recheck_log.csv")

normalize_key <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

delete_existing_shapefile <- function(shp_path) {
  stem <- file_path_sans_ext(shp_path)
  sidecars <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix")
  existing <- paste0(stem, sidecars)
  unlink(existing[file.exists(existing)], force = TRUE)
}

na_like <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(NA))
  }
  if (is.integer(x)) {
    return(NA_integer_)
  }
  if (is.numeric(x)) {
    return(NA_real_)
  }
  if (is.logical(x)) {
    return(NA)
  }
  NA_character_
}

list_recheck_shps <- function(folder) {
  shp_files <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  data.frame(
    path = shp_files,
    city_name = file_path_sans_ext(basename(shp_files)),
    city_key = normalize_key(file_path_sans_ext(basename(shp_files))),
    stringsAsFactors = FALSE
  ) |>
    arrange(city_name)
}

read_recheck_geom <- function(path, target_crs) {
  x <- st_read(path, quiet = TRUE)
  x <- st_make_valid(x)
  if (!is.na(st_crs(x)) && st_crs(x) != target_crs) {
    x <- st_transform(x, target_crs)
  }
  st_sfc(st_union(st_geometry(x)), crs = target_crs)
}

build_blank_row <- function(master_sf, city_name, orig_order, shp_path) {
  blank <- master_sf[1, ]
  for (nm in setdiff(names(blank), attr(blank, "sf_column"))) {
    if (nm == "orig_order") {
      blank[[nm]] <- orig_order
    } else {
      blank[[nm]] <- na_like(blank[[nm]])
    }
  }
  if ("shapeName" %in% names(blank)) {
    blank[["shapeName"]] <- city_name
  }
  if ("path" %in% names(blank)) {
    blank[["path"]] <- shp_path
  }
  blank
}

recheck_index <- list_recheck_shps(recheck_dir)
master_sf <- st_read(master_path, quiet = TRUE)
master_sf <- st_make_valid(master_sf)
master_sf$orig_order <- seq_len(nrow(master_sf))

if (!"shapeName" %in% names(master_sf)) {
  stop("shapeName field not found in 108finalCities.shp.")
}

master_index <- data.frame(
  master_row = seq_len(nrow(master_sf)),
  shapeName = as.character(master_sf[["shapeName"]]),
  city_key = normalize_key(as.character(master_sf[["shapeName"]])),
  orig_order = master_sf$orig_order,
  stringsAsFactors = FALSE
)

if (any(duplicated(recheck_index$city_key))) {
  stop("Duplicate city names detected in recheck directory after normalization.")
}

if (any(duplicated(master_index$city_key))) {
  stop("Duplicate shapeName values detected in 108finalCities.shp after normalization.")
}

replace_keys <- recheck_index$city_key
master_kept <- master_sf[!(master_index$city_key %in% replace_keys), ]

replacement_rows <- vector("list", nrow(recheck_index))
replacement_log <- vector("list", nrow(recheck_index))
next_order <- max(master_sf$orig_order, na.rm = TRUE)

for (i in seq_len(nrow(recheck_index))) {
  row_i <- recheck_index[i, ]
  message(sprintf("[%02d/%02d] Replacing %s", i, nrow(recheck_index), row_i$city_name))

  match_idx <- which(master_index$city_key == row_i$city_key)
  geom_i <- read_recheck_geom(row_i$path, st_crs(master_sf))

  if (length(match_idx) == 1) {
    new_row <- master_sf[match_idx, ]
    matched_shape <- master_index$shapeName[match_idx]
    match_status <- "matched_existing"
  } else {
    next_order <- next_order + 1
    new_row <- build_blank_row(master_sf, row_i$city_name, next_order, row_i$path)
    matched_shape <- NA_character_
    match_status <- "new_city_not_in_master"
  }

  if ("shapeName" %in% names(new_row)) {
    new_row[["shapeName"]] <- row_i$city_name
  }
  if ("path" %in% names(new_row)) {
    new_row[["path"]] <- row_i$path
  }
  st_geometry(new_row) <- geom_i

  replacement_rows[[i]] <- new_row
  replacement_log[[i]] <- data.frame(
    city_name = row_i$city_name,
    city_key = row_i$city_key,
    match_status = match_status,
    matched_shapeName = matched_shape,
    recheck_path = row_i$path,
    stringsAsFactors = FALSE
  )
}

updated_master <- bind_rows(master_kept, bind_rows(replacement_rows)) |>
  arrange(orig_order)

replacement_log_df <- bind_rows(replacement_log)
write.csv(replacement_log_df, out_log, row.names = FALSE)

if (file.exists(out_gpkg)) {
  unlink(out_gpkg, force = TRUE)
}
st_write(
  updated_master |> select(-orig_order),
  dsn = out_gpkg,
  layer = "108finalCities_recheck",
  driver = "GPKG",
  quiet = TRUE
)

ascii_fields <- intersect(
  c("shapeName", "shapeID", "shapeGroup", "shapeType", "layer", "path", "city", "country"),
  names(updated_master)
)
updated_master_ascii <- updated_master[, c(ascii_fields, attr(updated_master, "sf_column"))]
delete_existing_shapefile(out_shp)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
st_write(
  updated_master_ascii,
  dsn = dirname(out_shp),
  layer = file_path_sans_ext(basename(out_shp)),
  driver = "ESRI Shapefile",
  quiet = TRUE
)

message("Done.")
message("Full output written to: ", out_gpkg)
message("ASCII shapefile written to: ", out_shp)
message("Replacement log written to: ", out_log)
