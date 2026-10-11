# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: 6a7854f83cc0e2a1e966a2840cc59060eea008d01f7ec5e07d9503901ef9c7df
# ==== 📚 Load required packages ====
packages <- c("terra", "sf", "rnaturalearth", "dplyr")
lapply(packages, function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg))
lapply(packages, library, character.only = TRUE)

# ==== 📂 Read three-level invasion risk raster ====
r <- rast("${WORK_ROOT}/results/Fisher-Jenks Classification.tif")

# ==== 🔢 Classify as 1 = Low, 2 = Medium, 3 = High ====
r_classified <- r
# ==== 🌍 Get continent boundaries ====
continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()

# ==== 🌐 Unify projection (Mollweide) ====
moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
r_proj <- project(r_classified, moll_crs)
continents_proj <- st_transform(continents, crs(r_proj))

# ==== 💾 Output directory ====
output_dir <- "${WORK_ROOT}/results/continent_risk_tifs"
dir.create(output_dir, showWarnings = FALSE)

# ==== 🎯 Clip each continent and save as .tif ====
for (i in 1:nrow(continents_proj)) {
  cont <- continents_proj[i, ]
  cont_name <- cont$continent

  if (cont_name %in% c("Antarctica", "Seven seas (open ocean)")) {
    cat("🚫 Skipping:", cont_name, "\n")
    next
  }

  cat("🔎 Cropping:", cont_name, "\n")

  r_crop <- try(mask(crop(r_proj, vect(cont)), vect(cont)), silent = TRUE)
  if (inherits(r_crop, "try-error")) {
    cat("⚠️ Failed to crop:", cont_name, "\n")
    next
  }

  # ==== 💾 Save as GeoTIFF ====
  tif_path <- file.path(output_dir, paste0(gsub(" ", "_", cont_name), "_risk.tif"))
  writeRaster(r_crop, tif_path, overwrite = TRUE)

  cat("✅ Saved:", tif_path, "\n")
}

cat("🎉 All continent raster exports completed.\n")
