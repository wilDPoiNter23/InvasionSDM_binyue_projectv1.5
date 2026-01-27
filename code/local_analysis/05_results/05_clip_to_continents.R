# ==== 📚 Load required packages ====
packages <- c("terra", "sf", "rnaturalearth", "dplyr")
lapply(packages, function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg))
lapply(packages, library, character.only = TRUE)

# ==== 📂 Read three-level invasion risk raster ====
r <- rast("/thfs1/home/qs_songsj1/invasionSDM_binyue/results/Fisher-Jenks Classification.tif")

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
output_dir <- "/thfs1/home/qs_songsj1/invasionSDM_binyue/results/continent_risk_tifs"
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
