# Original source: 050101cliptocontinent.r
# Original source location: <LOCAL_CODE_ROOT>/050101cliptocontinent.r
# Original source SHA-256: cc8956fbf5fcb5f18292113379207d7cd8df1e8a5087803e62d5ad98394252c1
# Public-safe original snapshot: archive/original_sources/local/050101cliptocontinent.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
# ==== 📚 加载必要包 ====
packages <- c("terra", "sf", "rnaturalearth", "dplyr")
lapply(packages, function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg))
lapply(packages, library, character.only = TRUE)

# ==== 📂 读取三等级入侵风险栅格 ====
r <- rast(publication_path("WORK_ROOT", "results/Fisher-Jenks Classification.tif"))

# ==== 🔢 分类为 1 = Low, 2 = Medium, 3 = High ====
r_classified <- r
# ==== 🌍 获取大陆边界 ====
continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()

# ==== 🌐 投影统一（Mollweide） ====
moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
r_proj <- project(r_classified, moll_crs)
continents_proj <- st_transform(continents, crs(r_proj))

# ==== 💾 输出目录 ====
output_dir <- publication_path("WORK_ROOT", "results/continent_risk_tifs")
dir.create(output_dir, showWarnings = FALSE)

# ==== 🎯 每个大陆裁剪并保存为 .tif ====
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

  # ==== 💾 保存为 GeoTIFF ====
  tif_path <- file.path(output_dir, paste0(gsub(" ", "_", cont_name), "_risk.tif"))
  writeRaster(r_crop, tif_path, overwrite = TRUE)

  cat("✅ Saved:", tif_path, "\n")
}

cat("🎉 All continent raster exports completed.\n")
