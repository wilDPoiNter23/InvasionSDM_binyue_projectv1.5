# Original source: 0405Allstack.r
# Original source location: <LOCAL_CODE_ROOT>/0405Allstack.r
# Original source SHA-256: 1377265b93531f895ecac92e080b432fdc26267b81c4c4029c1feb9269d8a729
# Public-safe original snapshot: archive/original_sources/local/0405Allstack.r
# Classification: SUPPORTING
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(terra)
library(openxlsx)

# 路径设置
base_dir <- "<WORK_ROOT>"
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_mean <- file.path(base_dir, "results/hotspot_mean_suitability_normalized.tif")
log_file <- file.path(base_dir, "results/stack_failed_species_log_normalized.xlsx")

# 读取 summary 表
summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

# 初始化
fail_log <- data.frame(species = character(), reason = character())
r_sum <- NULL
count <- 0
normalize_raster <- function(r) {
  mm <- terra::minmax(r)
  rmin <- mm[1]
  rmax <- mm[2]
  if (is.na(rmin) || is.na(rmax) || rmin == rmax) {
    return(r * 0)  # 避免异常情况
  }
  return((r - rmin) / (rmax - rmin))
}


for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  cat(sprintf("[%03d/%03d] Processing %s (%s)\n", i, nrow(summary_df), sp, proj_type))

  # RDS 路径
  rds_path <- file.path(proj_dir, paste0(sp, "_", proj_type, "_projection.rds"))
  rds <- tryCatch(readRDS(rds_path), error = function(e) NULL)

  if (is.null(rds) || !inherits(rds, "BIOMOD.projection.out")) {
    cat("❌ Failed:", sp, "(Invalid RDS)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Invalid RDS"))
    next
  }

  model_name <- rds@models.projected[1]
  search_dir <- file.path(model_dir, sp_dot)

  # 递归找匹配的 .tif 或 .grd 文件
  tif_file <- list.files(
    search_dir,
    pattern = paste0(model_name, ".*\\.(tif|grd)$"),
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(tif_file) == 0) {
    cat("❌ Failed:", sp, "(Prediction raster not found)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Prediction raster not found"))
    next
  }

  r <- tryCatch(terra::rast(tif_file[1]), error = function(e) NULL)
  if (is.null(r)) {
    cat("❌ Failed:", sp, "(Raster read error)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Raster read error"))
    next
  }

  # Min-Max 归一化
  r <- normalize_raster(r)
  
  # 将所有 rasters 统一为 double 类型
r <- terra::ifel(is.na(r), 0, r)

if (is.null(r_sum)) {
  r_sum <- r
} else {
  tryCatch({
    r_sum <- terra::cover(r_sum + r, r_sum)  # NA-safe 累加
  }, error = function(e) {
    cat("❌ Failed:", sp, "(Dimension mismatch)\n")
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Dimension mismatch"))
  })
}

  count <- count + 1
  cat("✅ Success:", sp, "\n")
}

# 写出平均图层
if (!is.null(r_sum) && count > 0) {
  r_mean <- r_sum / count
  terra::writeRaster(r_mean, output_mean, overwrite = TRUE)
  cat("✅ Mean suitability map saved to:", output_mean, "\n")
} else {
  cat("⚠️ No valid rasters to average.\n")
}

# 写出失败日志
if (nrow(fail_log) > 0) {
  openxlsx::write.xlsx(fail_log, file = log_file, overwrite = TRUE)
  cat("⚠️ Failures logged to:", log_file, "\n")
}