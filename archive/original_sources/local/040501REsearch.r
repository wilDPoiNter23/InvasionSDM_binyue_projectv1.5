# Original source: 040501REsearch.r
# Original source location: <LOCAL_CODE_ROOT>/040501REsearch.r
# Original source SHA-256: c6ff83500425d646c3369a4cc4bb1e2b2f1d32a05b0f2015161d2524853c592c
# Public-safe original snapshot: archive/original_sources/local/040501REsearch.r
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
output_mean <- file.path(base_dir, "results/hotspot_mean_suitability_normalized2.0.tif")
log_file <- file.path(base_dir, "results/2.0log_normalized.xlsx")

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

# --- 如果找不到，尝试更宽松策略：包含物种名，但排除 bin/TSS/filt 文件 ---
if (length(tif_file) == 0) {
  all_grd <- list.files(
    search_dir,
    pattern = "\\.(grd|tif)$",
    full.names = TRUE,
    recursive = TRUE
  )

  tif_file <- all_grd[
    grepl(sp_dot, basename(all_grd)) &
    !grepl("bin|TSS|filt", basename(all_grd), ignore.case = TRUE)
  ]

  if (length(tif_file) > 0) {
    cat("⚠️ Used relaxed fallback match:", sp, "\n")
  }
}

# --- 如果仍未找到，记录失败 ---
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