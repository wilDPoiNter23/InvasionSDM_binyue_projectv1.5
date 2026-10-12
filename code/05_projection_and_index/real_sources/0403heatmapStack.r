# Original source: 0403heatmapStack.r
# Original source location: <LOCAL_CODE_ROOT>/0403heatmapStack.r
# Original source SHA-256: 1416547540fc20b83d06c9217a419a7e5785b491a624fcb58222dc1069015df8
# Public-safe original snapshot: archive/original_sources/local/0403heatmapStack.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
library(terra)
library(pbapply)

# 设置路径
proj_dir <- publication_path("LOCAL_AUX_ROOT", "projections")
rds_files <- list.files(proj_dir, pattern = "\\.rds$", full.names = TRUE)
rds_files
str(readRDS(publication_path("LOCAL_AUX_ROOT", "projections/Veronica.persica_ensemble_projection.rds")))
proj_obj <- readRDS(publication_path("LOCAL_AUX_ROOT", "projections/Veronica.persica_ensemble_projection.rds"))


ras <- rast(proj_obj@proj.out@link)


rds_files <- list.files(proj_dir, pattern = "\\.rds$", full.names = TRUE)

# 初始化空列表与进度条
raster_list <- list()
total <- length(rds_files)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in seq_along(rds_files)) {
  rds_file <- rds_files[i]
  message("🔄 正在处理: ", basename(rds_file))
  
  proj_obj <- tryCatch(readRDS(rds_file), error = function(e) NULL)
  
  if (!is.null(proj_obj) && "BIOMOD.Projection" %in% class(proj_obj)) {
    raster_layers <- tryCatch({
      # proj@proj@val 存储栅格（注意结构可能不同）
      ras <- rast(proj_obj@proj@val)
      readAll(ras)  # 💡 预读至内存
    }, error = function(e) {
      message("⚠️ 栅格转换失败: ", basename(rds_file))
      NULL
    })
    
    if (!is.null(raster_layers)) {
      sp_name <- gsub("\\.rds$", "", basename(rds_file))
      names(raster_layers) <- paste0(sp_name, "_", seq_len(nlyr(raster_layers)))
      raster_list[[sp_name]] <- raster_layers
    }
  } else {
    message("⚠️ 无法读取 RDS 或对象结构异常: ", basename(rds_file))
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)

# 合并所有 SpatRaster
if (length(raster_list) > 0) {
  all_species_stack <- do.call(c, raster_list)
  
  # 保存为 GeoTIFF 栅格堆栈
  output_file <- file.path(proj_dir, "all_species_projection_stack.tif")
  writeRaster(
    all_species_stack,
    output_file,
    overwrite = TRUE,
    datatype = "FLT4S"  # 32-bit float
  )
  
  message("✅ 所有物种投影已合并并保存为: ", output_file)
} else {
  message("❌ 没有成功加载任何投影栅格")
}