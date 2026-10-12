# Original source: 02environmental data.R
# Original source location: <LOCAL_CODE_ROOT>/02environmental data.R
# Original source SHA-256: 65c1175711fd4922e3fb7df5001af74bc6fb6237ae1a85f55025f55152e08dd5
# Public-safe original snapshot: archive/original_sources/local/02environmental data.R
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
setwd(publication_path("LOCAL_ANALYSIS_ROOT", "data"))
library(devtools)
library(pbapply)
library(geodata)
library(terra)
library(sf)
pboptions(type = "timer")

# 下载土壤数据
target_vars <- c(
  "bdod",     # 土壤容重
  "cfvo",     # 粗碎屑体积
  "clay",     # 黏土含量
  "nitrogen", # 氮含量
  "ocd",      # 有机碳密度
  "phh2o",    # pH值
  "sand",     # 砂土含量
  "silt",     # 粉砂含量
  "soc"       # 土壤有机碳
)
output_dir <- "data/"

download_soil_fill <- function(var) {
  tryCatch({
    # 下载数据（默认包含填充后的均值）
    soil_data <- soil_world(
      var = var,          # 土壤变量
      depth = 15,   # 指定深度
      path = output_dir,  # 保存路径
      download = TRUE     # 强制下载最新数据
    )
    
    # 检查文件是否生成
    expected_file <- file.path(output_dir, paste0(var, "_5-15cm_fill.tif"))
    if (file.exists(expected_file)) {
      message("成功下载: ", var)
      return(TRUE)
    } else {
      message("文件未生成: ", var)
      return(FALSE)
    }
  }, error = function(e) {
    message("下载失败: ", var, " - ", e$message)
    return(FALSE)
  })
}

results <- sapply(target_vars, download_soil_fill)

# 打印总结
cat("\n下载完成！成功下载文件数:", sum(results), "/", length(target_vars))



# 下载气候数据
climate_vars <- c('tmin', 'tmax', 'prec')
climate_resolutions <- c(2.5)

worldclim_data <- pblapply(climate_vars, function(var) {
  pblapply(climate_resolutions, function(res) {
    getData('worldclim', var = var, res = res, path = 'data/')
  })
})

elev <- rast("data/Topo/wc2.1_2.5m_elev.tif")
aspect <- terrain(elev, v = "aspect", unit = "degrees")
writeRaster(aspect, "data/Topo/aspect.tif", overwrite = TRUE)
slope <- terrain(elev, v = "slope", unit = "degrees")
writeRaster(slope, "data/Topo/slope.tif", overwrite = TRUE)
ext(aspect)
res(elev)
elev_global <- elevation_global(res = 2.5, path = "data/Topo")
aspect <- terrain(elev_global, v = "aspect", unit = "degrees")



# resample population data from 30 arc seconds to 2.5 mins
r30s <- rast('data/humanbeing/buildup2025/BUILT_S_E2025_30ss.tif')
r2_5m_template <- rast('data/Topo/wc2.1_2.5m_elev.tif')

# Resample using bilinear interpolation (for continuous data)
r2_5m_resampled <- resample(r30s, r2_5m_template, method = "bilinear")
print(names(r2_5m_resampled))
 res(r2_5m_resampled)
names(r2_5m_resampled) <- "BUILT_S_E2025_2.5min"
writeRaster(r2_5m_resampled, "data/humanbeing/buildup2025/BUILT_S_E2025_2.5min.tif", overwrite = TRUE)

# 设置路径
input_dir <- "data/soil_world"  # 替换为输入目录
output_dir <- "data/soilworld0-15cm_2.5mins"  # 替换为输出目录

depth1_files <- list.files(
  path = input_dir,
  pattern = "_0-5cm_mean_30s\\.tif$",
  full.names = TRUE
)
var_names <- gsub("_0-5cm_mean_30s\\.tif$", "", basename(depth1_files))

for (var in var_names) {
  # 构建两个深度层的文件路径
  depth1_file <- file.path(input_dir, paste0(var, "_0-5cm_mean_30s.tif"))
  depth2_file <- file.path(input_dir, paste0(var, "_5-15cm_mean_30s.tif"))
  
  # 检查文件是否存在
  if (!all(file.exists(c(depth1_file, depth2_file)))) {
    message("跳过 ", var, ": 缺失深度层文件")
    next
  } # 读取栅格数据
  depth1 <- rast(depth1_file)
  depth2 <- rast(depth2_file)
  
  # 计算0-15cm均值（假设为简单平均）
  combined <- mean(depth1, depth2)
  
  # 重采样到2.5分钟分辨率（双线性插值）

  resampled <- resample(combined, r2_5m_template, method = "bilinear")
  
  # 构建输出文件名
  output_file <- file.path(
    output_dir,
    paste0(var, "_0-15cm_mean_2.5mins.tif")
  )
  
  # 保存结果
  writeRaster(
    resampled,
    filename = output_file,
    overwrite = TRUE,
    datatype = "FLT4S" )
  
  message("已处理: ", var)
}

message("全部处理完成！输出目录: ", output_dir)


## build factors stack

env_dir <- 'data/climate/wc2.1_2.5m'
env_files <- list.files(path = env_dir, pattern = ".tif$", full.names = TRUE)

ocean_dir <- 'data/climate/ocean'
ocean_files <- list.files(path = ocean_dir, pattern = ".tif$", full.names = TRUE)

soil_dir <- 'data/soilworld0-15cm_2.5mins'
soil_files <- list.files(path = soil_dir, pattern = ".tif$", full.names = TRUE)

topo_dir <- 'data/TOPO'
topo_files <- list.files(path = topo_dir, pattern = ".tif$", full.names = TRUE)

human_dir <- 'data/humanbeing/InputLayer_SDM'
human_files <- list.files(path = human_dir, pattern = ".tif$", full.names = TRUE)


# 定义安全加载栅格函数（修正循环逻辑）
safe_stack <- function(file_paths) {
  if (length(file_paths) == 0) return(NULL)  # 跳过空目录
  
  # 读取栅格
  rst <- rast(file_paths)
  
  # 检查坐标系一致性
  ref_crs <- crs(rst)
  for (i in 1:nlyr(rst)) {  # 通过索引逐层检查
    if (!identical(crs(rst[[i]]), ref_crs)) {
      stop("坐标系不一致: 第", i, "层 (", names(rst[[i]]), ") vs 基准 (", ref_crs, ")")
    }
  }
  
  # 检查分辨率一致性
  ref_res <- res(rst)
  tolerance <- 1e-5
  for (i in 1:nlyr(rst)) {
    layer_res <- res(rst[[i]])
    if (any(abs(layer_res - ref_res) > tolerance)) {
      warning("分辨率不一致: 第", i, "层 (", names(rst[[i]]), ") 分辨率: ", paste(layer_res, collapse = ", "))
    }
  }
  
  return(rst)
}
# 分步加载各数据集
env_stack <- safe_stack(env_files)
ocean_stack <- safe_stack(ocean_files)
soil_stack <- safe_stack(soil_files)
topo_stack <- safe_stack(topo_files)
human_stack <- rast(human_files)

print(names(human_stack))
# 打印所有栅格的范围和分辨率
check_extents <- function(files) {
  for (f in files) {
    r <- rast(f)
    cat("文件:", basename(f), "\n")
    cat("范围:", as.character(ext(r)), "\n")
    cat("分辨率:", res(r), "\n")
    cat("坐标系:", crs(r), "\n\n")
  }
}

# 检查人类活动数据栅格
check_extents(ocean_files)

full_stack <- c(
  env_stack,
  soil_stack,
  human_stack,
  topo_stack,
  ocean_stack
)
cat("最终堆栈范围:", as.character(ext(full_stack)), "\n")
cat("分辨率:", res(full_stack[[1]]), "\n")
cat("层数:", nlyr(full_stack))

writeRaster(
  full_stack,
  filename = "full_stack.tif",  # 输出文件名
  overwrite = TRUE,             # 覆盖已有文件
  datatype = "FLT4S"        # 数据类型（Float32）

)
