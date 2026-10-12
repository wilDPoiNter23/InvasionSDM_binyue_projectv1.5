# Original source: 0404fallbackTobin.r
# Original source location: <LOCAL_CODE_ROOT>/0404fallbackTobin.r
# Original source SHA-256: 68507ecd9c0eb0e57591b273ee982b6fbc0b69b815314a18cd082f33a8fafbad
# Public-safe original snapshot: archive/original_sources/local/0404fallbackTobin.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
library(terra)
library(biomod2)
library(stringr)

base_dir <- "<WORK_ROOT>"
proj_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
model_dir <- file.path(base_dir, "models")

# 读取 fallback 模型信息
proj_df <- read.csv(proj_csv)
fallback_species <- proj_df[proj_df$type == "fallback", "species"]
species_dot_names <- gsub(" ", ".", fallback_species)

# 处理每个 fallback 物种
for (i in seq_along(fallback_species)) {
  species <- fallback_species[i]
  species_dir <- file.path(model_dir, species_dot_names[i])
  proj_dir <- file.path(species_dir, "proj_fallback_projection", "individual_projections")

  # 找到唯一的 .tif 预测文件
  tif_file <- list.files(proj_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_file) != 1) {
    cat("跳过物种", species, "- 找到", length(tif_file), "个预测文件\n")
    next
  }

  # 找回模型对象
  model_rds_path <- file.path(model_dir, species_dot_names[i], paste0(species_dot_names[i], ".biomod2.models.out"))
  if (!file.exists(model_rds_path)) {
    cat("跳过", species, "- 找不到 biomod2 模型对象\n")
    next
  }

  model_obj <- readRDS(model_rds_path)

  # 获取 best 模型名（tif 文件名里含模型名）
  tif_basename <- basename(tif_file)
  model_match <- str_extract(tif_basename, paste(get_built_models(model_obj), collapse = "|"))
  if (is.na(model_match)) {
    cat("跳过", species, "- tif 文件中无法识别模型名\n")
    next
  }

  # 获取对应模型的 TSS cutoff
  evals <- get_evaluations(model_obj)
  tss_cutoff <- evals["TSS", "Testing.data", model_match]
  if (is.na(tss_cutoff)) {
    cat("跳过", species, "- 找不到模型 TSS cutoff\n")
    next
  }

  # 生成 binary 栅格
  pred_raster <- rast(tif_file)
  binary_raster <- pred_raster > tss_cutoff
  bin_path <- sub("\\.tif$", "_bin.tif", tif_file)
  writeRaster(binary_raster, bin_path, overwrite = TRUE)
  cat("完成 fallback binary prediction:", bin_path, "\n")
}
