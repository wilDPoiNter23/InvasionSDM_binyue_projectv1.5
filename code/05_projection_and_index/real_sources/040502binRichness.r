# Original source: 040502binRichness.r
# Original source location: <LOCAL_CODE_ROOT>/040502binRichness.r
# Original source SHA-256: 05e58e7473c6987727619fd538f7413f9c91a058378bc59d3896fe1bf99f4f20
# Public-safe original snapshot: archive/original_sources/local/040502binRichness.r
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
library(openxlsx)

# === 路径设置 ===
base_dir <- publication_path("WORK_ROOT")
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_richness <- file.path(base_dir, "results/richness_binary_map.tif")
log_file <- file.path(base_dir, "results/stack_failed_richness_log.xlsx")

# === 数据准备 ===
summary_df <- read.csv(summary_csv)
summary_df$species_dot <- gsub(" ", ".", summary_df$species)

fail_log <- data.frame(species = character(), reason = character())
richness <- NULL
count <- 0L

for (i in seq_len(nrow(summary_df))) {
  sp <- summary_df$species[i]
  sp_dot <- summary_df$species_dot[i]
  proj_type <- summary_df$type[i]
  cat(sprintf("[%03d/%03d] %s (%s)\n", i, nrow(summary_df), sp, proj_type))

  rds_path <- file.path(proj_dir, paste0(sp, "_", proj_type, "_projection.rds"))
  rds <- tryCatch(readRDS(rds_path), error = function(e) NULL)

  if (is.null(rds) || !inherits(rds, "BIOMOD.projection.out")) {
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Invalid RDS"))
    next
  }

  model_name <- gsub("[\r\n]", "", trimws(rds@models.projected[1]))
  search_dir <- file.path(model_dir, sp_dot)

  # --- 优先找 bin 文件 ---
  bin_file <- list.files(
    search_dir,
    pattern = paste0(".*bin.*\\.(tif|grd)$"),
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(bin_file) == 0) {
    all_files <- list.files(search_dir, pattern = "\\.(tif|grd)$", full.names = TRUE, recursive = TRUE)
    bin_file <- all_files[
      grepl(sp_dot, basename(all_files)) &
      grepl("bin", basename(all_files), ignore.case = TRUE) &
      !grepl("filt", basename(all_files), ignore.case = TRUE)
    ]
  }

  r_bin <- NULL

  if (length(bin_file) > 0) {
    r_bin <- tryCatch(rast(bin_file[1]), error = function(e) NULL)
  } else {
    # fallback：找概率图并阈值化
    prob_file <- list.files(
      search_dir,
      pattern = paste0(model_name, ".*\\.(tif|grd)$"),
      full.names = TRUE,
      recursive = TRUE
    )

    if (length(prob_file) == 0) {
      all_prob <- list.files(search_dir, pattern = "\\.(grd|tif)$", full.names = TRUE, recursive = TRUE)
      prob_file <- all_prob[
        grepl(sp_dot, basename(all_prob)) &
        !grepl("bin|filt", basename(all_prob), ignore.case = TRUE)
      ]
    }

    if (length(prob_file) > 0) {
      r_prob <- tryCatch(rast(prob_file[1]), error = function(e) NULL)
      if (!is.null(r_prob)) {
        r_bin <- classify(r_prob, rbind(c(-Inf, 0.5, 0), c(0.5, Inf, 1)))
        cat("⚠️ Used fallback thresholding for:", sp, "\n")
      }
    }
  }

  if (is.null(r_bin)) {
    fail_log <- rbind(fail_log, data.frame(species = sp, reason = "Raster read error or not found"))
    next
  }

  # === 保证 raster 为 0/1 且对齐 ===
  r_bin <- classify(r_bin, rbind(c(-Inf, 0.5, 0), c(0.5, Inf, 1)))
  r_bin <- terra::ifel(is.na(r_bin), 0, r_bin)

  if (!is.null(richness)) {
    if (!compareGeom(r_bin, richness, stopOnError = FALSE)) {
      r_bin <- resample(r_bin, richness, method = "near")
    }
  }

  if (is.null(richness)) {
    richness <- r_bin
  } else {
    tryCatch({
      richness <- cover(richness + r_bin, richness)
    }, error = function(e) {
      fail_log <<- rbind(fail_log, data.frame(species = sp, reason = "Dimension mismatch"))
    })
  }

  count <- count + 1
  cat("✅ Success:", sp, "\n")
}

# === 写出栅格和日志 ===
if (!is.null(richness) && count > 0) {
  names(richness) <- "richness"
  writeRaster(richness, output_richness, overwrite = TRUE)
  cat("✅ Richness map saved to:", output_richness, "\n")
} else {
  cat("⚠️ No valid raster processed.\n")
}

if (nrow(fail_log) > 0) {
  write.xlsx(fail_log, file = log_file, overwrite = TRUE)
  cat("⚠️ Some species failed. See log:", log_file, "\n")
}
