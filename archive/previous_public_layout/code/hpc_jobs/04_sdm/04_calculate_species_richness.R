# ARCHIVE: historical reference; symbolic paths are intentionally not runnable.
# Original SHA256: d7cf54d175763397841c99607c1db8ae0fb831919e4149d1f893f95c2786bc52
library(terra)
library(openxlsx)

# === Path settings ===
base_dir <- "${WORK_ROOT}"
proj_dir <- file.path(base_dir, "results", "projections")
model_dir <- file.path(base_dir, "models")
summary_csv <- file.path(base_dir, "results/evaluations/projection_summary.csv")
output_richness <- file.path(base_dir, "results/richness_binary_map.tif")
log_file <- file.path(base_dir, "results/stack_failed_richness_log.xlsx")

# === Data preparation ===
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

  # --- Prioritize finding bin files ---
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
    # fallback: find probability map and threshold it
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

  # === Ensure raster is 0/1 and aligned ===
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

# === Write out raster and log ===
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
