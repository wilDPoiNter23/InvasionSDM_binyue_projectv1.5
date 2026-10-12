# Original source: classify_coastal_association.R
# Original source location: ${WORK_ROOT}/archive/v1_deprecated/classify_coastal_association.R
# Original source SHA-256: 10e93bcd602392ee58f476ae2f8d55855edcfed71b0f8332a0ac53677b581ef1
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_813_REPAIR_PROJECTION_QC)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# Step 6 (CORRECTED): Classify 813 species by coastal association
# Uses CLEANED spatially independent occurrence records:
#   valid coords → unique coords → land filter → unique 2.5 arc-min cells
# Distance method: s2 spherical-geometry geodesic (sf::st_distance)
# ============================================================================

.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

sf_use_s2(TRUE)

FS       <- "<HPC_INPUT_ROOT>/climate/full_stack.tif"
OCC_DIR  <- "<WORK_ROOT>/data/species_occ"
AUDIT    <- "<NON_NATIVE_ROOT>/audit"
PROJ_SUM <- "<WORK_ROOT>/results/evaluations/projection_summary.csv"

LAMBDAS   <- c(25, 50, 100)
MIN_COASTAL_OCC  <- 20
MIN_COASTAL_PROP <- 0.10
BLOCK_SIZE <- 20000

# ── Coastline from land mask ──────────────────────────────────────────
cat("Building coastline for distance computation...\n")
fs   <- rast(FS)
bio1 <- fs[["wc2.1_2.5m_bio_1"]]
land_mask <- !is.na(bio1)

coast_r  <- boundaries(land_mask, classes=FALSE, directions=8)
coast_xy <- xyFromCell(coast_r, which(values(coast_r) == 1))
rm(coast_r); gc()
coast_sf <- st_as_sf(as.data.frame(coast_xy), coords=c("x","y"), crs=4326)
cat(sprintf("  Coastline: %d points\n", nrow(coast_sf)))
rm(coast_xy); gc()

# ── Species list ──────────────────────────────────────────────────────
ps  <- read.csv(PROJ_SUM, stringsAsFactors=FALSE)
spp <- ps$species
cat(sprintf("Species: %d\n", length(spp)))

# ── Output columns ────────────────────────────────────────────────────
out_cols <- c("species","raw_count","valid_coord_count","unique_coord_count",
  "land_cell_count","unique_raster_cell_count",
  "n_within_25km","n_within_50km","n_within_100km",
  "prop_within_25km","prop_within_50km","prop_within_100km",
  "coastal_association_class","eligible_C1","eligible_C2","data_quality_warnings")

results <- as.data.frame(matrix(NA, nrow=length(spp), ncol=length(out_cols)))
names(results) <- out_cols

pb <- txtProgressBar(min=0, max=length(spp), style=3)

for (i in seq_along(spp)) {
  sp  <- spp[i]
  sp_ <- gsub(" ", "_", sp)
  
  results$species[i] <- sp
  
  occ_file <- file.path(OCC_DIR, sp_, paste0(sp_, "_occurrences.csv"))
  if (!file.exists(occ_file)) {
    results$data_quality_warnings[i] <- "missing_occurrence_file"
    setTxtProgressBar(pb, i); next
  }
  
  occ <- tryCatch(read.csv(occ_file, stringsAsFactors=FALSE), error=function(e) NULL)
  if (is.null(occ) || nrow(occ) == 0) {
    results$data_quality_warnings[i] <- "empty_occurrence_file"
    setTxtProgressBar(pb, i); next
  }
  
  raw_n <- nrow(occ)
  results$raw_count[i] <- raw_n
  
  # ── Step 2: Valid coordinates ────────────────────────────────────
  ok <- !is.na(occ$decimalLongitude) & !is.na(occ$decimalLatitude) &
        occ$decimalLongitude >= -180 & occ$decimalLongitude <= 180 &
        occ$decimalLatitude >= -90 & occ$decimalLatitude <= 90
  occ <- occ[ok, ]
  valid_n <- nrow(occ)
  results$valid_coord_count[i] <- valid_n
  
  if (valid_n < 5) {
    results$data_quality_warnings[i] <- paste0("too_few_valid_coords(n=", valid_n, ")")
    setTxtProgressBar(pb, i); next
  }
  
  # ── Step 3: Unique coordinates ──────────────────────────────────
  occ <- occ[!duplicated(occ[, c("decimalLongitude","decimalLatitude")]), ]
  uniq_n <- nrow(occ)
  results$unique_coord_count[i] <- uniq_n
  
  if (uniq_n < 5) {
    results$data_quality_warnings[i] <- paste0("too_few_unique_coords(n=", uniq_n, ")")
    setTxtProgressBar(pb, i); next
  }
  
  # ── Step 4: Land mask filter ────────────────────────────────────
  occ_xy <- as.matrix(occ[, c("decimalLongitude","decimalLatitude")])
  env <- extract(bio1, occ_xy)
  occ <- occ[!is.na(env[,2]), ]   # keep only land cells
  land_n <- nrow(occ)
  results$land_cell_count[i] <- land_n
  
  if (land_n < 5) {
    results$data_quality_warnings[i] <- paste0("mostly_ocean(land_n=", land_n, ")")
    setTxtProgressBar(pb, i); next
  }
  
  # ── Step 5: Unique 2.5 arc-min raster cells ─────────────────────
  occ_xy <- as.matrix(occ[, c("decimalLongitude","decimalLatitude")])
  occ_cells <- cellFromXY(bio1, occ_xy)
  occ <- occ[!duplicated(occ_cells), ]
  cell_n <- nrow(occ)
  results$unique_raster_cell_count[i] <- cell_n
  
  if (cell_n < 5) {
    results$data_quality_warnings[i] <- paste0("too_few_raster_cells(n=", cell_n, ")")
    setTxtProgressBar(pb, i); next
  }
  
  # ── Step 6: Coastal distance (s2 geodesic) ──────────────────────
  occ_xy <- as.matrix(occ[, c("decimalLongitude","decimalLatitude")])
  
  # Block-wise geodesic distance to nearest coast
  n_occ <- nrow(occ_xy)
  min_dist_km <- numeric(n_occ)
  
  for (j in seq(1, n_occ, by=BLOCK_SIZE)) {
    end_j <- min(j + BLOCK_SIZE - 1, n_occ)
    block_xy <- occ_xy[j:end_j, , drop=FALSE]
    block_sf <- st_as_sf(as.data.frame(block_xy), coords=c("x","y"), crs=4326)
    nearest  <- st_nearest_feature(block_sf, coast_sf)
    d_m <- st_distance(block_sf, coast_sf[nearest, ], by_element=TRUE)
    min_dist_km[j:end_j] <- as.numeric(d_m) / 1000
  }
  
  n_25  <- sum(min_dist_km <= 25)
  n_50  <- sum(min_dist_km <= 50)
  n_100 <- sum(min_dist_km <= 100)
  
  prop_25  <- n_25  / cell_n
  prop_50  <- n_50  / cell_n
  prop_100 <- n_100 / cell_n
  
  results$n_within_25km[i]  <- n_25
  results$n_within_50km[i]  <- n_50
  results$n_within_100km[i] <- n_100
  results$prop_within_25km[i]  <- round(prop_25, 4)
  results$prop_within_50km[i]  <- round(prop_50, 4)
  results$prop_within_100km[i] <- round(prop_100, 4)
  
  # ── Classification ──────────────────────────────────────────────
  if (cell_n < 10) {
    cclass <- "insufficient_data"
  } else if (prop_25 >= 0.50) {
    cclass <- "coastal_specialist"
  } else if (prop_50 >= 0.30) {
    cclass <- "coastal_associated"
  } else if (prop_100 >= 0.10) {
    cclass <- "mixed_coastal_inland"
  } else {
    cclass <- "primarily_inland"
  }
  results$coastal_association_class[i] <- cclass
  
  # C1 eligibility
  elig_aug <- "no"
  for (d in LAMBDAS) {
    nd <- if(d==25) n_25 else if(d==50) n_50 else n_100
    pd <- if(d==25) prop_25 else if(d==50) prop_50 else prop_100
    if (nd >= MIN_COASTAL_OCC && pd >= MIN_COASTAL_PROP) {
      elig_aug <- sprintf("yes(at_%d_km)", d); break
    }
  }
  results$eligible_C1[i] <- elig_aug
  
  # C2 eligibility
  elig_co <- "no"
  for (d in LAMBDAS) {
    nd <- if(d==25) n_25 else if(d==50) n_50 else n_100
    pd <- if(d==25) prop_25 else if(d==50) prop_50 else prop_100
    if (nd >= 30 && pd >= 0.20) {
      elig_co <- sprintf("yes(at_%d_km)", d); break
    }
  }
  results$eligible_C2[i] <- elig_co
  
  # ── Data quality warnings ───────────────────────────────────────
  warns <- character(0)
  if (raw_n / max(uniq_n, 1) > 10) warns <- c(warns, "high_duplicate_rate")
  if (uniq_n > 0 && land_n / uniq_n < 0.8) warns <- c(warns, "large_ocean_fraction")
  if (cell_n < 20) warns <- c(warns, "small_sample")
  if (cell_n < 10) warns <- c(warns, "tiny_sample")
  results$data_quality_warnings[i] <- paste(warns, collapse="; ")
  if (results$data_quality_warnings[i] == "") results$data_quality_warnings[i] <- "none"
  
  setTxtProgressBar(pb, i)
}
close(pb)

# ── Write ─────────────────────────────────────────────────────────────
out_path <- file.path(AUDIT, "all_813_species_coastal_association_cleaned.csv")
write.csv(results, out_path, row.names=FALSE)

# ── Summary ──────────────────────────────────────────────────────────
cat("\n\n===== Coastal Association Summary (cleaned occurrences) =====\n")
cat(sprintf("Total species: %d\n\n", nrow(results)))

tbl <- table(results$coastal_association_class)
for (cl in names(sort(tbl, decreasing=TRUE))) {
  cat(sprintf("  %-30s %5d\n", cl, tbl[cl]))
}

cat(sprintf("\n  C1-eligible: %d\n", sum(grepl("^yes", results$eligible_C1))))
cat(sprintf("  C2-eligible: %d\n", sum(grepl("^yes", results$eligible_C2))))

# Data quality summary
cat(sprintf("\n  Species with high duplicate rate: %d\n",
  sum(grepl("high_duplicate_rate", results$data_quality_warnings))))
cat(sprintf("  Species with large ocean fraction: %d\n",
  sum(grepl("large_ocean_fraction", results$data_quality_warnings))))
cat(sprintf("  Species with small sample (<20 cells): %d\n",
  sum(grepl("small_sample", results$data_quality_warnings))))
cat(sprintf("  Species flagged as insufficient_data: %d\n",
  sum(results$coastal_association_class == "insufficient_data")))

# ── 13 negative-TSS species ──────────────────────────────────────────
cat("\n--- 13 negative-TSS species (cleaned) ---\n")
neg13 <- c("Adiantum latifolium","Ageratum houstonianum","Ailanthus altissima",
  "Cenchrus echinatus","Diodia teres","Geranium dissectum","Glebionis coronaria",
  "Hibiscus trionum","Tamarix aphylla","Veronica arvensis",
  "Urochloa eminii","Musa ornata","Vicia eriocarpa")
for (sp in neg13) {
  row <- results[results$species == sp, ]
  if (nrow(row) > 0 && !is.na(row$unique_raster_cell_count)) {
    cat(sprintf("  %-30s raw=%6d cells=%5d  25km:%5.1f%%  50km:%5.1f%%  100km:%5.1f%%  %-22s C1=%s\n",
      sp, row$raw_count, row$unique_raster_cell_count,
      100*row$prop_within_25km, 100*row$prop_within_50km, 100*row$prop_within_100km,
      row$coastal_association_class, row$eligible_C1))
  } else {
    cat(sprintf("  %-30s NO DATA or insufficient records\n", sp))
  }
}

cat(sprintf("\nOutput: %s\n", out_path))
