# Original source: 33_build_variant_inputs.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/33_build_variant_inputs.R
# Original source SHA-256: 588595b4799736a0f4c22f98b37fb3524767d13098420af2ddc6eca7a862f81b
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 33_build_variant_inputs.R — build per-taxon input layers for the two aggregation
# variants (all-binary vs all-continuous), native domains masked to NA.
#   Version A input: TSS-binary, native -> NA   -> bin_masked/<taxon>__tss_binary_non_native.tif
#   Version B input: continuous ensemble, native -> NA -> cont_masked/<taxon>__ensemble_suitability_non_native.tif
# Sources (from tssbin_manifest_audit_813.csv):
#   - is_tssbin=TRUE  : A source = manifest path (binary); B source = continuous sibling
#   - is_tssbin=FALSE : B source = manifest path (continuous);
#                       A source = TSSbin sibling if present, else threshold continuous
#                       at the species' TSS cutoff from .BIOMOD_DATA/*/models.evaluation
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

VARDIR <- file.path(DIR_AGG_RASTERS, "aggregation_variants_20260917")
BINDIR <- file.path(VARDIR, "inputs", "bin_masked")
CONTDIR <- file.path(VARDIR, "inputs", "cont_masked")
dir.create(BINDIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CONTDIR, recursive = TRUE, showWarnings = FALSE)

aud <- read.csv(file.path(DIR_TABLES, "tssbin_manifest_audit_813.csv"), stringsAsFactors = FALSE)
aud$sibling_exists[is.na(aud$sibling_exists)] <- FALSE

poly_all <- vect(NATIVE_GPKG)
stopifnot("accepted_taxon" %in% names(poly_all))

get_cutoff <- function(species, path) {
  sp_dot <- gsub(" ", ".", species)
  de <- file.path("<WORK_ROOT>/models", sp_dot, ".BIOMOD_DATA")
  if (!dir.exists(de)) return(list(cutoff = NA_real_, model = NA_character_))
  ens_dirs <- list.dirs(de, recursive = FALSE)
  if (!length(ens_dirs)) return(list(cutoff = NA_real_, model = NA_character_))
  f <- file.path(ens_dirs[1], "models.evaluation")
  if (!file.exists(f)) return(list(cutoff = NA_real_, model = NA_character_))
  e <- new.env(); try(load(f, envir = e), silent = TRUE)
  objs <- ls(e); if (!length(objs)) return(list(cutoff = NA_real_, model = NA_character_))
  d <- get(objs[1], envir = e)
  if (!is.data.frame(d) || !("cutoff" %in% names(d))) return(list(cutoff = NA_real_, model = NA_character_))
  model <- sub("^proj_(fallback|ensemble)_projection_", "", basename(path))
  model <- sub("\\.(tif|grd)$", "", model)
  hit <- d$full.name == model
  if (!any(hit)) hit <- grepl("PA1_RUN1", d$full.name) & d$metric.eval == "TSS"
  if (!any(hit)) hit <- d$metric.eval == "TSS"
  d2 <- d[hit & d$metric.eval == "TSS", ]
  if (!nrow(d2)) return(list(cutoff = NA_real_, model = model))
  i <- which.max(d2$validation)
  list(cutoff = as.numeric(d2$cutoff[i]), model = model)
}

mask_native <- function(r, species) {
  poly <- poly_all[poly_all$accepted_taxon == species, ]
  if (nrow(poly) == 0) return(r)   # no mapped domain (3 taxa) -> unchanged
  psf <- st_as_sf(poly)
  psf <- st_wrap_dateline(psf, options = c("WRAPDATELINE=YES"))
  pv <- project(vect(psf), "EPSG:4326")
  mask(r, pv, inverse = TRUE)
}

log_csv <- file.path(VARDIR, "inputs", "variant_inputs_QC.csv")
rows <- list()
if (file.exists(log_csv)) {
  prev <- read.csv(log_csv, stringsAsFactors = FALSE)
  rows <- split(prev, seq_len(nrow(prev)))
}

tsafe <- function(x) gsub("[ .]+", "_", trimws(x))

for (i in seq_len(nrow(aud))) {
  sp <- aud$accepted_taxon[i]
  fa <- file.path(BINDIR, paste0(tsafe(sp), "__tss_binary_non_native.tif"))
  fc <- file.path(CONTDIR, paste0(tsafe(sp), "__ensemble_suitability_non_native.tif"))
  if (file.exists(fa) && file.exists(fc)) next

  # ---- Version B (continuous) ----
  if (!file.exists(fc)) {
    src_b <- if (isTRUE(aud$is_tssbin[i]) && isTRUE(aud$sibling_exists[i])) aud$sibling[i] else aud$path[i]
    rb <- rast(src_b)
    mm <- minmax(rb)
    if (mm[2] <= 1.01 && mm[2] > 0) {
      # already 0-1 -> keep as is (units: suitability 0-1)
    } else if (mm[2] <= 1000.001) {
      rb <- rb / 1000
    }
    rb <- mask_native(rb, sp)
    writeRaster(rb, fc, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
                gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
    rm(rb); gc()
  }
  # ---- Version A (binary) ----
  if (!file.exists(fa)) {
    cut_used <- NA_real_
    if (isTRUE(aud$is_tssbin[i])) {
      ra <- rast(aud$path[i])
    } else if (isTRUE(aud$sibling_exists[i])) {
      ra <- rast(aud$sibling[i])
    } else {
      co <- get_cutoff(sp, aud$path[i])
      cut_used <- co$cutoff
      rc <- rast(aud$path[i])
      mm <- minmax(rc)
      if (mm[2] <= 1.01 && mm[2] > 0) { cut_used <- cut_used / 1000 }  # cutoff table is 0-1000
      if (is.na(cut_used)) {
        cut_used <- mm[1] + 0.5 * (mm[2] - mm[1])  # documented fallback: mid-range
      }
      ra <- ifel(rc >= cut_used, 1, 0)
      rm(rc); gc()
    }
    ra <- mask_native(ra, sp)
    writeRaster(ra, fa, overwrite = TRUE, datatype = "FLT4S", NAflag = -3.4e+38,
                gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"))
    rm(ra); gc()
  }
  if (i %% 25 == 0) cat(sprintf("[%d/813] %s\n", i, sp))
}

# rebuild QC table from outputs on disk
out <- data.frame()
for (i in seq_len(nrow(aud))) {
  sp <- aud$accepted_taxon[i]
  fa <- file.path(BINDIR, paste0(tsafe(sp), "__tss_binary_non_native.tif"))
  fc <- file.path(CONTDIR, paste0(tsafe(sp), "__ensemble_suitability_non_native.tif"))
  mb <- tryCatch(minmax(rast(fa)), error = function(e) c(NA, NA))
  mc <- tryCatch(minmax(rast(fc)), error = function(e) c(NA, NA))
  out <- rbind(out, data.frame(accepted_taxon = sp,
    bin_source = if (isTRUE(aud$is_tssbin[i])) "manifest_TSSbin" else if (isTRUE(aud$sibling_exists[i])) "TSSbin_sibling" else "thresholded_at_TSS_cutoff",
    bin_min = mb[1], bin_max = mb[2],
    cont_source = if (isTRUE(aud$is_tssbin[i])) "continuous_sibling" else "manifest_continuous",
    cont_min = mc[1], cont_max = mc[2], stringsAsFactors = FALSE))
}
write.csv(out, log_csv, row.names = FALSE)
cat("bin_masked files:", length(list.files(BINDIR)), " cont_masked files:", length(list.files(CONTDIR)), "\n")
cat("bin classes:\n"); print(table(out$bin_source))
cat("DONE\n")
