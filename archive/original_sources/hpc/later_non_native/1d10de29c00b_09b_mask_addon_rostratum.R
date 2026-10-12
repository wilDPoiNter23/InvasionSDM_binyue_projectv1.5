# Original source: 09b_mask_addon_rostratum.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/09b_mask_addon_rostratum.R
# Original source SHA-256: 1d10de29c00b05c53720217da5e68f458689b6e4fa8fce325ca6b5735d6faa43
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 09b_mask_addon_rostratum.R — non-native masking for Solanum rostratum (add-on)
# Mirrors the 09_mask_one_taxon.R protocol exactly (inverse mask, chunked QC).
# Input:  rostratum EMmean projection (4320x8640, EPSG:4326, BIOMOD 0-1000)
# Mask:   native_range_addon_Solanum_rostratum.gpkg (Mexico + USA)
# Output: <rasters>/Solanum_rostratum__non_native_suitability.tif + per-taxon QC
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

tx <- "Solanum rostratum"
in_path  <- "<WORK_ROOT>/results/Solanum_rostratum_occpatch_v1/projection/Solanum_rostratum_occpatch_EMmean.tif"
out_path <- non_native_output_path(tx)
qc_file  <- file.path(PER_TAXON_QC_DIR, paste0(taxon_safe(tx), "__qc.csv"))
ADDON_GPKG <- file.path(DIR_MASK_VECTORS, "native_range_addon_Solanum_rostratum.gpkg")
log <- function(...) cat(sprintf("[%s] ", tx), ..., "\n")

make_qc_row <- function(fields) {
  defaults <- c(
    accepted_taxon = tx, input_projection_path = in_path, input_SHA256 = sha256_of(in_path),
    native_polygon_found = NA, native_country_count = NA,
    output_path = out_path, output_SHA256 = NA,
    CRS_match = NA, extent_match = NA, resolution_match = NA,
    dimensions_match = NA, geometry_match = NA,
    input_nonNA = NA, output_nonNA = NA, native_cells_removed = NA,
    proportion_cells_removed = NA, input_min = NA, input_max = NA,
    output_min = NA, output_max = NA, values_preserved = NA,
    has_Inf = NA, has_NaN = NA, all_NA = NA, all_zero = NA,
    QC_pass = NA, notes = "")
  for (nm in names(fields)) defaults[[nm]] <- as.character(fields[[nm]])[1]
  d <- as.data.frame(as.list(defaults), stringsAsFactors = FALSE)
  write.csv(d, qc_file, row.names = FALSE)
  invisible(d)
}
fail <- function(note) {
  log("FAIL:", note)
  make_qc_row(list(QC_pass = FALSE, notes = note))
  quit(save = "no", status = 1)
}

# 1. input raster geometry checks (same expectations as 09)
r <- tryCatch(rast(in_path), error = function(e) NULL)
if (is.null(r)) fail("RASTER_READ_ERROR")
if (nlyr(r) != 1) fail(sprintf("MULTILAYER_RASTER (%d layers)", nlyr(r)))
exp_dim <- c(4320, 8640); exp_ext <- ext(-180, 180, -90, 90); exp_crs <- "EPSG:4326"
geom_ok <- isTRUE(all(dim(r)[1:2] == exp_dim)) &&
           isTRUE(ext(r) == exp_ext) &&
           isTRUE(crs(r, describe = TRUE)$code == "4326")
if (!geom_ok) fail(sprintf("GEOMETRY_MISMATCH dim=%s ext=%s crs=%s",
  paste(dim(r)[1:2], collapse = "x"), paste(as.vector(ext(r)), collapse = ","),
  crs(r, describe = TRUE)$code))

# 2. add-on native polygon
poly_all <- tryCatch(vect(ADDON_GPKG), error = function(e) NULL)
if (is.null(poly_all)) fail("NATIVE_GPKG_READ_ERROR")
if (!all(c("accepted_taxon", "n_native_countries", "mapping_source") %in% names(poly_all)))
  fail("NATIVE_GPKG_SCHEMA_MISMATCH")
poly <- poly_all[poly_all$accepted_taxon == tx, ]
if (nrow(poly) == 0) fail("NO_NATIVE_POLYGON")
n_countries <- poly$n_native_countries[1]
log(sprintf("native features: %d countries: %d", nrow(poly), n_countries))

# antimeridian-safe prep (same as 09)
poly_sf <- st_as_sf(poly)
poly_sf <- st_wrap_dateline(poly_sf, options = c("WRAPDATELINE=YES"))
poly_v <- project(vect(poly_sf), "EPSG:4326")

# 3. inverse mask
out <- tryCatch(mask(r, poly_v, inverse = TRUE), error = function(e) NULL)
if (is.null(out)) fail("MASK_ERROR")

# 4. write output
dt_in <- terra::datatype(r)
tryCatch(
  writeRaster(out, out_path, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"),
              datatype = dt_in),
  error = function(e) fail(paste0("WRITE_ERROR: ", conditionMessage(e))))

# 5. chunked numeric QC (identical logic to 09)
m <- tryCatch(rasterize(poly_v, r, field = 1, touches = TRUE), error = function(e) NULL)
if (is.null(m)) fail("QC_RASTERIZE_ERROR")
in_nonNA <- 0L; out_nonNA <- 0L; wrong <- 0L; added <- 0L
nat_removed <- 0L; nat_kept <- 0L; nat_in <- 0L
step <- 256; nrow_r <- nrow(r)
for (st in seq(1, nrow_r, by = step)) {
  n <- min(step, nrow_r - st + 1)
  vi <- values(r,   row = st, nrows = n, mat = TRUE)
  vo <- values(out, row = st, nrows = n, mat = TRUE)
  vm <- values(m,   row = st, nrows = n, mat = TRUE)
  in_nonNA  <- in_nonNA  + sum(!is.na(vi))
  out_nonNA <- out_nonNA + sum(!is.na(vo))
  keep <- !is.na(vo)
  if (any(keep)) {
    wrong <- wrong + sum(abs(vi[keep] - vo[keep]) > 1e-9, na.rm = TRUE)
    added <- added + sum(is.na(vi[keep]))
  }
  natm <- !is.na(vm)
  if (any(natm)) {
    nat_removed <- nat_removed + sum(natm & is.na(vo) & !is.na(vi))
    nat_kept    <- nat_kept    + sum(natm & !is.na(vo))
    nat_in      <- nat_in      + sum(natm & !is.na(vi))
  }
}
mm_in <- minmax(r); mm_out <- minmax(out)
has_inf  <- any(is.infinite(mm_out))
has_nan  <- any(is.nan(mm_out))
all_na   <- out_nonNA == 0
all_zero <- !all_na && min(abs(mm_out), na.rm = TRUE) == 0 && max(abs(mm_out), na.rm = TRUE) == 0
native_cells_removed <- in_nonNA - out_nonNA
values_preserved <- wrong == 0 && added == 0 && nat_kept == 0 &&
                    native_cells_removed == nat_removed
qc_pass <- geom_ok && values_preserved && (out_nonNA <= in_nonNA)

make_qc_row(list(
  native_polygon_found = TRUE, native_country_count = n_countries,
  output_SHA256 = sha256_of(out_path),
  CRS_match = TRUE, extent_match = TRUE, resolution_match = TRUE,
  dimensions_match = TRUE, geometry_match = TRUE,
  input_nonNA = in_nonNA, output_nonNA = out_nonNA,
  native_cells_removed = native_cells_removed,
  proportion_cells_removed = round(native_cells_removed / in_nonNA, 6),
  input_min = mm_in[1], input_max = mm_in[2],
  output_min = mm_out[1], output_max = mm_out[2],
  values_preserved = values_preserved,
  has_Inf = has_inf, has_NaN = has_nan, all_NA = all_na, all_zero = all_zero,
  QC_pass = qc_pass,
  notes = "ADDON_2026-08-17: native mask = Mexico + USA (contiguous). Inverse mask protocol identical to 09_mask_one_taxon.R."))
log(sprintf("DONE pass=%s removed=%d (%.2f%%)",
    qc_pass, native_cells_removed, 100 * native_cells_removed / in_nonNA))
if (!qc_pass) quit(save = "no", status = 1) else quit(save = "no", status = 0)
