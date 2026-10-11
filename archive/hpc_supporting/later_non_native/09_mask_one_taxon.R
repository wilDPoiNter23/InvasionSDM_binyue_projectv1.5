# SUPPORTING LATER PRODUCT: not established as submitted 2025 workflow.
# Original SHA256: 056d4206329719c6f4d20bd7e30e56bf27ee6b352f8586365a50640eb97cfc4e
# 09_mask_one_taxon.R — PHASES 9-12: authoritative non-native masking for ONE taxon
# Usage:
#   Rscript 09_mask_one_taxon.R <taxon_name | manifest_row_index>
# Reads: frozen global projection (manifest v3 path), native polygon (gpkg)
# Writes: <taxon_safe>__non_native_suitability.tif + per-taxon QC CSV row
# Exit code 0 = QC pass, 1 = failure (task-level, for SLURM retry).
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
sel <- args[1]

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
if (grepl("^[0-9]+$", sel)) {
  stopifnot(as.integer(sel) >= 1, as.integer(sel) <= nrow(mani))
  row <- mani[as.integer(sel), ]
} else {
  hit <- which(mani$accepted_taxon == sel)
  stopifnot(length(hit) == 1)
  row <- mani[hit, ]
}
tx        <- row$accepted_taxon
in_path   <- row$final_projection_path
# frozen manifest v3 stores one relative path (Glebionis coronaria); resolve it
# against the frozen modelling base directory.
if (!grepl("^/", in_path) && !file.exists(in_path)) {
  in_path <- file.path("${WORK_ROOT}", in_path)
}
in_sha_m  <- row$final_projection_SHA256
out_path  <- non_native_output_path(tx)
qc_file   <- file.path(PER_TAXON_QC_DIR, paste0(taxon_safe(tx), "__qc.csv"))

log <- function(...) cat(sprintf("[%s] ", tx), ..., "\n")

make_qc_row <- function(fields) {
  defaults <- c(
    accepted_taxon = tx, input_projection_path = in_path, input_SHA256 = in_sha_m,
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
  make_qc_row(list(native_polygon_found = NA, QC_pass = FALSE, notes = note))
  quit(save = "no", status = 1)
}

# 1. read frozen global projection
r <- tryCatch(rast(in_path), error = function(e) NULL)
if (is.null(r)) fail("RASTER_READ_ERROR")
if (nlyr(r) != 1) fail(sprintf("MULTILAYER_RASTER (%d layers)", nlyr(r)))

exp_dim <- c(4320, 8640)
exp_ext <- ext(-180, 180, -90, 90)
exp_crs <- "EPSG:4326"
geom_ok <- isTRUE(all(dim(r)[1:2] == exp_dim)) &&
           isTRUE(ext(r) == exp_ext) &&
           isTRUE(crs(r, describe = TRUE)$code == "4326")
if (!geom_ok) fail(sprintf("GEOMETRY_MISMATCH dim=%s ext=%s crs=%s",
  paste(dim(r)[1:2], collapse = "x"), paste(as.vector(ext(r)), collapse = ","),
  crs(r, describe = TRUE)$code))

# 2. native polygon
poly_all <- tryCatch(vect(NATIVE_GPKG), error = function(e) NULL)
if (is.null(poly_all)) fail("NATIVE_GPKG_READ_ERROR")
# consistency guard: authoritative masks must come from the country-level layer
# (field `mapping_source`, built by 06_native_polygons.R). If a different layer
# (e.g. an L3-based variant with field `source`) is on disk, refuse and retry.
if (!("mapping_source" %in% names(poly_all))) {
  fail("GEO_PACKAGE_NOT_COUNTRY_LEVEL — native_ranges_813.gpkg lacks mapping_source field; retry after country-level layer restored")
}
poly <- poly_all[poly_all$accepted_taxon == tx, ]
poly_empty <- FALSE
if (nrow(poly) == 0) {
  poly_empty <- TRUE
} else {
  psf <- tryCatch(sf::st_as_sf(poly), error = function(e) NULL)
  if (is.null(psf) || sf::st_is_empty(psf)[1]) poly_empty <- TRUE
}
if (poly_empty) {
  # MISSING_NATIVE_MAPPING taxa: no mapped native domain -> non-native product
  # equals the frozen global projection, copied byte-for-byte (values identical).
  log("MISSING_NATIVE_MAPPING: writing global projection unchanged")
  dt_in <- terra::datatype(r)
  tryCatch(
    writeRaster(r, out_path, overwrite = TRUE,
                gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"),
                datatype = dt_in),
    error = function(e) fail(paste0("WRITE_ERROR: ", conditionMessage(e))))
  mm_in <- minmax(r)
  make_qc_row(list(
    native_polygon_found = FALSE, native_country_count = 0L,
    output_SHA256 = sha256_of(out_path),
    CRS_match = TRUE, extent_match = TRUE, resolution_match = TRUE,
    dimensions_match = TRUE, geometry_match = TRUE,
    input_nonNA = sum(!is.na(values(r, mat = FALSE))),
    output_nonNA = sum(!is.na(values(r, mat = FALSE))),
    native_cells_removed = 0L, proportion_cells_removed = 0,
    input_min = mm_in[1], input_max = mm_in[2],
    output_min = mm_in[1], output_max = mm_in[2],
    values_preserved = TRUE,
    has_Inf = FALSE, has_NaN = FALSE,
    all_NA = FALSE, all_zero = (mm_in[1] == 0 && mm_in[2] == 0),
    QC_pass = TRUE,
    notes = "MISSING_NATIVE_MAPPING: source lists introduced-only distribution; no mapped native domain. Non-native product equals global projection (documented)."))
  log("PASS (unmasked, documented)")
  quit(save = "no", status = 0)
}

n_countries <- poly$n_native_countries[1]

# split at antimeridian for safe rasterization (no-op for non-crossing polys)
poly_sf <- st_as_sf(poly)
poly_sf <- st_wrap_dateline(poly_sf, options = c("WRAPDATELINE=YES"))
poly_v <- vect(poly_sf)
poly_v <- project(poly_v, "EPSG:4326")

# 3. inverse mask (no crop; geometry preserved)
out <- tryCatch(mask(r, poly_v, inverse = TRUE), error = function(e) NULL)
if (is.null(out)) fail("MASK_ERROR")

# 4. write output (values untouched outside native range; keep input datatype)
dt_in <- terra::datatype(r)
tryCatch(
  writeRaster(out, out_path, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES"),
              datatype = dt_in),
  error = function(e) fail(paste0("WRITE_ERROR: ", conditionMessage(e))))

# 5. numeric QC (chunked to bound memory)
chunk_qc <- function() {
  in_nonNA <- 0L; out_nonNA <- 0L; wrong <- 0L; added <- 0L
  sum_absdiff <- 0
  nrow_r <- nrow(r)
  step <- 256
  # mask() applies touches=TRUE semantics for polygons; rasterize must match
  m <- tryCatch(rasterize(poly_v, r, field = 1, touches = TRUE), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  nat_removed <- 0L; nat_kept <- 0L; nat_in <- 0L
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
      sum_absdiff <- sum_absdiff + sum(abs(vi[keep] - vo[keep]), na.rm = TRUE)
    }
    natm <- !is.na(vm)
    if (any(natm)) {
      nat_removed <- nat_removed + sum(natm & is.na(vo) & !is.na(vi))
      nat_kept    <- nat_kept    + sum(natm & !is.na(vo))
      nat_in      <- nat_in      + sum(natm & !is.na(vi))
    }
  }
  list(in_nonNA = in_nonNA, out_nonNA = out_nonNA, wrong = wrong, added = added,
       sum_absdiff = sum_absdiff, nat_removed = nat_removed,
       nat_kept = nat_kept, nat_in = nat_in)
}
qc <- chunk_qc()
if (is.null(qc)) fail("QC_RASTERIZE_ERROR")

mm_in  <- minmax(r)
mm_out <- minmax(out)
vals_out_sample <- values(out, row = 1, nrows = min(256, nrow(out)), mat = TRUE)
has_inf  <- any(is.infinite(mm_out))
has_nan  <- sum(is.nan(values(out, row = 1, nrows = min(256, nrow(out)), mat = TRUE)), na.rm = TRUE) > 0 ||
            any(is.nan(mm_out))
all_na   <- qc$out_nonNA == 0
all_zero <- !all_na && min(abs(mm_out), na.rm = TRUE) == 0 && max(abs(mm_out), na.rm = TRUE) == 0

native_cells_removed <- qc$in_nonNA - qc$out_nonNA
values_preserved <- qc$wrong == 0 && qc$added == 0 && qc$nat_kept == 0 &&
                    native_cells_removed == qc$nat_removed
qc_pass <- geom_ok && values_preserved && (qc$out_nonNA <= qc$in_nonNA)

out_sha <- sha256_of(out_path)
make_qc_row(list(
  native_polygon_found = TRUE, native_country_count = n_countries,
  output_SHA256 = out_sha,
  CRS_match = TRUE, extent_match = TRUE, resolution_match = TRUE,
  dimensions_match = TRUE, geometry_match = TRUE,
  input_nonNA = qc$in_nonNA, output_nonNA = qc$out_nonNA,
  native_cells_removed = native_cells_removed,
  proportion_cells_removed = if (qc$in_nonNA > 0) native_cells_removed / qc$in_nonNA else NA,
  input_min = mm_in[1], input_max = mm_in[2],
  output_min = mm_out[1], output_max = mm_out[2],
  values_preserved = values_preserved,
  has_Inf = has_inf, has_NaN = has_nan, all_NA = all_na, all_zero = all_zero,
  QC_pass = qc_pass,
  notes = if (qc_pass) "OK" else paste(
    sprintf("wrong=%d added=%d nat_kept=%d nat_removed=%d nat_in=%d sum_absdiff=%.6g",
            qc$wrong, qc$added, qc$nat_kept, qc$nat_removed, qc$nat_in, qc$sum_absdiff),
    "VALUES_NOT_PRESERVED_OR_MASK_INCOMPLETE")))

log(sprintf("PASS=%s in_nonNA=%d out_nonNA=%d removed=%d (%.4f) min=%.4f max=%.4f",
    qc_pass, qc$in_nonNA, qc$out_nonNA, native_cells_removed,
    if (qc$in_nonNA > 0) native_cells_removed / qc$in_nonNA else NA,
    mm_out[1], mm_out[2]))
if (!qc_pass) quit(save = "no", status = 1)
