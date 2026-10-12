# Original source: 06c_native_polygons_country_level.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06c_native_polygons_country_level.R
# Original source SHA-256: 3efaf97daa0b1eebc618bbe6f275a6ec3b58c558f4a7fb1b275a778e231d2411
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06c_native_polygons_country_level.R — country-level native domains with L3 fallback
# Design (aligned with project boundary rules and the 09 consistency guard):
#   - Primary geometry: geoBoundaries country polygons (ADM1 dissolved by ISO3)
#     from native_country_code (country-level masking; a TDWG area inside a
#     country masks the whole country — conservative approximation).
#   - Fallback geometry: WGSRPD level-3 polygons for codes with NO ADM1 country
#     coverage (island/territory tokens, e.g. REU, CYM, PYF, TKL...), documented
#     in the crosswalk as MANUAL_CONFIRMED.
#   - MISSING_MAPPING taxa (3) get empty geometry.
# Field `mapping_source` is required by 09_mask_one_taxon.R's consistency guard.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")

suppressMessages({ library(sf); library(terra) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
cw    <- read.csv(file.path(DIR_INPUTS, "native_country_boundary_crosswalk.csv"), stringsAsFactors = FALSE)

countries <- st_read(COUNTRY_GPKG, quiet = TRUE)
countries <- st_make_valid(countries)

w3 <- vect(WGSRPD_L3_SHP)
if (any(!is.valid(w3))) w3 <- makeValid(w3)
w3 <- st_as_sf(w3)
w3$l3_name <- trimws(w3$LEVEL3_NAM)

# token -> L3 unit names (from crosswalk)
tok_l3 <- split(cw$native_country_standardised[!is.na(cw$native_country_standardised)],
                cw$native_country_original[!is.na(cw$native_country_standardised)])
tok_l3 <- lapply(tok_l3, unique)

mk_empty <- function() st_sfc(st_multipolygon(), crs = st_crs(countries))

polys <- lapply(seq_len(nrow(clean)), function(i) {
  tx    <- clean$accepted_taxon[i]
  code  <- clean$native_country_code[i]
  st_map <- clean$mapping_status[i]
  if (is.na(code) || trimws(code) == "") {
    return(st_sf(accepted_taxon = tx, n_native_countries = 0L,
                 native_country_codes = "", native_country_names = "",
                 mapping_source = st_map, source = "country_level",
                 geometry_valid = NA, notes = clean$notes[i],
                 geometry = mk_empty(), stringsAsFactors = FALSE))
  }
  codes <- trimws(unlist(strsplit(code, ";")))
  geoms <- list()
  names_used <- character(0)
  for (cc in codes) {
    sub <- countries[countries$iso3 == cc, ]
    if (nrow(sub) >= 1) {
      geoms[[length(geoms) + 1]] <- st_geometry(sub[1, ])
      names_used <- c(names_used, cc)
    } else {
      # fallback: WGSRPD level-3 polygon for this territory
      l3u <- unlist(tok_l3[[cc]], use.names = FALSE)
      if (length(l3u)) {
        idx <- which(w3$l3_name %in% l3u)
        if (length(idx)) {
          geoms[[length(geoms) + 1]] <- st_geometry(w3[idx[1], ])
          names_used <- c(names_used, paste0(cc, "[L3:", l3u[1], "]"))
        }
      }
    }
  }
  if (length(geoms) == 0) {
    return(st_sf(accepted_taxon = tx, n_native_countries = length(codes),
                 native_country_codes = code, native_country_names = "",
                 mapping_source = st_map, source = "country_level",
                 geometry_valid = NA,
                 notes = paste(clean$notes[i], "NO_BOUNDARY_GEOMETRY"),
                 geometry = mk_empty(), stringsAsFactors = FALSE))
  }
  u <- st_union(do.call(c, geoms))
  u <- st_make_valid(u)
  uu <- st_collection_extract(u, "POLYGON")
  uu <- st_cast(uu, "MULTIPOLYGON")
  st_sf(accepted_taxon = tx,
        n_native_countries = length(codes),
        native_country_codes = paste(sort(codes), collapse = ";"),
        native_country_names = paste(names_used, collapse = ";"),
        mapping_source = st_map, source = "country_level",
        geometry_valid = st_is_valid(uu),
        notes = clean$notes[i],
        geometry = uu, stringsAsFactors = FALSE)
})
allp <- do.call(rbind, polys)
allp <- st_cast(allp, "MULTIPOLYGON")
st_write(allp, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)

# read-back verification (guards against corrupt writes)
chk <- tryCatch(terra::vect(NATIVE_GPKG), error = function(e) NULL)
if (is.null(chk)) {
  stop("NATIVE_GPKG written but unreadable by terra — corrupt output")
}
cat("Wrote and verified readable:", NATIVE_GPKG, "rows:", nrow(chk), "\n")
cat("rows:", nrow(allp), " nonempty:", sum(!st_is_empty(allp)),
    " empty:", sum(st_is_empty(allp)), " invalid:", sum(!st_is_valid(allp), na.rm = TRUE), "\n")
cat("mapping_source present:", "mapping_source" %in% names(allp), "\n")
cat("n_native_countries summary:\n"); print(summary(allp$n_native_countries))
