# Original source: 06b_native_polygons_L3.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06b_native_polygons_L3.R
# Original source SHA-256: bf02272b5bd0bc5db3a63169f2c82a3f74401d4821b7f4a59185c7e22f9d718d
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06b_native_polygons_L3.R — build native_ranges_813.gpkg from WGSRPD level-3 polygons
# Interpretation (documented): the native-range source (GloNAF) expresses native
# distribution as TDWG level-3 botanical areas. Each token resolves to a WGSRPD
# level-3 polygon; the taxon native domain = union of those polygons.
# ISO3 codes are recorded for reporting only. "South Sudan" uses the country
# polygon (SSD) because the WGSRPD source predates 2011.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")

suppressMessages({ library(sf); library(terra) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
w3 <- vect(WGSRPD_L3_SHP)
iv <- is.valid(w3)
if (any(!iv)) w3 <- makeValid(w3)
w3 <- st_as_sf(w3)
l3_names <- trimws(w3$LEVEL3_NAM)
w3$l3_name <- l3_names

countries <- st_read(COUNTRY_GPKG, quiet = TRUE)
ssd <- countries[countries$iso3 == "SSD", ]
if (nrow(ssd) == 0) stop("SSD polygon missing from country gpkg")

mk_empty <- function() st_sfc(st_multipolygon(), crs = st_crs(w3))

polys <- lapply(seq_len(nrow(clean)), function(i) {
  tx <- clean$accepted_taxon[i]
  codes <- clean$native_country_code[i]
  units <- trimws(unlist(strsplit(clean$native_country_standardised[i], ";")))
  units <- units[!is.na(units) & units != ""]
  if (length(units) == 0) {
    return(st_sf(accepted_taxon = tx, n_native_countries = 0L,
                 native_country_codes = "", native_country_names = "",
                 source = "TDWG_level3", geometry_valid = NA,
                 notes = clean$notes[i], geometry = mk_empty(),
                 stringsAsFactors = FALSE))
  }
  geoms <- list()
  for (u in units) {
    if (u == "South Sudan") {
      geoms[[length(geoms) + 1]] <- st_geometry(st_make_valid(ssd))
    } else {
      idx <- which(l3_names == u)
      if (length(idx) == 1) geoms[[length(geoms) + 1]] <- st_geometry(w3[idx, ])
    }
  }
  if (length(geoms) == 0) {
    return(st_sf(accepted_taxon = tx, n_native_countries = 0L,
                 native_country_codes = "", native_country_names = "",
                 source = "TDWG_level3", geometry_valid = NA,
                 notes = paste(clean$notes[i], "NO_L3_GEOMETRY"),
                 geometry = mk_empty(), stringsAsFactors = FALSE))
  }
  u <- st_union(do.call(c, geoms))
  u <- st_make_valid(u)
  uu <- st_collection_extract(u, "POLYGON")  # keep polygon/multipolygon only
  st_sf(accepted_taxon = tx,
        n_native_countries = if (is.na(codes) || codes == "") 0L else length(trimws(unlist(strsplit(codes, ";")))),
        native_country_codes = if (is.na(codes)) "" else codes,
        native_country_names = paste(sort(units), collapse = ";"),
        source = "TDWG_level3",
        geometry_valid = st_is_valid(uu),
        notes = clean$notes[i],
        geometry = uu, stringsAsFactors = FALSE)
})
allp <- do.call(rbind, polys)
st_write(allp, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)

cat("Wrote:", NATIVE_GPKG, "\n")
cat("rows:", nrow(allp), " nonempty:", sum(!st_is_empty(allp)),
    " empty:", sum(st_is_empty(allp)), " invalid:", sum(!st_is_valid(allp), na.rm = TRUE), "\n")
cat("n_native_countries summary:\n"); print(summary(allp$n_native_countries))
cat("example multipart sizes (n geometry parts):\n")
print(head(sort(vapply(st_geometry(allp), function(g) if (st_is_empty(g)) 0L else length(unlist(g, recursive=FALSE)), integer(1)), decreasing=TRUE), 10))
