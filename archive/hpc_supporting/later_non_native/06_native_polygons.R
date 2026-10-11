# SUPPORTING LATER PRODUCT: not established as submitted 2025 workflow.
# Original SHA256: aa1e9400bcee458706ec32ef125627d0dc83c74eea06f39e2b83838af8013ef1
# 06_native_polygons.R — PHASE 6: build species native-range polygons (GeoPackage)
# Country-level polygons (dissolved geoBoundaries ADM1 by ISO3), one geometry per
# accepted taxon. Faster terra-based dissolve.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

clean  <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
                   stringsAsFactors = FALSE)
countries <- vect(COUNTRY_GPKG)

mk_empty <- function() st_sfc(st_multipolygon(), crs = 4326)

polys <- lapply(seq_len(nrow(clean)), function(i) {
  tx   <- clean$accepted_taxon[i]
  code <- clean$native_country_code[i]
  if (is.na(code) || trimws(code) == "") {
    return(st_sf(accepted_taxon = tx, n_native_countries = 0L,
                 native_country_codes = "", native_country_names = "",
                 mapping_source = clean$mapping_status[i],
                 geometry_valid = NA, notes = clean$notes[i],
                 geometry = mk_empty(), stringsAsFactors = FALSE))
  }
  codes <- sort(trimws(unlist(strsplit(code, ";"))))
  sub <- countries[match(codes, countries$iso3), ]
  missing_codes <- setdiff(codes, sub$iso3)
  # terra dissolve (fast GEOS unary union)
  sub <- makeValid(sub)
  u <- aggregate(sub)                     # single multipart geometry
  u_sf <- st_as_sf(u)
  st_sf(
    accepted_taxon = tx,
    n_native_countries = length(codes),
    native_country_codes = paste(codes, collapse = ";"),
    native_country_names = paste(sub$country_name[match(codes, sub$iso3)], collapse = ";"),
    mapping_source = clean$mapping_status[i],
    geometry_valid = st_is_valid(st_geometry(u_sf)),
    notes = if (length(missing_codes)) {
      paste0(clean$notes[i], " CODES_WITHOUT_BOUNDARY: ", paste(missing_codes, collapse = ","))
    } else clean$notes[i],
    geometry = st_geometry(u_sf), stringsAsFactors = FALSE)
})

allp <- do.call(rbind, polys)
allp <- st_set_crs(allp, 4326)
allp <- st_make_valid(allp)
st_write(allp, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)

cat("Wrote:", NATIVE_GPKG, "\n")
cat("rows:", nrow(allp), "with geometry:", sum(!st_is_empty(allp)),
    "empty:", sum(st_is_empty(allp)), "invalid:", sum(!st_is_valid(allp), na.rm = TRUE), "\n")
cat("n_native_countries summary:\n")
print(summary(allp$n_native_countries))
