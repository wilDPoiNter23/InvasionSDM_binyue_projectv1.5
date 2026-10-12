# Original source: 06d_rebuild_native_ranges_terra.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06d_rebuild_native_ranges_terra.R
# Original source SHA-256: 4d88605d1edb8a6652f543718768034d005a8a648ca51807a3dbf66473892aa0
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06d_rebuild_native_ranges_terra.R — compact rebuild of native_ranges_813.gpkg
# Replaces the bloated 8.1 GB layer with a compact country-level layer built with
# terra::aggregate (fast dissolve). Same design as 06c:
#   country polygons by ISO3 (whole-country masking), WGSRPD L3 fallback for
#   territory codes absent from the country layer, documented.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
countries <- vect(COUNTRY_GPKG)
w3 <- vect(WGSRPD_L3_SHP)
w3$l3_name <- trimws(w3$LEVEL3_NAM)

rows_geom <- list(); rows_empty <- list()
for (i in seq_len(nrow(clean))) {
  tx    <- clean$accepted_taxon[i]
  code  <- clean$native_country_code[i]
  stmap <- clean$mapping_status[i]
  if (is.na(code) || trimws(code) == "") {
    rows_empty[[length(rows_empty) + 1]] <- data.frame(
      accepted_taxon = tx, n_native_countries = 0L,
      native_country_codes = "", native_country_names = "",
      mapping_source = stmap, notes = clean$notes[i], stringsAsFactors = FALSE)
    next
  }
  codes <- trimws(unlist(strsplit(code, ";")))
  parts <- list(); names_used <- character(0)
  for (cc in codes) {
    sub <- countries[countries$iso3 == cc, ]
    if (nrow(sub) >= 1) {
      parts[[length(parts) + 1]] <- sub
      nm <- sub$country_name[1]
      names_used <- c(names_used, if (!is.na(nm) && nm != "") nm else cc)
    } else {
      l3u <- trimws(unlist(strsplit(clean$native_country_standardised[i], ";")))
      idx <- which(w3$l3_name %in% l3u)
      if (length(idx)) {
        parts[[length(parts) + 1]] <- w3[idx[1], ]
        names_used <- c(names_used, paste0(cc, "[L3]"))
      }
    }
  }
  if (length(parts) == 0) {
    rows_empty[[length(rows_empty) + 1]] <- data.frame(
      accepted_taxon = tx, n_native_countries = length(codes),
      native_country_codes = code, native_country_names = "",
      mapping_source = stmap, notes = paste(clean$notes[i], "NO_BOUNDARY_GEOMETRY"),
      stringsAsFactors = FALSE)
    next
  }
  comb <- do.call(rbind, parts)
  u <- aggregate(comb, dissolve = TRUE)
  if (nrow(u) > 1) u <- aggregate(u, dissolve = TRUE)
  u$accepted_taxon <- tx
  u$n_native_countries <- length(codes)
  u$native_country_codes <- paste(sort(codes), collapse = ";")
  u$native_country_names <- paste(names_used, collapse = ";")
  u$mapping_source <- stmap
  u$notes <- clean$notes[i]
  keep <- c("accepted_taxon","n_native_countries","native_country_codes",
            "native_country_names","mapping_source","notes")
  u <- u[, keep]
  rows_geom[[length(rows_geom) + 1]] <- u
  if (i %% 100 == 0) cat("built", i, "/", nrow(clean), "\n")
}
main <- do.call(rbind, rows_geom)
cat("geometric rows:", nrow(main), " empty rows:", length(rows_empty), "\n")
writeVector(main, NATIVE_GPKG, overwrite = TRUE)
cat("main layer written.\n")

# append the empty-geometry rows via sf (compact layer, fast)
s <- st_read(NATIVE_GPKG, quiet = TRUE)
if (length(rows_empty)) {
  empt <- do.call(rbind, lapply(rows_empty, function(rr) {
    st_sf(rr, geometry = st_sfc(st_multipolygon(), crs = st_crs(s)), stringsAsFactors = FALSE)
  }))
  for (col in setdiff(names(s), names(empt))) empt[[col]] <- NA
  empt <- empt[, names(s)]
  s2 <- rbind(s, empt)
  s2 <- s2[order(match(s2$accepted_taxon, clean$accepted_taxon)), ]
  st_write(s2, NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)
  cat("appended", length(rows_empty), "empty rows; total", nrow(s2), "\n")
}
chk <- vect(NATIVE_GPKG)
cat("verify terra read rows:", nrow(chk), " cols:", paste(names(chk), collapse=","), "\n")
cat("DONE\n")
