# Original source: 06g_rebuild_native_ranges_batched.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/06g_rebuild_native_ranges_batched.R
# Original source SHA-256: 934fdc26526c0559886120d1b7dea02e76bfb75bb45df8b933fe8d9ce1a1a058
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 06g_rebuild_native_ranges_batched.R — batched append build (memory-bounded)
# One taxon = one or more features (no dissolve). Batches of 100 taxa appended
# to the gpkg to avoid any giant in-memory rbind.
source("<NON_NATIVE_ROOT>/02_derived_non_native_potential/07_scripts/00_config.R")
suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"), stringsAsFactors = FALSE)
countries <- vect(COUNTRY_GPKG)
w3 <- vect(WGSRPD_L3_SHP)
w3$l3_name <- trimws(w3$LEVEL3_NAM)

std_cols <- c("accepted_taxon","n_native_countries","native_country_codes",
              "mapping_source","notes")

build_taxon <- function(i) {
  tx    <- clean$accepted_taxon[i]
  code  <- clean$native_country_code[i]
  stmap <- clean$mapping_status[i]
  if (is.na(code) || trimws(code) == "") return(NULL)
  codes <- trimws(unlist(strsplit(code, ";")))
  sub <- countries[match(codes, countries$iso3), ]
  sub <- sub[!is.na(sub), ]
  out <- list()
  if (nrow(sub) >= 1) {
    sub$accepted_taxon <- tx
    sub$n_native_countries <- length(codes)
    sub$native_country_codes <- paste(sort(codes), collapse = ";")
    sub$mapping_source <- stmap
    sub$notes <- clean$notes[i]
    sub <- sub[, std_cols]
    out[[length(out) + 1]] <- sub
  }
  miss <- setdiff(codes, countries$iso3)
  if (length(miss)) {
    l3u <- trimws(unlist(strsplit(clean$native_country_standardised[i], ";")))
    idx <- which(w3$l3_name %in% l3u)
    if (length(idx)) {
      fb <- w3[idx, ]
      fb$accepted_taxon <- tx
      fb$n_native_countries <- length(codes)
      fb$native_country_codes <- paste(sort(codes), collapse = ";")
      fb$mapping_source <- stmap
      fb$notes <- paste(clean$notes[i], "L3_FALLBACK")
      fb <- fb[, std_cols]
      out[[length(out) + 1]] <- fb
    }
  }
  if (length(out) == 1) out[[1]] else do.call(rbind, out)
}

first_file <- TRUE
batches <- split(seq_len(nrow(clean)), ceiling(seq_len(nrow(clean)) / 100))
missing_taxa <- character(0)
for (b in batches) {
  pieces <- lapply(b, build_taxon)
  empty <- b[vapply(pieces, is.null, logical(1))]
  missing_taxa <- c(missing_taxa, clean$accepted_taxon[empty])
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0) next
  batch <- do.call(rbind, pieces)
  if (first_file) {
    writeVector(batch, NATIVE_GPKG, overwrite = TRUE)
    first_file <- FALSE
  } else {
    writeVector(batch, NATIVE_GPKG, insert = TRUE)
  }
  cat("batch done:", b[1], "-", b[length(b)], " features:", nrow(batch), "\n")
}
cat("missing-mapping taxa:", length(missing_taxa), "\n")

# append empty rows
if (length(missing_taxa)) {
  s <- st_read(NATIVE_GPKG, quiet = TRUE)
  empt <- do.call(rbind, lapply(missing_taxa, function(tx) {
    st_sf(data.frame(accepted_taxon = tx, n_native_countries = 0L,
                     native_country_codes = "", mapping_source = "MISSING_NATIVE_MAPPING",
                     notes = "introduced-only distribution in source", stringsAsFactors = FALSE),
          geometry = st_sfc(st_multipolygon(), crs = st_crs(s)))
  }))
  for (col in setdiff(names(s), names(empt))) empt[[col]] <- NA
  empt <- empt[, names(s)]
  st_write(rbind(s, empt), NATIVE_GPKG, delete_dsn = TRUE, quiet = TRUE)
  cat("appended", length(missing_taxa), "empty rows\n")
}

chk <- vect(NATIVE_GPKG)
cat("verify features:", nrow(chk), " unique taxa:", length(unique(chk$accepted_taxon)), "\n")
cat("DONE\n")
