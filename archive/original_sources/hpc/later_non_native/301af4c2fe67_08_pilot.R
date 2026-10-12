# Original source: 08_pilot.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/08_pilot.R
# Original source SHA-256: 301af4c2fe671494ac6329633c19c01ad798bd7078c5cb16dc35d73d4ebc7b48
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 08_pilot.R — PHASE 8: 10-taxon pilot (visual + value-preservation check)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))

suppressMessages({ library(terra); library(sf) })
sf_use_s2(FALSE)

mani <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
clean <- read.csv(file.path(DIR_INPUTS, "species_native_mapping_clean.csv"),
                  stringsAsFactors = FALSE)
map_qc <- read.csv(file.path(DIR_MASK_QC, "native_range_mapping_QC.csv"),
                   stringsAsFactors = FALSE)
native <- vect(NATIVE_GPKG)

code_count <- function(x) if (is.na(x) || x == "") 0L else
  length(unique(trimws(unlist(strsplit(x, ";")))))
clean$n_codes <- vapply(clean$native_country_code, code_count, integer(1))
mappable <- clean[clean$n_codes > 0, ]

has_only <- function(pattern, pat_codes = NULL) {
  i <- which(vapply(mappable$native_country_code, function(x) {
    codes <- unique(trimws(unlist(strsplit(x, ";"))))
    length(codes) == 1 && codes[1] %in% pat_codes
  }, logical(1)))
  mappable$accepted_taxon[i]
}

pilot <- unique(c(
  "Spartina alterniflora",                       # mandated; coastal Americas; synonym case
  mappable$accepted_taxon[which.max(mappable$n_codes)],   # many countries
  mappable$accepted_taxon[which.min(mappable$n_codes)],   # single country
  has_only(NULL, "AUS")[1],                      # Australia-only
  has_only(NULL, "CHN")[1],                      # China-only
  has_only(NULL, "FJI")[1],                      # dateline (Fiji) if present
  has_only(NULL, "NZL")[1],                      # island
  mappable$accepted_taxon[grep("^Aca", mappable$accepted_taxon)[1]]  # plus an African-focused? fallback
))

# ensure ~10 distinct taxa with diverse continents; fill from broad-range taxa
geoflags <- read.csv(file.path(DIR_MASK_QC, "native_range_geometry_QC.csv"),
                     stringsAsFactors = FALSE)
tiny <- geoflags$accepted_taxon[grepl("VERY_SMALL_TERRITORY", geoflags$flag_notes)]
pilot <- c(pilot, tiny[1])
afr <- mappable$accepted_taxon[grep("; EGY|; ZAF|; NGA|; KEN|; TZA|; SEN", mappable$native_country_code)][1]
pilot <- c(pilot, afr)
eur <- mappable$accepted_taxon[grep("; GBR|; FRA|; DEU|; ESP|; ITA", mappable$native_country_code)][1]
pilot <- c(pilot, eur)

pilot <- unique(pilot[!is.na(pilot)])
pilot <- head(pilot, 11)
cat("Pilot taxa:\n"); print(pilot)

pdf(file.path(DIR_PILOT, "pilot_10_taxa_maps.pdf"), width = 16, height = 6)

qc_rows <- list()
for (tx in pilot) {
  row <- mani[mani$accepted_taxon == tx, ]
  r <- rast(row$final_projection_path)
  poly <- native[native$accepted_taxon == tx, ]
  poly_sf <- st_as_sf(poly)
  poly_sf <- st_wrap_dateline(poly_sf, options = c("WRAPDATELINE=YES"))
  poly_v <- project(vect(poly_sf), "EPSG:4326")
  out <- mask(r, poly_v, inverse = TRUE)

  # value preservation check (chunked)
  wrong <- 0L; kept_nat <- 0L; in_nn <- 0L; out_nn <- 0L
  m <- rasterize(poly_v, r, field = 1)
  for (st in seq(1, nrow(r), by = 512)) {
    n <- min(512, nrow(r) - st + 1)
    vi <- values(r,   row = st, nrows = n, mat = TRUE)
    vo <- values(out, row = st, nrows = n, mat = TRUE)
    vm <- values(m,   row = st, nrows = n, mat = TRUE)
    in_nn <- in_nn + sum(!is.na(vi)); out_nn <- out_nn + sum(!is.na(vo))
    keep <- !is.na(vo)
    if (any(keep)) wrong <- wrong + sum(abs(vi[keep] - vo[keep]) > 1e-9, na.rm = TRUE)
    nm <- !is.na(vm)
    if (any(nm)) kept_nat <- kept_nat + sum(nm & !is.na(vo))
  }
  preserved <- wrong == 0 && kept_nat == 0

  # plot: global | native polygon | non-native
  par(mfrow = c(1, 3), mar = c(2, 2, 2.5, 2))
  plot(r, main = paste0(tx, " — frozen global"), maxcell = 5e5,
       col = rev(terrain.colors(60)), legend = FALSE)
  plot(poly_v, add = TRUE, border = "black", lwd = 0.5)
  plot(poly_v, main = "mapped native domain", col = "darkred", border = "darkred",
       background = "lightgrey")
  plot(out, main = "non-native suitability", maxcell = 5e5,
       col = rev(terrain.colors(60)), legend = FALSE)

  qc_rows[[length(qc_rows) + 1]] <- data.frame(
    accepted_taxon = tx,
    input_nonNA = in_nn, output_nonNA = out_nn,
    native_cells_removed = in_nn - out_nn,
    values_preserved = preserved,
    native_cells_kept_in_output = kept_nat,
    pilot_QC_pass = preserved && kept_nat == 0 && (out_nn <= in_nn),
    stringsAsFactors = FALSE)
}
dev.off()

pilot_qc <- do.call(rbind, qc_rows)
write.csv(pilot_qc, file.path(DIR_PILOT, "pilot_QC.csv"), row.names = FALSE)
print(pilot_qc)

# write pilot figure PNGs (no PNG device available -> pdftoppm)
system2("pdftoppm", c("-png", "-r", "120",
                      file.path(DIR_PILOT, "pilot_10_taxa_maps.pdf"),
                      file.path(DIR_PILOT, "pilot_map")))
cat("Pilot complete. All pass:", all(pilot_qc$pilot_QC_pass), "\n")
