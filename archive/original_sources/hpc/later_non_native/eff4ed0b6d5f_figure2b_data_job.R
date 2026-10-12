# Original source: figure2b_data_job.R
# Original source location: ${NON_NATIVE_ROOT}/temp/figure2b_data_job.R
# Original source SHA-256: eff4ed0b6d5fe3e98ccd76a79afe1cabf35e14506b2f88a971a4e4fd0aa5969b
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(data.table))

FIG <- "<NON_NATIVE_ROOT>/publication_figures"
acc <- read.csv("<WORK_ROOT>/publication_release_v1/manifests/final_accepted_projection_manifest_813_v3.csv", stringsAsFactors = FALSE)$accepted_taxon

cat("Reading CSV (species column only)...", as.character(Sys.time()), "\n")
d <- fread("<HPC_INPUT_ROOT>/species_geodatapoint_cleaned.csv", select = c("species", "decimalLatitude", "decimalLongitude"))
cat("Rows:", nrow(d), "\n")
d <- d[species %chin% acc]
cat("Rows after 813 filter:", nrow(d), "\n")

# (a) per-species counts for accepted taxa only
per_sp <- d[, .N, by = species][order(-N)]
fwrite(per_sp, file.path(FIG, "figure2_records_per_species_813.csv"))
cat("813 per-species: min", min(per_sp$N), " median", median(per_sp$N), " max", max(per_sp$N), "\n")

tpl <- rast(nrows = 4320, ncols = 8640, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
lat <- d$decimalLatitude; lon <- d$decimalLongitude
ok <- is.finite(lat) & is.finite(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
cell <- cellFromXY(tpl, cbind(lon[ok], lat[ok]))
sp <- d$species[ok]
dt <- data.table(sp = sp, cell = cell)
dens <- dt[, .N, by = cell]
r_dens <- tpl; r_dens[dens$cell] <- dens$N
writeRaster(r_dens, file.path(FIG, "figure2_record_density_813.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW", "TILED=YES"))
rich <- dt[, .(nsp = uniqueN(sp)), by = cell]
r_rich <- tpl; r_rich[rich$cell] <- rich$nsp
writeRaster(r_rich, file.path(FIG, "figure2_species_richness_813.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW", "TILED=YES"))
cat("density max:", max(dens$N), " richness max:", max(rich$nsp), "\n")
cat("DONE", as.character(Sys.time()), "\n")
