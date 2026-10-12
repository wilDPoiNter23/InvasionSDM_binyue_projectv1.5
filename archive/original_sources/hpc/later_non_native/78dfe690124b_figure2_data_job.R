# Original source: figure2_data_job.R
# Original source location: ${NON_NATIVE_ROOT}/temp/figure2_data_job.R
# Original source SHA-256: 78dfe690124b3e40ed940f698cf9992685df9f76294188b7787a57acde64c61f
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(data.table))

FIG <- "<NON_NATIVE_ROOT>/publication_figures"
cat("Reading occurrence CSV...", as.character(Sys.time()), "\n")
d <- fread("<HPC_INPUT_ROOT>/species_geodatapoint_cleaned.csv", select = c("species", "decimalLatitude", "decimalLongitude"))
cat("Rows:", nrow(d), "Species:", uniqueN(d$species), "\n")

# (a) records per species
per_sp <- d[, .N, by = species][order(-N)]
fwrite(per_sp, file.path(FIG, "figure2_records_per_species.csv"))
cat("per-species counts written. n species:", nrow(per_sp),
    " min:", min(per_sp$N), " median:", median(per_sp$N), " max:", max(per_sp$N), "\n")

# (b) grid cell maps at final-product grid (2.5 arc-min)
tpl <- rast(nrows = 4320, ncols = 8640, xmin = -180, xmax = 180, ymin = -90, ymax = 90)
lat <- d$decimalLatitude; lon <- d$decimalLongitude
ok <- is.finite(lat) & is.finite(lon) & lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
cat("valid rows:", sum(ok), "\n")
lat <- lat[ok]; lon <- lon[ok]; sp <- d$species[ok]
cell <- cellFromXY(tpl, cbind(lon, lat))
dt <- data.table(sp = sp, cell = cell)
# record density
dens <- dt[, .N, by = cell]
r_dens <- tpl
r_dens[dens$cell] <- dens$N
writeRaster(r_dens, file.path(FIG, "figure2_record_density.tif"), overwrite = TRUE,
            gdal = c("COMPRESS=LZW", "TILED=YES"))
# species richness (unique species per cell)
rich <- dt[, .(nsp = uniqueN(sp)), by = cell]
r_rich <- tpl
r_rich[rich$cell] <- rich$nsp
writeRaster(r_rich, file.path(FIG, "figure2_species_richness.tif"), overwrite = TRUE,
            gdal = c("COMPRESS=LZW", "TILED=YES"))
cat("density max:", max(dens$N), " richness max:", max(rich$nsp), "\n")
cat("DONE", as.character(Sys.time()), "\n")
