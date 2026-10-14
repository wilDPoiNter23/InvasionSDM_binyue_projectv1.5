# Native-cell area-weighted threshold responses on paired species and polygons.
# Input: private JSON with paired_manifest, thresholds, cities, city_id,
# reclamation_checkpoint_root, output_directory, optional first_species/count.
# Output: per-species suitable area and common finite support for both groups.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
if(!length(args) %in% c(1L,4L))stop("Supply private JSON [output_directory first_species count]")
cfg <- fromJSON(args[1],simplifyVector=TRUE)
if(length(args)==4L) {
  cfg$output_directory <- args[2];cfg$first_species <- as.integer(args[3]);cfg$count <- as.integer(args[4])
}
if(dir.exists(cfg$output_directory))stop("Output directory exists")
pairs <- read.csv(cfg$paired_manifest,stringsAsFactors=FALSE)
thresholds <- read.csv(cfg$thresholds,stringsAsFactors=FALSE)
if(anyDuplicated(pairs$species) || nrow(pairs)!=684L)stop("Frozen paired684 roster required")
if(anyDuplicated(paste(thresholds$species,thresholds$group)))stop("Duplicate thresholds")
if(is.null(cfg$city_support_directory))stop("Frozen city_support_directory required")
support_file <- file.path(cfg$city_support_directory,"support.rds")
support_receipt <- fromJSON(file.path(cfg$city_support_directory,"support_receipt.json"))
if(digest(file=support_file,algo="sha256")!=support_receipt$support_sha256)stop("Frozen support checksum differs")
support <- readRDS(support_file)
if(digest(file=cfg$paired_manifest,algo="sha256")!=support$paired_manifest_sha256)stop("Paired roster differs from support")
regions <- support$regions; grid <- support$grid
reg_template <- rast(nrows=grid$nrows,ncols=grid$ncols,xmin=grid$xmin,xmax=grid$xmax,ymin=grid$ymin,ymax=grid$ymax,crs=grid$crs)
template <- reg_template
area <- support$weights$area_km2
region_indices <- split(seq_len(nrow(support$weights)),factor(support$weights$ID,levels=seq_len(nrow(regions))))
first <- if(is.null(cfg$first_species))1L else as.integer(cfg$first_species)
count <- if(is.null(cfg$count))nrow(pairs) else as.integer(cfg$count)
if(first<1L || count<1L || first>nrow(pairs))stop("Invalid species range")
ix <- seq.int(first,min(nrow(pairs),first+count-1L))
dir.create(cfg$output_directory,recursive=TRUE);write.csv(support$gates,file.path(cfg$output_directory,"reclamation_scope.csv"),row.names=FALSE)
result <- list()
for(i in ix) {
  pair <- pairs[i,,drop=FALSE]
  full <- rast(pair$full_projection); env <- rast(pair$env_projection)
  if(!compareGeom(full,env,stopOnError=FALSE) || !compareGeom(full,template,stopOnError=FALSE))stop("Paired geometry differs")
  rr <- c(full,env);names(rr) <- c("FULL","ENV")
  values <- terra::extract(rr,support$cells)
  e <- support$weights
  e$FULL <- values$FULL[e$cell_index];e$ENV <- values$ENV[e$cell_index]
  valid <- is.finite(e$FULL) & is.finite(e$ENV) & is.finite(area) & area>0
  for(group in c("FULL","ENV")) {
    tt <- thresholds[thresholds$species==pair$species & thresholds$group==group,,drop=FALSE]
    if(nrow(tt)!=1L || tt$projection_path!=pair[[paste0(tolower(group),"_projection")]])stop("Threshold/product identity mismatch")
    raw <- e[[group]]
    if(any(raw[is.finite(raw)]<0 | raw[is.finite(raw)]>as.numeric(pair[[paste0(tolower(group),"_scale_divisor")]])))stop("Score scale mismatch")
    for(method in c("t10","max_tss")) {
      cutoff <- tt[[paste0(method,"_raw")]]
      for(j in seq_len(nrow(regions))) {
        selected <- region_indices[[j]]
        selected <- selected[valid[selected]]
        denominator <- sum(area[selected])
        numerator <- sum(area[selected[raw[selected]>=cutoff]],na.rm=TRUE)
        result[[length(result)+1L]] <- data.frame(species=pair$species,group=group,method=method,
          city_id=regions$city_id[j],region=regions$region[j],cutoff_raw=cutoff,
          common_finite_cells=length(unique(e$cell[selected])),
          common_finite_area_km2=denominator,suitable_area_km2=numerator,
          suitable_fraction=if(denominator>0)numerator/denominator else NA_real_,
          support="PER_SPECIES_FULL_ENV_COMMON_FINITE_NATIVE_CELLS",
          validation_scope="CURRENT_MAP_CALIBRATION_POOLED_PA")
      }
    }
  }
}
write.csv(do.call(rbind,result),file.path(cfg$output_directory,"city_species_threshold_area.csv"),row.names=FALSE)
write_json(list(status="EXTRACTED",frozen_paired_species=684L,first_species=first,species_processed=length(ix),
  paired_manifest_sha256=digest(file=cfg$paired_manifest,algo="sha256"),
  thresholds_sha256=digest(file=cfg$thresholds,algo="sha256"),
  scope="108 full city polygons; mapped reclamation checkpoint availability reported separately",
  weights="Frozen native cell geodesic area multiplied by exact polygon cell fraction",
  support_sha256=support_receipt$support_sha256,
  interpretation="Suitable species area is not unique habitat area. Cell support is frozen per species and pair; denominator species count must accompany aggregate responses."),
  file.path(cfg$output_directory,"extraction_receipt.json"),pretty=TRUE,auto_unbox=TRUE)
