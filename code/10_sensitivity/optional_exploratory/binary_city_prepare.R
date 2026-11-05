# Freeze native polygon-cell fractions and geodesic area weights once.
# Input: private city configuration and fresh support output directory.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite);library(digest)})
sf_use_s2(FALSE)
args <- commandArgs(TRUE)
if(length(args)!=2L)stop("Supply private JSON and support directory")
cfg <- fromJSON(args[1],simplifyVector=TRUE)
if(dir.exists(args[2]))stop("Support directory exists")
pairs <- read.csv(cfg$paired_manifest,stringsAsFactors=FALSE)
if(nrow(pairs)!=684L || anyDuplicated(pairs$species))stop("Frozen paired684 required")
cities <- st_make_valid(st_transform(st_read(cfg$cities,quiet=TRUE),6933))
if(nrow(cities)!=108L || !cfg$city_id %in% names(cities))stop("Expected108 city geometry")
cities$city_id <- as.character(cities[[cfg$city_id]])
if(anyDuplicated(cities$city_id))stop("Duplicate city identifiers")
regions <- list(); gates <- list()
excluded_reclamation <- character()
if(!is.null(cfg$coverage_table)) {
  coverage <- read.csv(cfg$coverage_table,stringsAsFactors=FALSE)
  coverage <- coverage[coverage$layer=="coastal_change",,drop=FALSE]
  limit <- pmax(.001,.001*coverage$mapped_area_km2)
  excluded_reclamation <- coverage$city_id[!is.finite(coverage$overlap_excess_km2) |
    !is.finite(coverage$mapped_area_km2) | coverage$overlap_excess_km2>limit]
}
for(i in seq_len(nrow(cities))) {
  regions[[length(regions)+1L]] <- st_sf(city_id=cities$city_id[i],region="CITY",geometry=st_geometry(cities[i,]))
  cp <- file.path(cfg$reclamation_checkpoint_root,sprintf("city_%03d.rds",i))
  if(cities$city_id[i] %in% excluded_reclamation) {
    gates[[i]] <- data.frame(city_id=cities$city_id[i],status="EXCLUDED_RECLAMATION_OVERLAP")
  } else if(file.exists(cp)) {
    z <- readRDS(cp)
    if(!all(z$city_id==cities$city_id[i]))stop("Checkpoint city identity differs")
    if(nrow(z)>0L) {
      z <- st_transform(st_make_valid(z),st_crs(cities))
      regions[[length(regions)+1L]] <- st_sf(city_id=cities$city_id[i],region="MAPPED_RECLAMATION",geometry=st_union(st_geometry(z)))
      gates[[i]] <- data.frame(city_id=cities$city_id[i],status="MAPPED_RECLAMATION_CHECKPOINT")
    } else gates[[i]] <- data.frame(city_id=cities$city_id[i],status="NO_MAPPED_RECLAMATION")
  } else gates[[i]] <- data.frame(city_id=cities$city_id[i],status="NO_RECLAMATION_CHECKPOINT")
}
regions <- do.call(rbind,regions)
template <- rast(pairs$full_projection[1]);stopifnot(is.lonlat(template),nlyr(template)==1L)
# One native longitude column gives row-specific geodesic cell areas without
# allocating a full global area raster or changing the prediction grid.
column <- rast(nrows=nrow(template),ncols=1,xmin=xmin(template),xmax=xmin(template)+res(template)[1],
  ymin=ymin(template),ymax=ymax(template),crs=crs(template))
row_area <- as.numeric(values(cellSize(column,unit="km",mask=FALSE)))
regions_v <- vect(st_make_valid(st_transform(regions,crs(template))))

cat("Preparing exact native polygon-cell support once\n")
e <- terra::extract(template,regions_v,exact=TRUE,cells=TRUE)
fraction <- intersect(c("fraction","weight"),names(e))
if(length(fraction)!=1L)stop("Exact fractions unavailable")
keep <- is.finite(e$cell) & is.finite(e[[fraction]]) & e[[fraction]]>0
e <- e[keep,,drop=FALSE]
weights <- data.frame(ID=e$ID,cell=e$cell,fraction=e[[fraction]],
  area_km2=row_area[rowFromCell(template,e$cell)]*e[[fraction]])
if(any(!is.finite(weights$area_km2)|weights$area_km2<=0))stop("Invalid native area weights")
cells <- sort(unique(weights$cell));weights$cell_index <- match(weights$cell,cells)
sources <- unique(c(cfg$paired_manifest,cfg$coverage_table,
  list.files(dirname(cfg$cities),pattern=paste0("^",tools::file_path_sans_ext(basename(cfg$cities)),"[.]"),full.names=TRUE),
  list.files(cfg$reclamation_checkpoint_root,pattern="[.]rds$",full.names=TRUE)))
checksums <- data.frame(path=sources,sha256=vapply(sources,function(p)digest(file=p,algo="sha256"),character(1)))
support <- list(regions=st_drop_geometry(regions),gates=do.call(rbind,gates),weights=weights,cells=cells,
  grid=list(nrows=nrow(template),ncols=ncol(template),xmin=xmin(template),xmax=xmax(template),ymin=ymin(template),ymax=ymax(template),crs=crs(template)),
  input_checksums=checksums,paired_manifest_sha256=digest(file=cfg$paired_manifest,algo="sha256"))
dir.create(args[2],recursive=TRUE)
path <- file.path(args[2],"support.rds");saveRDS(support,path,compress=FALSE)
write.csv(checksums,file.path(args[2],"input_checksums.csv"),row.names=FALSE)
write.csv(support$gates,file.path(args[2],"reclamation_scope.csv"),row.names=FALSE)
write_json(list(status="NATIVE_SUPPORT_FROZEN",support_sha256=digest(file=path,algo="sha256"),
  paired_manifest_sha256=support$paired_manifest_sha256,regions=nrow(support$regions),
  native_cells=length(cells),polygon_cell_rows=nrow(weights),
  rule="Exact polygon-cell fractions times native geodesic cell area; per-species FULL/ENV finite support applied later"),
  file.path(args[2],"support_receipt.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Native support frozen:",length(cells),"cells;",nrow(weights),"polygon-cell rows\n")
