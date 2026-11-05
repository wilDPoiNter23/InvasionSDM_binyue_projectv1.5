#!/usr/bin/env Rscript
# Intersect cached cohorts and land uses without changing the native grid.
suppressPackageStartupMessages({library(sf); library(terra); library(jsonlite); library(digest)})
a <- commandArgs(TRUE)
if(length(a)!=1L) stop("Supply a private JSON configuration")
cfg <- fromJSON(a[1],simplifyVector=FALSE)
stopifnot(isTRUE(cfg$period_source_verified),length(cfg$responses)==2L)
out <- cfg$joint_output
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"extraction_complete.json"))) stop("Existing extraction retained")
sf_use_s2(FALSE)
coverage_file <- file.path(cfg$screen_output,"city_coverage.csv")
coverage <- read.csv(coverage_file,stringsAsFactors=FALSE)
stopifnot(all(c("city_id","layer","mapped_area_km2","overlap_excess_km2") %in% names(coverage)))
coverage <- coverage[coverage$layer %in% c("coastal_change","coastal_landuse"),]
stopifnot(!anyDuplicated(paste(coverage$layer,coverage$city_id)))
coverage$overlap_threshold_km2 <- pmax(.001,.001*coverage$mapped_area_km2)
coverage$failed_overlap <- !is.finite(coverage$overlap_excess_km2) |
  !is.finite(coverage$mapped_area_km2) |
  coverage$overlap_excess_km2>coverage$overlap_threshold_km2
excluded <- unique(coverage$city_id[coverage$failed_overlap])
write.csv(coverage,file.path(out,"original_city_coverage_with_gate.csv"),row.names=FALSE)
write.csv(coverage[coverage$city_id %in% excluded,],file.path(out,"whole_city_geometry_exclusions.csv"),row.names=FALSE)
cities <- st_transform(st_make_valid(st_read(cfg$cities,quiet=TRUE)),6933)
stopifnot(nrow(cities)==108L,cfg$city_id %in% names(cities))
cities$city_id <- as.character(cities[[cfg$city_id]])
stopifnot(!anyDuplicated(cities$city_id),all(cities$city_id %in% coverage$city_id))
rasters <- lapply(cfg$responses,rast)
stopifnot(all(vapply(rasters,nlyr,numeric(1))==1L))
if(!compareGeom(rasters[[1]],rasters[[2]],stopOnError=FALSE))stop("Paired native grids differ")
r <- c(rasters[[1]],rasters[[2]])
names(r) <- names(cfg$responses)
stopifnot(all(abs(res(r)-1/24)<1e-8),is.lonlat(r))
checkpoint_root <- file.path(cfg$screen_output,"geometry_checkpoints")
source_paths <- c(coverage_file,cfg$cities,unlist(cfg$responses),
  list.files(checkpoint_root,pattern="[.]rds$|input_contract[.]txt$",recursive=TRUE,full.names=TRUE),
  list.files(dirname(cfg$cities),pattern=paste0("^",tools::file_path_sans_ext(basename(cfg$cities)),"[.]"),full.names=TRUE))
source_paths <- unique(source_paths)
checksums <- data.frame(path=source_paths,sha256=vapply(source_paths,function(p)digest(file=p,algo="sha256"),character(1)))
script <- sub("^--file=", "", grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
contract <- digest(list(inputs=checksums,config=cfg,writer=digest(file=script,algo="sha256")),algo="sha256")
contract_file <- file.path(out,"input_contract.txt")
if(file.exists(contract_file) && readLines(contract_file,warn=FALSE)[1]!=contract)stop("Checkpoint inputs differ")
if(!file.exists(contract_file))writeLines(contract,contract_file)
write.csv(checksums,file.path(out,"input_checksums.csv"),row.names=FALSE)
cache_dir <- file.path(out,"joint_checkpoints")
dir.create(cache_dir,showWarnings=FALSE)
summaries <- list(); city_rows <- list(); gates <- list()
extract_common <- function(z) {
  e <- terra::extract(r,vect(st_transform(z,crs(r))),exact=TRUE,cells=TRUE)
  wf <- intersect(c("fraction","weight"),names(e))
  if(length(wf)!=1L)stop("Exact fractions unavailable")
  e$fraction <- e[[wf]]
  e <- e[is.finite(e$fraction) & e$fraction>0 &
    is.finite(e[[names(r)[1]]]) & is.finite(e[[names(r)[2]]]),]
  e
}
summarise_native <- function(e) {
  w <- e$fraction
  c(native_cells=length(unique(e$cell)),covered_cell_equivalents=sum(w),
    effective_cells=if(sum(w^2)>0)sum(w)^2/sum(w^2) else 0,
    setNames(vapply(names(r),function(n)if(sum(w)>0)sum(w*e[[n]])/sum(w) else NA_real_,numeric(1)),names(r)))
}
for(i in seq_len(nrow(cities))) {
  city <- cities$city_id[i]
  cv <- coverage[coverage$city_id==city,]
  stopifnot(nrow(cv)==2L)
  reason <- if(city %in% excluded)"EXCLUDED_WHOLE_CITY_OVERLAP" else
    if(any(cv$mapped_area_km2<=0))"NO_COMMON_MAPPED_COVERAGE" else "ELIGIBLE"
  paths <- file.path(checkpoint_root,c("coastal_change","coastal_landuse"),sprintf("city_%03d.rds",i))
  if(reason=="ELIGIBLE" && !all(file.exists(paths)))stop("Mapped city checkpoint missing: ",city)
  if(reason!="ELIGIBLE") {
    gates[[city]] <- data.frame(city_id=city,status=reason,joint_area_km2=0)
    next
  }
  cache <- file.path(cache_dir,sprintf("city_%03d.rds",i))
  if(file.exists(cache)) ans <- readRDS(cache) else {
    year <- readRDS(paths[1]); use <- readRDS(paths[2])
    stopifnot(all(year$city_id==city),all(use$city_id==city),
      st_crs(year)$epsg==6933,st_crs(use)$epsg==6933,
      !anyDuplicated(year$reported_category),!anyDuplicated(use$reported_category))
    year$period_endpoint <- as.numeric(year$reported_category)
    stopifnot(all(year$period_endpoint %in% seq(1990,2020,5)))
    use$landuse <- as.character(use$reported_category)
    z <- suppressWarnings(st_intersection(year[,"period_endpoint",drop=FALSE],use[,"landuse",drop=FALSE]))
    z <- suppressWarnings(st_collection_extract(z,"POLYGON"))
    z$area_km2 <- as.numeric(st_area(z))/1e6
    z <- z[z$area_km2>0,]
    if(!nrow(z)) {
      ans <- list(summary=NULL,city=NULL,geometry=z,cells=NULL,status="NO_JOINT_INTERSECTION")
    } else {
      key <- paste(z$period_endpoint,z$landuse,sep="|")
      groups <- split(seq_len(nrow(z)),key)
      attrs <- st_drop_geometry(z)[vapply(groups,function(ix)ix[1],integer(1)),c("period_endpoint","landuse")]
      z <- st_sf(attrs,geometry=st_sfc(lapply(groups,function(ix)st_union(st_geometry(z)[ix])[[1]]),crs=6933))
      z$area_km2 <- as.numeric(st_area(z))/1e6
      union_area <- as.numeric(st_area(st_union(st_geometry(z))))/1e6
      overlap_excess <- max(0,sum(z$area_km2)-union_area)
      if(overlap_excess>max(.001,.001*union_area)) {
        ans <- list(summary=NULL,city=NULL,geometry=z,cells=NULL,status="EXCLUDED_JOINT_CATEGORY_OVERLAP")
      } else {
      e <- extract_common(z)
      s <- do.call(rbind,lapply(seq_len(nrow(z)),function(j) {
        d <- e[e$ID==j,]
        data.frame(city_id=city,period_endpoint=z$period_endpoint[j],landuse=z$landuse[j],
          area_km2=z$area_km2[j],as.list(summarise_native(d)),check.names=FALSE)
      }))
      joint <- st_union(st_geometry(z))
      union_e <- extract_common(st_sf(geometry=joint))
      pt <- st_coordinates(st_transform(st_point_on_surface(cities[i,]),4326))[1,]
      area <- as.numeric(st_area(joint))/1e6
      perimeter <- as.numeric(st_length(st_boundary(joint)))/1000
      comp <- aggregate(area_km2~landuse,st_drop_geometry(z),sum)
      crow <- data.frame(city_id=city,joint_area_km2=area,city_area_km2=as.numeric(st_area(cities[i,]))/1e6,
        longitude=pt[1],latitude=pt[2],compactness=if(perimeter>0)4*pi*area/perimeter^2 else NA_real_,
        mean_endpoint=weighted.mean(z$period_endpoint,z$area_km2),
        shared_native_cells=sum(table(e$cell)>1L),as.list(summarise_native(union_e)),check.names=FALSE)
      ans <- list(summary=s,city=crow,composition=comp,geometry=z,cells=e,union_cells=union_e,status="EXTRACTED",overlap_excess_km2=overlap_excess)
      }
    }
    saveRDS(ans,paste0(cache,".part"))
    if(!file.rename(paste0(cache,".part"),cache))stop("Checkpoint rename failed")
  }
  summaries[[city]] <- ans$summary
  city_rows[[city]] <- ans$city
  gates[[city]] <- data.frame(city_id=city,status=ans$status,joint_area_km2=if(is.null(ans$city))0 else ans$city$joint_area_km2)
  cat("City",i,city,ans$status,"\n");flush.console()
}
if(!length(Filter(Negate(is.null),summaries)))stop("No joint extraction")
write.csv(do.call(rbind,summaries),file.path(out,"joint_native_summary.csv"),row.names=FALSE)
write.csv(do.call(rbind,city_rows),file.path(out,"joint_city_summary.csv"),row.names=FALSE)
write.csv(do.call(rbind,gates),file.path(out,"city_eligibility.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"extraction_sessionInfo.txt"))
write_json(list(status="JOINT_EXTRACTION_ONLY",contract=contract,whole_city_exclusions=excluded,
  native_grid_resampling=FALSE,paired_finite_cell_support=TRUE,
  source_calendar="Five-year endpoints; no precise completion dates"),
  file.path(out,"extraction_complete.json"),pretty=TRUE,auto_unbox=TRUE)
