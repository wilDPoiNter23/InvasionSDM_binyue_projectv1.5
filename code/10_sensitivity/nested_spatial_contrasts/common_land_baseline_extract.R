# Three disjoint zones on identical native-grid support for paired responses.
suppressPackageStartupMessages({library(sf);library(terra);library(jsonlite);library(digest)})
script_arg<-grep("^--file=",commandArgs(FALSE),value=TRUE)
if(length(script_arg)!=1L)stop("Run with Rscript")
source(file.path(dirname(sub("^--file=","",script_arg)),"common_land_baseline_geometry.R"))
a<-commandArgs(TRUE);if(length(a)!=1L)stop("Supply private configuration")
cfg<-fromJSON(a[1],simplifyVector=TRUE);sf_use_s2(FALSE)
if(dir.exists(cfg$output_root))stop("Fresh output root required")
dir.create(cfg$output_root,recursive=TRUE);dir.create(file.path(cfg$output_root,"tmp"));dir.create(file.path(cfg$output_root,"cells"))
terraOptions(tempdir=file.path(cfg$output_root,"tmp"),memfrac=.2,progress=0)
cities<-st_make_valid(st_transform(st_read(cfg$cities,quiet=TRUE),6933))
stopifnot(nrow(cities)==108L,cfg$city_id %in% names(cities));cities$city_id<-as.character(cities[[cfg$city_id]])
if(anyDuplicated(cities$city_id))stop("Duplicate cities")
manifest<-read.csv(cfg$paired_manifest,stringsAsFactors=FALSE)
if(nrow(manifest)!=684L || anyDuplicated(manifest$species))stop("Frozen paired684 required")
r<-c(rast(cfg$full_mean),rast(cfg$env_mean),rast(cfg$species_count));names(r)<-c("FULL","ENV","species_count")
if(nlyr(r)!=3L || !is.lonlat(r) || any(abs(res(r)-1/24)>1e-8))stop("Native paired grid contract failed")
for(path in c(cfg$env_mean,cfg$species_count))if(!compareGeom(r[[1]],rast(path),stopOnError=FALSE))stop("Paired grid mismatch")
if(!is.null(cfg$land_mask)) {
  mask<-rast(cfg$land_mask);if(!compareGeom(mask,r[[1]],stopOnError=FALSE))stop("Independent land mask grid differs")
  r<-c(r,mask);names(r)[4]<-"land_mask"
}
column<-rast(nrows=nrow(r),ncols=1,xmin=xmin(r),xmax=xmin(r)+res(r)[1],ymin=ymin(r),ymax=ymax(r),crs=crs(r))
row_area<-as.numeric(values(cellSize(column,unit="km",mask=FALSE)))
city_union<-st_union(st_geometry(cities))
native_geometry<-function(x)st_set_crs(st_make_valid(st_transform(x,crs(r))),NA)
cities_native<-native_geometry(st_geometry(cities))
city_union_native<-st_union(cities_native)
coverage<-read.csv(cfg$coverage_table,stringsAsFactors=FALSE);coverage<-coverage[coverage$layer=="coastal_change",]
if(anyDuplicated(coverage$city_id))stop("Duplicate cohort coverage cities")
bad<-coverage$city_id[!is.finite(coverage$mapped_area_km2)|!is.finite(coverage$overlap_excess_km2)|coverage$overlap_excess_km2>pmax(.001,.001*coverage$mapped_area_km2)]
zones<-c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY")
summary_rows<-list();gates<-list();corrections<-list();checksums<-list()
for(i in seq_len(nrow(cities))) {
  city<-cities$city_id[i];cp<-file.path(cfg$cohort_checkpoint_root,sprintf("city_%03d.rds",i))
  result<-tryCatch({
    if(city %in% bad)stop("COHORT_GEOMETRY_OVERLAP_GATE")
    if(!file.exists(cp))stop("NO_MAPPED_COHORT_CHECKPOINT")
    z<-readRDS(cp);if(!nrow(z) || !all(z$city_id==city))stop("MISSING_OR_MISMATCHED_COHORT")
    if(!"reported_category" %in% names(z))stop("Cohort endpoint identity missing")
    endpoint<-suppressWarnings(as.numeric(as.character(z$reported_category)))
    if(anyNA(endpoint) || any(!endpoint %in% cfg$reported_cohort_endpoints))stop("Unexpected reported cohort endpoints")
    z<-st_make_valid(st_transform(z,6933));reclaimed<-st_union(st_geometry(z))
    reclaimed<-suppressWarnings(st_intersection(st_geometry(cities[i,]),reclaimed))
    if(!length(reclaimed)||all(st_is_empty(reclaimed)))stop("NO_RECLAIMED_INTERSECTION")
    nonreclaimed<-suppressWarnings(st_difference(st_geometry(cities[i,]),reclaimed))
    ring<-suppressWarnings(st_difference(st_buffer(st_geometry(cities[i,]),cfg$ring_metres),city_union))
    geom<-st_sfc(ring[[1]],nonreclaimed[[1]],reclaimed[[1]],crs=6933)
    if(any(st_is_empty(geom)))stop("EMPTY_ZONE_GEOMETRY")
    if(common_polygon_area(suppressWarnings(st_intersection(geom[2],geom[3])))>1e-5)stop("Within-city zones overlap")
    conservation<-abs(common_polygon_area(geom[2:3])-common_polygon_area(cities[i,]))/common_polygon_area(cities[i,])
    if(!is.finite(conservation)||conservation>1e-8)stop("Within-city area is not conserved")
    native<-common_native_partition(cities_native[i],native_geometry(reclaimed),
      native_geometry(st_buffer(st_geometry(cities[i,]),cfg$ring_metres)),city_union_native)
    native_city_area<-common_polygon_area(cities_native[i])
    native_conservation<-abs(common_polygon_area(native[2:3])-native_city_area)/native_city_area
    if(!is.finite(native_conservation)||native_conservation>1e-8)stop("Native within-city area is not conserved")
    for(pair in list(c(1,2),c(1,3),c(2,3))) {
      overlap<-common_polygon_area(suppressWarnings(st_intersection(native[pair[1]],native[pair[2]])))
      if(overlap>1e-12*prod(res(r)))stop("Native zones overlap")
    }
    polygons<-st_sf(zone=zones,geometry=st_set_crs(native,crs(r)))
    geom<-st_transform(st_geometry(polygons),6933)
    e<-terra::extract(r,vect(polygons),exact=TRUE,cells=TRUE)
    fraction_name<-intersect(c("fraction","weight"),names(e));if(length(fraction_name)!=1L)stop("Exact native fractions missing")
    e$fraction<-e[[fraction_name]]
    exceptional<-which(is.finite(e$fraction)&(e$fraction<0|e$fraction>1))
    total_fraction<-tapply(e$fraction,e$cell,sum,na.rm=TRUE)
    exceptional<-union(exceptional,which(e$cell %in% as.numeric(names(total_fraction)[total_fraction>1+1e-10])))
    for(j in exceptional) {
      xy<-xyFromCell(r,e$cell[j]);dx<-res(r)[1]/2;dy<-res(r)[2]/2
      box<-st_sfc(st_polygon(list(rbind(c(xy[1]-dx,xy[2]-dy),c(xy[1]+dx,xy[2]-dy),c(xy[1]+dx,xy[2]+dy),c(xy[1]-dx,xy[2]+dy),c(xy[1]-dx,xy[2]-dy)))))
      pg<-st_set_crs(st_geometry(polygons[e$ID[j],]),NA)
      clip<-suppressWarnings(st_intersection(pg,box));actual<-common_polygon_area(clip)/common_polygon_area(box)
      if(!is.finite(actual)||actual<0||actual>1+1e-10)stop("Independent fraction correction failed")
      corrections[[length(corrections)+1L]]<-data.frame(city_id=city,zone=zones[e$ID[j]],cell=e$cell[j],original=e$fraction[j],independent_geos=actual)
      e$fraction[j]<-min(1,actual)
    }
    total_fraction<-tapply(e$fraction,e$cell,sum,na.rm=TRUE)
    if(any(total_fraction>1+1e-8))stop("Disjoint-zone cell fractions exceed unity")
    if(any(is.finite(e$FULL)&(e$FULL<0|e$FULL>1))||any(is.finite(e$ENV)&(e$ENV<0|e$ENV>1)))stop("Mean score units differ from 0..1")
    if(any(is.finite(e$species_count)&(e$species_count<0|e$species_count>684|e$species_count!=round(e$species_count))))stop("Invalid species denominator")
    e$area_km2<-row_area[rowFromCell(r,e$cell)]*e$fraction
    common<-is.finite(e$FULL)&is.finite(e$ENV)&is.finite(e$species_count)&e$species_count>0&is.finite(e$area_km2)&e$area_km2>0
    if(!is.null(cfg$land_mask))common<-common&!is.na(e$land_mask)&e$land_mask==cfg$land_mask_value
    e$city_id<-city;e$zone<-zones[e$ID]
    write.csv(e[,c("city_id","zone","cell","fraction","area_km2","FULL","ENV","species_count")],file.path(cfg$output_root,"cells",sprintf("city_%03d.csv",i)),row.names=FALSE)
    for(policy in c("STRICT_684","PAIRED_AVAILABLE"))for(zone in zones) {
      take<-common&e$zone==zone;if(policy=="STRICT_684")take<-take&e$species_count==684
      d<-e[take,];weight<-sum(d$area_km2);equivalent<-sum(d$fraction)
      for(group in c("FULL","ENV"))summary_rows[[length(summary_rows)+1L]]<-data.frame(city_id=city,zone=zone,support_policy=policy,group=group,
        mean_suitability=if(weight>0)sum(d$area_km2*d[[group]])/weight else NA_real_,
        finite_native_cells=length(unique(d$cell)),native_cell_equivalents=equivalent,common_area_km2=weight,
        minimum_contributing_species=if(nrow(d))min(d$species_count)else NA_real_,maximum_contributing_species=if(nrow(d))max(d$species_count)else NA_real_,
        area_weighted_species_count=if(weight>0)sum(d$area_km2*d$species_count)/weight else NA_real_,
        geom_zone_area_km2=common_polygon_area(geom[match(zone,zones)])/1e6)
    }
    checksums[[length(checksums)+1L]]<-data.frame(city_id=city,path=cp,sha256=digest(file=cp,algo="sha256"))
    data.frame(city_id=city,status="EXTRACTED",reason="",within_city_area_relative_error=conservation)
  },error=function(error)data.frame(city_id=city,status="EXCLUDED_ALL_THREE_ZONES",reason=conditionMessage(error),within_city_area_relative_error=NA_real_))
  gates[[i]]<-result
  write.csv(do.call(rbind,gates),file.path(cfg$output_root,"city_geometry_status.csv"),row.names=FALSE)
  if(length(summary_rows))write.csv(do.call(rbind,summary_rows),file.path(cfg$output_root,"zone_source_means.csv"),row.names=FALSE)
  if(length(corrections))write.csv(do.call(rbind,corrections),file.path(cfg$output_root,"native_fraction_corrections.csv"),row.names=FALSE)
  if(length(checksums))write.csv(do.call(rbind,checksums),file.path(cfg$output_root,"cohort_source_checksums.csv"),row.names=FALSE)
  cat(city,result$status,result$reason,"\n");flush.console()
}
if(!length(summary_rows))stop("No eligible three-zone geometry")
write.csv(do.call(rbind,summary_rows),file.path(cfg$output_root,"zone_source_means.csv"),row.names=FALSE)
write.csv(do.call(rbind,gates),file.path(cfg$output_root,"city_geometry_status.csv"),row.names=FALSE)
if(length(corrections))write.csv(do.call(rbind,corrections),file.path(cfg$output_root,"native_fraction_corrections.csv"),row.names=FALSE)
if(length(checksums))write.csv(do.call(rbind,checksums),file.path(cfg$output_root,"cohort_source_checksums.csv"),row.names=FALSE)
city_components<-list.files(dirname(cfg$cities),pattern=paste0("^",tools::file_path_sans_ext(basename(cfg$cities)),"[.]"),full.names=TRUE)
paths<-unique(c(city_components,cfg$paired_manifest,cfg$full_mean,cfg$env_mean,cfg$species_count,cfg$coverage_table,cfg$land_mask))
write.csv(data.frame(path=paths,sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1))),file.path(cfg$output_root,"input_checksums.csv"),row.names=FALSE)
write_json(list(status="EXTRACTED",geometry_scope="Current mapped reported change-year endpoints; not historical39percent reconstruction",
  reported_cohort_endpoints=cfg$reported_cohort_endpoints,ring_metres=cfg$ring_metres,
  ring_definition="EPSG6933 city buffer transformed to native CRS then minus union of all108 native city polygons",
  partition_definition="Reclaimed clipped to native city; non-reclaimed native city minus reclaimed; all zone differences formed in native CRS",
  support="Native1/24degree; exact polygon fractions times geodesic cell area; same FULL/ENV cells and weights",
  land_mask=if(is.null(cfg$land_mask))"No independent shoreline mask: common finite modeled terrestrial support only"else cfg$land_mask,
  response_identity="FULL retains human predictors including reclamation where fitted; ENV excludes all specified human predictors",
  available_species_policy="Per-cell FULL/ENV membership identical; spatial membership may differ and N is reported"),file.path(cfg$output_root,"extraction_receipt.json"),auto_unbox=TRUE,pretty=TRUE)
