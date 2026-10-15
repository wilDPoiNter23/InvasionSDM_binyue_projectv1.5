# Independent native-value, denominator and cached-weight response verification.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);if(length(a)!=2L)stop("Supply private configuration and fresh verification output")
if(file.exists(a[2]))stop("Verification output exists")
cfg<-fromJSON(a[1],simplifyVector=TRUE)
receipt<-fromJSON(file.path(cfg$output_root,"extraction_receipt.json"))
if(is.null(receipt$partition_definition) || !grepl("differences formed in native CRS",receipt$partition_definition,fixed=TRUE))stop("Native partition definition missing")
d<-read.csv(file.path(cfg$output_root,"zone_source_means.csv"),stringsAsFactors=FALSE)
gates<-read.csv(file.path(cfg$output_root,"city_geometry_status.csv"),stringsAsFactors=FALSE)
stopifnot(nrow(gates)==108L,!anyDuplicated(gates$city_id),
 all(gates$status %in% c("EXTRACTED","EXCLUDED_ALL_THREE_ZONES")),
 all(nzchar(gates$reason[gates$status!="EXTRACTED"])),
 setequal(unique(d$city_id),gates$city_id[gates$status=="EXTRACTED"]))
hashes<-read.csv(file.path(cfg$output_root,"input_checksums.csv"),stringsAsFactors=FALSE)
stopifnot(all(vapply(seq_len(nrow(hashes)),function(i)digest(file=hashes$path[i],algo="sha256")==hashes$sha256[i],logical(1))))
r<-c(rast(cfg$full_mean),rast(cfg$env_mean),rast(cfg$species_count));names(r)<-c("FULL","ENV","species_count")
column<-rast(nrows=nrow(r),ncols=1,xmin=xmin(r),xmax=xmin(r)+res(r)[1],ymin=ymin(r),ymax=ymax(r),crs=crs(r))
row_area<-as.numeric(values(cellSize(column,unit="km",mask=FALSE)))
checks<-0L;max_error<-0;sample_checks<-0L
for(path in list.files(file.path(cfg$output_root,"cells"),pattern="[.]csv$",full.names=TRUE)) {
  e<-read.csv(path,stringsAsFactors=FALSE);city<-unique(e$city_id);stopifnot(length(city)==1L)
  if(!city %in% gates$city_id[gates$status=="EXTRACTED"])next
  stopifnot(all(is.na(e$fraction)|(e$fraction>=0&e$fraction<=1)),max(tapply(e$fraction,e$cell,sum,na.rm=TRUE))<=1+1e-8)
  area<-row_area[rowFromCell(r,e$cell)]*e$fraction
  stopifnot(max(abs(area-e$area_km2),na.rm=TRUE)<1e-8)
  ix<-unique(round(seq(1,nrow(e),length.out=min(10001,nrow(e)))))
  actual<-extract(r,e$cell[ix])
  for(group in c("FULL","ENV","species_count")) {
    stopifnot(identical(is.na(actual[[group]]),is.na(e[[group]][ix])))
    delta<-abs(actual[[group]]-e[[group]][ix]);stopifnot(all(delta[is.finite(delta)]<1e-10))
  }
  sample_checks<-sample_checks+length(ix)
  common<-is.finite(e$FULL)&is.finite(e$ENV)&is.finite(e$species_count)&e$species_count>0&is.finite(area)&area>0
  if(!is.null(cfg$land_mask)){land<-extract(rast(cfg$land_mask),e$cell)[[1]];common<-common&!is.na(land)&land==cfg$land_mask_value}
  for(policy in c("STRICT_684","PAIRED_AVAILABLE"))for(zone in unique(e$zone)) {
    take<-common&e$zone==zone;if(policy=="STRICT_684")take<-take&e$species_count==684
    denominator<-sum(area[take]);source<-d[d$city_id==city&d$zone==zone&d$support_policy==policy,]
    stopifnot(nrow(source)==2L,all(abs(source$common_area_km2-denominator)<1e-8),
      all(abs(source$native_cell_equivalents-sum(e$fraction[take]))<1e-8))
    for(group in c("FULL","ENV")) {
      q<-source[source$group==group,];expected<-if(denominator>0)sum(area[take]*e[[group]][take])/denominator else NA_real_
      stopifnot(identical(is.na(expected),is.na(q$mean_suitability)))
      if(is.finite(expected)){error<-abs(expected-q$mean_suitability);stopifnot(error<1e-10);max_error<-max(max_error,error)}
      checks<-checks+1L
    }
  }
}
sample<-read.csv(file.path(cfg$output_root,"common_baseline_city_source.csv"),stringsAsFactors=FALSE)
for(policy in unique(sample$support_policy))for(threshold in unique(sample$minimum_cell_equivalents)) {
  x<-sample[sample$support_policy==policy&sample$minimum_cell_equivalents==threshold,]
  sets<-split(x$city_id,paste(x$group,x$zone));stopifnot(length(sets)==6L,all(vapply(sets,function(z)setequal(z,sets[[1]]),logical(1))))
  for(group in c("FULL","ENV")) {
    z<-x[x$group==group,];ring<-z[z$zone=="OUTSIDE_RING",]
    expected<-100*z$mean_suitability/ring$mean_suitability[match(z$city_id,ring$city_id)]
    stopifnot(max(abs(expected-z$ring_100_index))<1e-8)
  }
}
write_json(list(status="PASS",native_cell_sample_checks=sample_checks,zone_response_checks=checks,
  maximum_weighted_mean_difference=max_error,checks="Native values, FULL/ENV common denominator and weights, disjoint cell fractions, identical city samples, ring100 identity; input hashes",
  causal_validation=FALSE),a[2],auto_unbox=TRUE,pretty=TRUE)
