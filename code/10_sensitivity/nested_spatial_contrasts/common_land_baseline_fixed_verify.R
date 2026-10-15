# Independent all-city response reconstruction and sampled261-species sums.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);if(length(a)!=2L||file.exists(a[2]))stop("Supply configuration and fresh receipt")
cfg<-fromJSON(a[1]);out<-cfg$output_root;receipt<-fromJSON(file.path(out,"extraction_receipt.json"))
stopifnot(receipt$status=="FIXED_261_COMPUTED",receipt$frozen_species_n==261L,receipt$core_roster_sha256==cfg$core_roster_sha256,digest(file=cfg$core_roster,algo="sha256")==cfg$core_roster_sha256)
for(pair in list(c("input_checksums.csv","input_manifest_sha256"),c("projection_checksums.csv","projection_manifest_sha256"),c("core_cell_values.csv","cell_values_sha256")))stopifnot(digest(file=file.path(out,pair[1]),algo="sha256")==receipt[[pair[2]]])
h<-read.csv(file.path(out,"input_checksums.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(h)))stopifnot(digest(file=h$path[i],algo="sha256")==h$sha256[i])
p<-read.csv(file.path(out,"projection_checksums.csv"),stringsAsFactors=FALSE)
roster<-read.csv(cfg$core_roster,stringsAsFactors=FALSE)$species
stopifnot(nrow(p)==522L,!anyDuplicated(paste(p$species,p$group)),setequal(p$species,roster),setequal(p$group,c("FULL","ENV")))
stopifnot(identical(read.csv(file.path(out,"core_species_roster.csv"),stringsAsFactors=FALSE)$species,roster))
paired<-read.csv(cfg$paired_manifest,stringsAsFactors=FALSE)
for(i in seq_len(nrow(p))){m<-p[i,];q<-paired[paired$species==m$species,];g<-tolower(m$group);stopifnot(nrow(q)==1L,q[[paste0(g,"_projection")]]==m$path,q[[paste0(g,"_scale_divisor")]]==m$scale_divisor)}
v<-read.csv(file.path(out,"core_cell_values.csv"));stopifnot(!anyDuplicated(v$cell),all(diff(v$cell)>0),all(v$joint_finite_species%in%0:261))
for(g in c("FULL","ENV")){good<-v$joint_finite_species==261L;stopifnot(identical(is.finite(v[[g]]),good),all(abs(v[[g]][good]-v[[paste0(g,"_sum")]][good]/261)<1e-12))}
set.seed(cfg$seed);ix<-sort(sample.int(nrow(v),min(32L,nrow(v))));sums<-matrix(0,nrow=length(ix),ncol=2,dimnames=list(NULL,c("FULL","ENV")));count<-integer(length(ix))
template<-rast(p$path[1]);stopifnot(compareGeom(template,rast(cfg$grid_template),stopOnError=FALSE))
for(species in roster){values<-list()
 for(g in c("FULL","ENV")){m<-p[p$species==species&p$group==g,];stopifnot(nrow(m)==1L,is.finite(m$scale_divisor),m$scale_divisor>0,digest(file=m$path,algo="sha256")==m$sha256)
 r<-rast(m$path);stopifnot(nlyr(r)==1L,compareGeom(template,r,stopOnError=FALSE));x<-as.numeric(extract(r,v$cell[ix])[[1]])/m$scale_divisor;stopifnot(length(x)==length(ix),all(x[is.finite(x)]>=0&x[is.finite(x)]<=1));values[[g]]<-x}
 good<-is.finite(values$FULL)&is.finite(values$ENV);count<-count+as.integer(good)
 for(g in c("FULL","ENV"))sums[good,g]<-sums[good,g]+values[[g]][good]
}
stopifnot(identical(as.integer(v$joint_finite_species[ix]),count))
maximum_sum_error<-max(abs(sums[,"FULL"]-v$FULL_sum[ix]),abs(sums[,"ENV"]-v$ENV_sum[ix]));stopifnot(maximum_sum_error<1e-8)
source<-read.csv(file.path(out,"zone_source_means.csv"),stringsAsFactors=FALSE);stopifnot(all(source$support_policy=="FIXED_261"),!anyDuplicated(paste(source$city_id,source$zone,source$group)))
gates<-read.csv(file.path(out,"city_geometry_status.csv"),stringsAsFactors=FALSE);ids<-gates$city_id[gates$status=="EXTRACTED"];stopifnot(setequal(source$city_id,ids),nrow(source)==6L*length(ids))
column<-rast(nrows=nrow(template),ncols=1,xmin=xmin(template),xmax=xmin(template)+res(template)[1],ymin=ymin(template),ymax=ymax(template),crs=crs(template));area<-as.numeric(values(cellSize(column,unit="km",mask=FALSE)))
reconstructed<-0L;union<-numeric();max_mean_error<-0
for(file in list.files(file.path(cfg$baseline_root,"cells"),pattern="[.]csv$",full.names=TRUE)){
 e<-read.csv(file,stringsAsFactors=FALSE);if(!unique(e$city_id)%in%ids)next
 stopifnot(all(is.na(e$fraction)|(e$fraction>=0&e$fraction<=1)),max(tapply(e$fraction,e$cell,sum,na.rm=TRUE))<=1+1e-8)
 stopifnot(max(abs(e$area_km2-area[rowFromCell(template,e$cell)]*e$fraction),na.rm=TRUE)<1e-8)
 union<-c(union,e$cell);w<-v[match(e$cell,v$cell),];stopifnot(!anyNA(w$cell));good<-w$joint_finite_species==261L&is.finite(e$area_km2)&e$area_km2>0
 for(zone in c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY"))for(g in c("FULL","ENV")){
 take<-good&e$zone==zone;den<-sum(e$area_km2[take]);expected<-if(den>0)sum(e$area_km2[take]*w[[g]][take])/den else NA_real_;q<-source[source$city_id==unique(e$city_id)&source$zone==zone&source$group==g,]
 stopifnot(nrow(q)==1L,abs(q$common_area_km2-den)<1e-8,abs(q$native_cell_equivalents-sum(e$fraction[take]))<1e-8,q$finite_native_cells==length(unique(e$cell[take])),identical(is.na(expected),is.na(q$mean_suitability)))
 if(is.finite(expected)){err<-abs(expected-q$mean_suitability);stopifnot(err<1e-10,q$minimum_contributing_species==261,q$maximum_contributing_species==261);max_mean_error<-max(max_mean_error,err)}
 reconstructed<-reconstructed+1L
 }}
stopifnot(setequal(union,v$cell))
x<-read.csv(file.path(out,"common_baseline_city_source.csv"),stringsAsFactors=FALSE)
for(i in seq_len(nrow(x))){q<-source[source$city_id==x$city_id[i]&source$group==x$group[i]&source$zone==x$zone[i],];stopifnot(nrow(q)==1L,abs(q$mean_suitability-x$mean_suitability[i])<1e-10,q$native_cell_equivalents>=x$minimum_cell_equivalents[i])}
for(minimum in unique(x$minimum_cell_equivalents)){
 z<-x[x$minimum_cell_equivalents==minimum,];sets<-split(z$city_id,paste(z$group,z$zone));stopifnot(length(sets)==6L,all(vapply(sets,function(y)setequal(y,sets[[1]]),logical(1))))
 for(g in c("FULL","ENV")){y<-z[z$group==g,];ring<-y[y$zone=="OUTSIDE_RING",];stopifnot(max(abs(y$ring_100_index-100*y$mean_suitability/ring$mean_suitability[match(y$city_id,ring$city_id)]))<1e-8)}
}
# Reconstruct paired bootstrap effects using independently assembled city vectors.
effect<-read.csv(file.path(out,"zone_effects.csv"),stringsAsFactors=FALSE)
set.seed(cfg$seed)
for(minimum in c(1,5)){
 z<-x[x$minimum_cell_equivalents==minimum,];ids<-sort(unique(z$city_id));n<-length(ids)
 candidates<-unique(source$city_id)
 eligible<-vapply(candidates,function(city){q<-source[source$city_id==city,]
  nrow(q)==6L&&all(is.finite(q$mean_suitability))&&all(q$common_area_km2>0)&&
   all(q$native_cell_equivalents>=minimum)&&all(q$mean_suitability[q$zone=="OUTSIDE_RING"]>0)},logical(1))
 stopifnot(setequal(ids,candidates[eligible]))
 if(n<2L)next
 resamples<-replicate(cfg$bootstrap_replicates,sample.int(n,n,replace=TRUE))
 for(g in c("FULL","ENV")){
  raw<-source[source$group==g,]
  zone_vector<-function(zone,field){a<-raw[raw$zone==zone,];a[[field]][match(ids,a$city_id)]}
  ring<-zone_vector("OUTSIDE_RING","mean_suitability");non<-zone_vector("NON_RECLAIMED_CITY","mean_suitability");rec<-zone_vector("RECLAIMED_CITY","mean_suitability")
  definitions<-list(NON_RECLAIMED_MINUS_RING=list(non,ring),RECLAIMED_MINUS_RING=list(rec,ring),RECLAIMED_MINUS_NON_RECLAIMED=list(rec,non))
  if(isTRUE(cfg$include_city_overall)){
   an<-zone_vector("NON_RECLAIMED_CITY","common_area_km2");ar<-zone_vector("RECLAIMED_CITY","common_area_km2")
   definitions$CITY_MINUS_RING<-list((non*an+rec*ar)/(an+ar),ring)
  }
  for(label in names(definitions)){
   pair<-definitions[[label]];delta<-pair[[1]]-pair[[2]]
   boot<-apply(resamples,2,function(ii)mean(delta[ii]));ci<-quantile(boot,c(.025,.975),names=FALSE)
   index_delta<-100*delta/ring;iboot<-apply(resamples,2,function(ii)mean(index_delta[ii]));ic<-quantile(iboot,c(.025,.975),names=FALSE)
   row<-effect[effect$group==g&effect$minimum_cell_equivalents==minimum&effect$contrast==label,]
   stopifnot(nrow(row)==1L,row$n_cities==n,abs(row$mean_difference-mean(delta))<1e-10,
    max(abs(c(row$CI_low,row$CI_high)-ci))<1e-10,max(abs(c(row$ring_index_CI_low,row$ring_index_CI_high)-ic))<1e-8,
    abs(row$mean_ring_100_index_difference-mean(index_delta))<1e-8,
    abs(row$target_mean-mean(pair[[1]]))<1e-10,abs(row$reference_mean-mean(pair[[2]]))<1e-10)
  }
 }
}
write_json(list(status="PASS",frozen_species_n=261,native_union_cells=nrow(v),sampled_cells=length(ix),sampled_species=261,source_maps=522,maximum_sample_sum_error=maximum_sum_error,zone_response_checks=reconstructed,maximum_mean_error=max_mean_error,
 checks="Source hashes, random32 cells times all261 paired species, complete fixed-community mask, all-city weighted responses, equivalent cells, matched samples and ring100 identity",causal_validation=FALSE),a[2],auto_unbox=TRUE,pretty=TRUE)
