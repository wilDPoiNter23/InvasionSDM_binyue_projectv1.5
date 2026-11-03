# Independently check sampled source values, paired reconstruction and zonal means.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'support.rds'));d<-readRDS(file.path(out,'cell_values.rds'));means<-read.csv(file.path(out,'city_zone_means.csv'))
rules<-read.csv(file.path(out,'processing_rules.csv'));metadata<-read.csv(file.path(out,'raster_input_metadata.csv'))
stopifnot(identical(s$cells,d$cells),all(s$weights$weight_km2>0),!anyDuplicated(rules$variant))
set.seed(cfg$seed+1);ix<-sort(sample.int(length(s$cells),min(50L,length(s$cells))))
sample_paths<-unique(c(head(metadata$path,3),tail(metadata$path,3)))
maximum_source_error<-0
for(path in sample_paths){cache<-readRDS(file.path(out,'input_cells',paste0(digest(path,algo='sha256'),'.rds')))
 r<-rast(path);xy<-xyFromCell(r,s$cells[ix]);z<-extract(r,xy);v<-as.numeric(z[[ncol(z)]])
 stopifnot(identical(is.finite(v),is.finite(cache$value[ix])))
 if(any(is.finite(v)))maximum_source_error<-max(maximum_source_error,max(abs(v-cache$value[ix]),na.rm=TRUE))
};stopifnot(maximum_source_error<1e-9)
pair_errors<-list()
required_paired_surfaces<-paste0('STORED_PAIRED_',c('FULL','ENV'))
if(!all(required_paired_surfaces%in%names(d$values)))stop('Paired verification requires STORED_PAIRED_FULL and STORED_PAIRED_ENV in configured surfaces')
for(g in c('FULL','ENV')){
 rebuilt<-d$values[[paste0('PAIRED_',g,'__SCALE_JOINT_AVAILABLE')]];stored<-d$values[[paste0('STORED_PAIRED_',g)]]
 stopifnot(identical(is.finite(rebuilt),is.finite(stored)));delta<-max(abs(rebuilt-stored),na.rm=TRUE);stopifnot(delta<1e-7);pair_errors[[g]]<-delta
}
maximum_mean_error<-0
take<-sort(sample.int(nrow(means),min(200L,nrow(means))))
for(i in take){z<-means[i,];w<-s$weights[s$weights$footprint==z$footprint&s$weights$city_id==z$city_id&s$weights$zone==z$zone,];v<-d$values[[z$variant]][match(w$cell,s$cells)];ok<-is.finite(v)
 expected<-if(any(ok))sum(w$weight_km2[ok]*v[ok])/sum(w$weight_km2[ok])else NA_real_
 stopifnot(identical(is.finite(expected),is.finite(z$mean)))
 if(is.finite(expected))maximum_mean_error<-max(maximum_mean_error,abs(expected-z$mean))
};stopifnot(maximum_mean_error<1e-10)
geometry<-read.csv(file.path(out,'city_geometry_status.csv'));stopifnot(!any(geometry$status=='GEOMETRY_ERROR'))
c<-read.csv(file.path(out,'contrasts.csv'));maximum_contrast_error<-0
for(i in seq_len(nrow(c))){z<-c[i,];m<-means[means$variant==z$variant&means$footprint==z$footprint,];r<-m[m$zone=='RECLAIMED',];n<-m[m$zone=='OTHER_URBAN',];q<-merge(r,n,by='city_id');q<-q[is.finite(q$mean.x)&is.finite(q$mean.y)&q$finite_area_km2.x>0&q$finite_area_km2.y>0,];stopifnot(nrow(q)==z$cities)
 if(z$weighting=='EQUAL_CITY')expected<-mean(q$mean.x)-mean(q$mean.y)else expected<-sum(q$mean.x*q$finite_area_km2.x)/sum(q$finite_area_km2.x)-sum(q$mean.y*q$finite_area_km2.y)/sum(q$finite_area_km2.y)
 maximum_contrast_error<-max(maximum_contrast_error,abs(expected-z$difference))
};stopifnot(maximum_contrast_error<1e-10)
write_json(list(status='PASS_NUMERICAL_COMPARISON',sampled_source_maps=length(sample_paths),sampled_cells=length(ix),maximum_source_error=maximum_source_error,paired_surface_reconstruction_maximum_errors=pair_errors,sampled_zonal_means=length(take),maximum_mean_error=maximum_mean_error,maximum_contrast_error=maximum_contrast_error,geometry_status=as.list(table(geometry$status)),historical_lineage='See historical_surface_concordance.csv; numerical verification does not establish an unbound historical writer'),file.path(out,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS numerical comparison\n')
