# Check mask semantics, source scales, missingness accounting and reported contrasts.
suppressPackageStartupMessages({library(terra);library(sf);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'analysis_support.rds'))
m<-readRDS(file.path(out,'native_cell_membership.rds'))
d<-readRDS(file.path(out,'non_native_cell_values.rds'))
v<-readRDS(file.path(out,'scaled_species_values.rds'))
meta<-read.csv(file.path(out,'continuous_input_scales.csv'))
stopifnot(identical(s$cells,m$cells),identical(s$cells,d$cells),all(meta$divisor==1000),all(meta$output_scale=='0-1'),!any(grepl('TSSbin|TSSfilt',meta$path)))
source_error<-0;set.seed(cfg$seed+1);source_checks<-0L;source_finite_checks<-0L
if(!is.null(cfg$reference_source_values)){
  reference<-read.csv(cfg$reference_source_values)
  reference<-reference[reference$cell_index%in%s$source_indices,];reference$cell_index<-match(reference$cell_index,s$source_indices)
  if(!nrow(reference))stop('No independent source reference rows remain on the selected support; provide overlapping references.')
  for(i in seq_len(nrow(reference))){z<-reference[i,];actual<-switch(z$group,FULL_814=v$full[z$cell_index,match(z$species,m$species)],PAIRED_FULL=v$paired_full[z$cell_index,match(z$species,v$paired_species)],PAIRED_ENV=v$paired_env[z$cell_index,match(z$species,v$paired_species)])
    stopifnot(length(actual)==1L,is.finite(actual)==is.finite(z$value));if(is.finite(actual)){source_error<-max(source_error,abs(actual-z$value));source_finite_checks<-source_finite_checks+1L}
  };source_checks<-nrow(reference)
}else for(i in unique(round(seq(1,nrow(meta),length.out=9)))){
  r<-rast(meta$path[i]);ix<-sample.int(length(s$cells),min(80L,length(s$cells)))
  z<-extract(r,s$xy[ix,,drop=FALSE]);x<-as.numeric(z[[ncol(z)]])/1000
  ref<-switch(meta$group[i],FULL_814=v$full[ix,match(meta$species[i],m$species)],PAIRED_FULL=v$paired_full[ix,match(meta$species[i],v$paired_species)],PAIRED_ENV=v$paired_env[ix,match(meta$species[i],v$paired_species)])
  stopifnot(identical(is.finite(x),is.finite(ref)))
  if(any(is.finite(x)))source_error<-max(source_error,max(abs(x-ref),na.rm=TRUE))
  source_checks<-source_checks+length(ix)
  source_finite_checks<-source_finite_checks+sum(is.finite(x))
}
if(source_checks<1L||source_finite_checks<1L)stop('Independent source crosschecks require at least one finite source value on the selected support.')
stopifnot(source_error<1e-12)
for(g in names(d$products)){
  q<-d$products[[g]];c<-q$counts
  stopifnot(all(c$native+c$eligible==c$pool),all(c$observed+c$missing==c$eligible),all(c$known+c$missing==c$pool),all(c$missing>=0))
  stopifnot(all(q$values$fixed_upper>=q$values$fixed_lower),max(abs(q$values$fixed_upper-q$values$fixed_lower-c$missing/c$pool))<1e-12)
  stopifnot(identical(is.finite(q$values$strict_fixed),c$missing==0))
}
stopifnot(identical(d$products$PAIRED_FULL$counts,d$products$PAIRED_ENV$counts))
for(i in sample.int(length(s$cells),min(120L,length(s$cells)))){
  native<-m$native[i,];known<-is.finite(v$full[i,])&!native
  expected<-sum(v$full[i,known])/(sum(native)+sum(known))
  actual<-d$products$FULL_814$values$coverage_adjusted[i]
  stopifnot(isTRUE(all.equal(expected,actual,tolerance=1e-12)))
}
# Compare the geometric membership with existing masked projections on finite source cells.
mask_checks<-list();k<-0L
if(!is.null(cfg$reference_mask_values)){
  reference<-read.csv(cfg$reference_mask_values)
  reference<-reference[reference$cell_index%in%s$source_indices,];reference$cell_index<-match(reference$cell_index,s$source_indices)
  if(!nrow(reference))stop('No independent native-mask reference rows remain on the selected support; provide overlapping references.')
  for(tx in unique(reference$species)){
    z<-reference[reference$species==tx,];j<-match(tx,m$species);stopifnot(!is.na(j));ok<-is.finite(z$source_value)
    actual<-m$native[z$cell_index[ok],j];expected<-!is.finite(z$masked_value[ok])
    k<-k+1L;mask_checks[[k]]<-data.frame(species=tx,finite_source_cells=sum(ok),native_cells=sum(actual),mismatches=sum(actual!=expected))
  }
}else{
old<-read.csv(cfg$native_projection_manifest)
candidate<-unique(round(seq(1,nrow(old),length.out=9)))
for(i in candidate){
  tx<-old$accepted_taxon[i];j<-match(tx,m$species);if(is.na(j))next
  raw<-rast(old$source_global_projection[i]);masked<-rast(old$output_non_native_projection[i])
  x<-extract(raw,s$xy);y<-extract(masked,s$xy);x<-as.numeric(x[[ncol(x)]]);y<-as.numeric(y[[ncol(y)]])
  ok<-is.finite(x);expected_native<-!is.finite(y[ok]);actual_native<-m$native[ok,j]
  k<-k+1L;mask_checks[[k]]<-data.frame(species=tx,finite_source_cells=sum(ok),native_cells=sum(actual_native),mismatches=sum(expected_native!=actual_native))
}
}
mask_checks<-do.call(rbind,mask_checks)
if(is.null(mask_checks)||!nrow(mask_checks)||sum(mask_checks$finite_source_cells)<1L)
 stop('No independent native-mask crosschecks have finite source cells on the selected support.')
write.csv(mask_checks,file.path(out,'native_mask_crosscheck.csv'),row.names=FALSE)
stopifnot(all(mask_checks$mismatches==0))
means<-read.csv(file.path(out,'non_native_city_zone_means.csv'));cc<-read.csv(file.path(out,'non_native_contrasts.csv'))
mean_error<-0
for(i in sample.int(nrow(means),min(160,nrow(means)))){
  z<-means[i,];w<-s$weights[s$weights$city_id==z$city_id&s$weights$footprint==z$footprint&s$weights$zone==z$zone,]
  x<-d$products[[z$group]]$values[[z$rule]][w$cell_index];ok<-is.finite(x)
  expected<-if(any(ok))sum(x[ok]*w$weight_km2[ok])/sum(w$weight_km2[ok])else NA_real_
  stopifnot(identical(is.finite(expected),is.finite(z$mean)))
  if(is.finite(expected))mean_error<-max(mean_error,abs(expected-z$mean))
}
contrast_error<-0
for(i in seq_len(nrow(cc))){
  z<-cc[i,];q<-means[means$group==z$group&means$rule==z$rule&means$footprint==z$footprint,]
  r<-q[q$zone=='RECLAIMED',];o<-q[q$zone=='OTHER_URBAN',];t<-merge(r,o,by='city_id',suffixes=c('_rec','_other'))
  t<-t[is.finite(t$mean_rec)&is.finite(t$mean_other)&t$finite_area_km2_rec>0&t$finite_area_km2_other>0,]
  stopifnot(nrow(t)==z$cities)
  p<-if(z$weighting=='EQUAL_CITY')c(mean(t$mean_rec),mean(t$mean_other))else c(weighted.mean(t$mean_rec,t$finite_area_km2_rec),weighted.mean(t$mean_other,t$finite_area_km2_other))
  contrast_error<-max(contrast_error,abs(z$percent-100*(p[1]/p[2]-1)),abs(z$difference-(p[1]-p[2])))
}
stopifnot(mean_error<1e-12,contrast_error<1e-9)
legacy_check<-NULL
if('SEPTEMBER_CONTINUOUS_814'%in%names(cfg$stored_surfaces)){
  old<-extract(rast(cfg$stored_surfaces$SEPTEMBER_CONTINUOUS_814),s$xy);old<-as.numeric(old[[ncol(old)]])
  first_ok<-is.finite(v$full[,1])&!m$native[,1]
  restored<-!is.finite(old)&is.finite(d$products$FULL_814$values$alien_mean)
  legacy_check<-list(first_species=m$species[1],finite_coverage_mismatches_to_first_species=sum(is.finite(old)!=first_ok),restored_cells=sum(restored),restored_first_native=sum(restored&m$native[,1]),restored_first_missing_outside_native=sum(restored&!m$native[,1]&!is.finite(v$full[,1])))
  stopifnot(legacy_check$finite_coverage_mismatches_to_first_species==0)
}
write_json(list(status='PASS',source_value_checks=source_checks,source_finite_value_checks=source_finite_checks,source_check_mode=if(is.null(cfg$reference_source_values))'Direct raster read'else'Independent raster samples exported at source',maximum_source_error=source_error,mask_species_checked=nrow(mask_checks),mask_finite_cells_checked=sum(mask_checks$finite_source_cells),mask_mismatches=sum(mask_checks$mismatches),maximum_zonal_mean_error=mean_error,maximum_contrast_error=contrast_error,full_species=ncol(v$full),paired_species=ncol(v$paired_full),native_grid_cells=length(s$cells),legacy_first_species_check=legacy_check),file.path(out,'non_native_verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS non-native processing checks\n')
