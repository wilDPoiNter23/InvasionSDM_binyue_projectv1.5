# Reconstruct aligned species means and missing-support contrasts independently.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'support.rds'));d<-readRDS(file.path(out,'cell_values.rds'));aligned<-readRDS(file.path(out,'aligned_full_cell_values.rds'))
roster<-read.csv(file.path(out,'full_source_alignment.csv'));paired<-read.csv(cfg$paired_manifest)
sets<-list(FULL_ALL_PAIRED_ROUTES=data.frame(species=roster$species,path=roster$aligned_path),BASELINE_FULL_PAIRED_ROSTER=roster[match(paired$species,roster$species),c('species','path')])
set.seed(cfg$seed+3);ix<-sort(sample.int(length(s$cells),min(50L,length(s$cells))));max_sum_error<-0
for(set in names(sets)){
 q<-sets[[set]];raw<-matrix(NA_real_,length(ix),nrow(q));normalized<-raw
 for(j in seq_len(nrow(q))){cache<-readRDS(file.path(out,'input_cells',paste0(digest(q$path[j],algo='sha256'),'.rds')));raw[,j]<-cache$value[ix];mm<-cache$metadata;normalized[,j]<-if(mm$maximum==mm$minimum)raw[,j]*0 else(raw[,j]-mm$minimum)/(mm$maximum-mm$minimum)}
 for(norm in c('SCALE','MINMAX')){
  value<-if(norm=='SCALE')raw/1000 else normalized;p<-aligned$products[[paste(set,norm,sep='__')]]
  count<-rowSums(is.finite(value));fixed<-rowSums(replace(value,!is.finite(value),0))/ncol(value);available<-rowMeans(value,na.rm=TRUE)
  stopifnot(identical(as.integer(count),as.integer(p$count[ix])),identical(is.finite(available),is.finite(p$available[ix])))
  max_sum_error<-max(max_sum_error,max(abs(fixed-p$fixed[ix])),max(abs(available-p$available[ix]),na.rm=TRUE))
 }
};stopifnot(max_sum_error<1e-10)
means<-read.csv(file.path(out,'missing_support_city_means.csv'));summaries<-read.csv(file.path(out,'missing_support_decomposition.csv'))
get_product<-function(label){base<-sub('__ZERO_FIXED$','',label);if(base%in%names(aligned$products))aligned$products[[base]]else list(fixed=d$values[[label]],available=d$values[[sub('ZERO_FIXED$','AVAILABLE',label)]],count=d$counts[[label]])}
max_mean_error<-0
for(i in sample.int(nrow(means),min(200L,nrow(means)))){
 z<-means[i,];p<-get_product(z$base_variant);v<-if(z$stage=='AVAILABLE')p$available else p$fixed;if(z$stage=='ZERO_FIXED_MODELED_SUPPORT')v[p$count==0]<-NA_real_
 w<-s$weights[s$weights$city_id==z$city_id&s$weights$footprint==z$footprint&s$weights$zone==z$zone,];v<-v[match(w$cell,s$cells)];ok<-is.finite(v)
 expected<-if(any(ok))weighted.mean(v[ok],w$weight_km2[ok])else NA_real_;stopifnot(identical(is.finite(expected),is.finite(z$mean)))
 if(is.finite(expected))max_mean_error<-max(max_mean_error,abs(expected-z$mean))
};stopifnot(max_mean_error<1e-10)
max_contrast_error<-0
for(i in seq_len(nrow(summaries))){z<-summaries[i,];m<-means[means$base_variant==z$base_variant&means$footprint==z$footprint,]
 if(z$city_scope=='SAME_COMPUTABLE_CITIES'){av<-m[m$stage=='AVAILABLE',];keep<-intersect(av$city_id[av$zone=='RECLAIMED'&is.finite(av$mean)],av$city_id[av$zone=='OTHER_URBAN'&is.finite(av$mean)]);m<-m[m$city_id%in%keep,]}
 m<-m[m$stage==z$stage,];q<-merge(m[m$zone=='RECLAIMED',],m[m$zone=='OTHER_URBAN',],by='city_id');q<-q[is.finite(q$mean.x)&is.finite(q$mean.y),];stopifnot(nrow(q)==z$cities)
 max_contrast_error<-max(max_contrast_error,abs(mean(q$mean.x-q$mean.y)-z$difference))
};stopifnot(max_contrast_error<1e-10)
write_json(list(status='PASS',sampled_cells=length(ix),aligned_products=length(aligned$products),maximum_species_sum_error=max_sum_error,maximum_zonal_mean_error=max_mean_error,maximum_contrast_error=max_contrast_error,source_replacements=sum(roster$source_replaced),cities_not_frozen_to_historical_list=TRUE),file.path(out,'missing_support_verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS missing-support decomposition\n')
