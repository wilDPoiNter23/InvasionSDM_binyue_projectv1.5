# Separate all-missing cell treatment from partial-species denominators.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'support.rds'));d<-readRDS(file.path(out,'cell_values.rds'));rules<-read.csv(file.path(out,'processing_rules.csv'))
weights<-s$weights;groups<-split(seq_len(nrow(weights)),interaction(weights$footprint,weights$city_id,weights$zone,drop=TRUE))
source<-list();k<-0L
for(label in rules$variant[rules$denominator=='ZERO_FIXED']){
 n<-d$counts[[label]];fixed<-d$values[[label]];available_label<-sub('ZERO_FIXED$','AVAILABLE',label);available<-d$values[[available_label]]
 modeled<-fixed;modeled[n==0]<-NA_real_
 variants<-list(ZERO_FIXED=fixed,ZERO_FIXED_MODELED_SUPPORT=modeled,AVAILABLE=available)
 for(stage in names(variants))for(ix in groups){w<-weights[ix,];v<-variants[[stage]][w$cell_index];count<-n[w$cell_index];ok<-is.finite(v)
  k<-k+1L;source[[k]]<-data.frame(base_variant=label,stage=stage,footprint=w$footprint[1],city_id=w$city_id[1],zone=w$zone[1],mean=if(any(ok))sum(v[ok]*w$weight_km2[ok])/sum(w$weight_km2[ok])else NA_real_,finite_area_km2=sum(w$weight_km2[ok]),total_area_km2=sum(w$weight_km2),no_prediction_area_km2=sum(w$weight_km2[count==0]),mean_count_all_area=sum(count*w$weight_km2)/sum(w$weight_km2))
 }
}
z<-do.call(rbind,source);write.csv(z,file.path(out,'missing_support_city_means.csv'),row.names=FALSE)
results<-list();k<-0L;set.seed(cfg$seed+2)
for(label in unique(z$base_variant))for(foot in unique(z$footprint)){
 q<-z[z$base_variant==label&z$footprint==foot,];av<-q[q$stage=='AVAILABLE',]
 avrec<-av$city_id[av$zone=='RECLAIMED'&is.finite(av$mean)&av$finite_area_km2>0]
 avother<-av$city_id[av$zone=='OTHER_URBAN'&is.finite(av$mean)&av$finite_area_km2>0];computable<-intersect(avrec,avother)
 for(stage in unique(q$stage))for(scope in c('EACH_RECIPE','SAME_COMPUTABLE_CITIES')){
  t<-q[q$stage==stage,];if(scope=='SAME_COMPUTABLE_CITIES')t<-t[t$city_id%in%computable,]
  r<-t[t$zone=='RECLAIMED',];n<-t[t$zone=='OTHER_URBAN',];m<-merge(r,n,by='city_id',suffixes=c('_rec','_other'));m<-m[is.finite(m$mean_rec)&is.finite(m$mean_other)&m$finite_area_km2_rec>0&m$finite_area_km2_other>0,]
  if(!nrow(m))next
  rec<-mean(m$mean_rec);other<-mean(m$mean_other)
  boot<-replicate(cfg$bootstrap_replicates,{ix<-sample.int(nrow(m),nrow(m),TRUE);mean(m$mean_rec[ix]-m$mean_other[ix])})
  k<-k+1L;results[[k]]<-data.frame(base_variant=label,footprint=foot,stage=stage,city_scope=scope,cities=nrow(m),reclaimed=rec,other_urban=other,difference=rec-other,percent=if(other>0)100*(rec/other-1)else NA_real_,difference_lower=unname(quantile(boot,.025)),difference_upper=unname(quantile(boot,.975)),reclaimed_no_prediction_area_percent=100*sum(m$no_prediction_area_km2_rec)/sum(m$total_area_km2_rec),other_no_prediction_area_percent=100*sum(m$no_prediction_area_km2_other)/sum(m$total_area_km2_other))
 }
}
write.csv(do.call(rbind,results),file.path(out,'missing_support_decomposition.csv'),row.names=FALSE)
write_json(list(status='COMPUTED',cell_values_sha256=digest(file=file.path(out,'cell_values.rds'),algo='sha256'),steps=c('Fixed denominator with all missing cells treated as zero','Same fixed denominator with all-missing cells retained as missing','Available-species denominator on modeled cells'),city_scope='Each recipe is reported on all computable cities; a separate same-computable-city comparison isolates cell processing without freezing a historical city list'),file.path(out,'missing_support_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
