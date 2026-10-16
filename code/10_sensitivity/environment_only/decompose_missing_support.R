# Separate all-missing cell treatment from partial-species denominators.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'support.rds'));d<-readRDS(file.path(out,'cell_values.rds'));rules<-read.csv(file.path(out,'processing_rules.csv'))
inputs<-read.csv(file.path(out,'species_inputs.csv'));metadata<-read.csv(file.path(out,'raster_input_metadata.csv'));paired<-read.csv(cfg$paired_manifest)
full<-inputs[inputs$set=='ALL_FULL_CONTINUOUS',c('species','path','exists')]
stopifnot(!anyDuplicated(full$species),all(full$exists),all(paired$species%in%full$species))
roster<-merge(full,paired[,c('species','full_projection')],by='species',all.x=TRUE)
roster$aligned_path<-ifelse(is.na(roster$full_projection),roster$path,roster$full_projection)
roster$source_replaced<-roster$path!=roster$aligned_path
write.csv(roster,file.path(out,'full_source_alignment.csv'),row.names=FALSE)
extra<-list(BASELINE_FULL_PAIRED_ROSTER=full[match(paired$species,full$species),c('species','path')],FULL_ALL_PAIRED_ROUTES=data.frame(species=roster$species,path=roster$aligned_path))
aligned<-list()
for(set in names(extra))for(normalization in c('SCALE','MINMAX')){
 q<-extra[[set]];total<-numeric(length(s$cells));count<-integer(length(s$cells))
 for(j in seq_len(nrow(q))){path<-q$path[j];cache<-readRDS(file.path(out,'input_cells',paste0(digest(path,algo='sha256'),'.rds')));v<-cache$value;mm<-cache$metadata;ok<-is.finite(v)
  if(normalization=='SCALE')v<-v/1000 else v<-if(mm$maximum==mm$minimum)v*0 else (v-mm$minimum)/(mm$maximum-mm$minimum)
  total[ok]<-total[ok]+v[ok];count[ok]<-count[ok]+1L
 }
 base<-paste(set,normalization,sep='__');fixed_label<-paste0(base,'__ZERO_FIXED');av_label<-paste0(base,'__AVAILABLE')
 d$values[[fixed_label]]<-total/nrow(q);d$values[[av_label]]<-ifelse(count>0,total/count,NA_real_);d$counts[[fixed_label]]<-count;d$counts[[av_label]]<-count
 rules<-rbind(rules,data.frame(variant=fixed_label,species_set=set,normalization=normalization,denominator='ZERO_FIXED',requested_species=nrow(q),available_species=nrow(q),complete_input_set=TRUE,native_range_mask='No additional native-range mask introduced'))
 aligned[[base]]<-list(fixed=total/nrow(q),available=d$values[[av_label]],count=count,species=nrow(q))
}
saveRDS(list(cells=s$cells,products=aligned),file.path(out,'aligned_full_cell_values.rds'))
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
write_json(list(status='COMPUTED',cell_values_sha256=digest(file=file.path(out,'cell_values.rds'),algo='sha256'),aligned_values_sha256=digest(file=file.path(out,'aligned_full_cell_values.rds'),algo='sha256'),full_species=nrow(full),paired_species=nrow(paired),full_sources_replaced=sum(roster$source_replaced),steps=c('Fixed denominator with all missing cells treated as zero','Same fixed denominator with all-missing cells retained as missing','Available-species denominator on modeled cells'),city_scope='Each recipe is reported on all computable cities; a separate same-computable-city comparison isolates cell processing without freezing a historical city list'),file.path(out,'missing_support_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
