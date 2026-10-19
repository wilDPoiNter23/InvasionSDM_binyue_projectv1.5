# Apply existing thresholds to saved scaled values before native-excluded aggregation.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
v<-readRDS(cfg$scaled_values);m<-readRDS(cfg$native_membership);s<-readRDS(cfg$city_support)
stopifnot(identical(m$cells,s$cells));pm<-match(v$paired_species,m$species)
t<-read.csv(cfg$paired_thresholds);main<-read.csv(cfg$main_thresholds);scales<-read.csv(cfg$input_scales)
stopifnot(!anyDuplicated(paste(t$species,t$group)),!anyDuplicated(main$species))
w<-s$weights;w<-w[w$footprint=='HISTORICAL'&!tolower(w$city_id)%in%tolower(cfg$exclude_city_keys),]
city<-w;city$region<-'CITY'
recl<-w[w$zone=='RECLAIMED',];recl$region<-'MAPPED_RECLAMATION'
ww<-rbind(city,recl);ww$id<-paste(ww$city_id,ww$region,sep='|')
regions<-unique(ww[,c('id','city_id','region')]);parts<-split(seq_len(nrow(ww)),ww$id)
groups<-list(FULL_814=v$full,FULL=v$paired_full,ENV=v$paired_env)
responses<-list();species_rows<-list();cells<-list();qc<-list()
for(g in names(groups)){
 x<-groups[[g]];sp<-if(g=='FULL_814')m$species else v$paired_species
 native<-if(g=='FULL_814')m$native else m$native[,pm,drop=FALSE]
 tt<-if(g=='FULL_814')main else t[t$group==g,]
 tt<-tt[match(sp,tt$species),];input_group<-switch(g,FULL_814='FULL_814',FULL='PAIRED_FULL',ENV='PAIRED_ENV')
 ss<-scales[scales$group==input_group,];ss<-ss[match(sp,ss$species),]
 ok_threshold<-!is.na(tt$species)&is.finite(tt$t10_01)&is.finite(tt$max_tss_01)
 stopifnot(all(tt$projection_path[ok_threshold]==ss$path[ok_threshold]),all(tt$scale_divisor[ok_threshold]==ss$divisor[ok_threshold]))
 if(any(!ok_threshold))stopifnot(all(colSums(is.finite(x[,!ok_threshold,drop=FALSE]))==0))
 valid<-is.finite(x)&!native
 qc[[g]]<-data.frame(group=g,pool=length(sp),native_excluded=sum(native),genuine_missing=sum(!is.finite(x)&!native),thresholds_available=sum(ok_threshold))
 for(method in c('t10','max_tss')){
  cutoff<-tt[[paste0(method,'_01')]];binary<-sweep(x,2,cutoff,`>=`);binary[!valid]<-NA
  observed<-rowSums(is.finite(binary));score<-rowMeans(binary,na.rm=TRUE);score[!is.finite(score)]<-NA_real_
  cells[[paste(g,method,sep='_')]]<-list(mean=score,observed_species=observed)
  for(j in seq_len(nrow(regions))){
   ri<-parts[[regions$id[j]]];ci<-ww$cell_index[ri];area<-ww$weight_km2[ri]
   fine<-is.finite(score[ci]);den<-sum(area[fine]);effective_area<-if(den>0)sum(score[ci[fine]]*area[fine])else NA_real_
   suitable<-colSums(ifelse(is.finite(binary[ci,,drop=FALSE])&binary[ci,,drop=FALSE],area,0))
   finite<-colSums(ifelse(is.finite(binary[ci,,drop=FALSE]),area,0))
   eligible<-finite>0
   species_rows[[length(species_rows)+1L]]<-data.frame(city_id=regions$city_id[j],region=regions$region[j],group=g,method=method,species=sp,cutoff_01=cutoff,finite_non_native_area_km2=finite,suitable_area_km2=ifelse(eligible,suitable,NA_real_),suitable_fraction=ifelse(eligible,suitable/finite,NA_real_))
   responses[[length(responses)+1L]]<-data.frame(city_id=regions$city_id[j],region=regions$region[j],group=if(g=='FULL_814')'FULL'else g,source_group=g,threshold=if(method=='t10')'T10'else'MAX_TSS',frozen_species_n=length(sp),finite_species_n=sum(eligible),finite_area_km2=den,mean_species_suitable_fraction=if(den>0)effective_area/den else NA_real_,mean_species_suitable_area_km2=effective_area,suitable_species_area_km2=sum(suitable[eligible]),status=if(den>0)paste0('AVAILABLE_NON_NATIVE_',length(sp))else'NO_FINITE_NON_NATIVE_PREDICTIONS')
  }
 }
}
d<-do.call(rbind,responses)
write.csv(d[d$source_group!='FULL_814',],file.path(out,'paired_city_responses.csv'),row.names=FALSE)
write.csv(d[d$source_group=='FULL_814',],file.path(out,'full814_city_responses.csv'),row.names=FALSE)
write.csv(do.call(rbind,species_rows),file.path(out,'city_species_threshold_area.csv'),row.names=FALSE)
write.csv(do.call(rbind,qc),file.path(out,'mask_threshold_coverage.csv'),row.names=FALSE)
saveRDS(list(cells=s$cells,products=cells),file.path(out,'binary_cell_means.rds'))
write_json(list(status='EXISTING_THRESHOLDS_NATIVE_EXCLUDED',boundary='score >= cutoff',thresholds_reestimated=FALSE,aggregation='Mean across finite non-native species within each cell, then area weighted. Effective suitable area integrates this mean; fraction divides by finite spatial area. Species-area sum is recorded separately and is not union area.',native_and_missing='Native excluded; genuine missing unknown; no fixed denominator zero filling',threshold_sha256=vapply(c(cfg$paired_thresholds,cfg$main_thresholds),function(f)digest(file=f,algo='sha256'),character(1))),file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
