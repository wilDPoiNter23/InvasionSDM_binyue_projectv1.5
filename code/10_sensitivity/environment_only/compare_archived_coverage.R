# Change coverage treatment while preserving every finite historical score.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
s<-readRDS(file.path(out,'support.rds'));d<-readRDS(file.path(out,'cell_values.rds'));historical<-d$values[[cfg$historical_surface_label]]
references<-c(FULL_SELECTED='ALL_FULL_SELECTED__SCALE__AVAILABLE',FULL_CONTINUOUS='ALL_FULL_CONTINUOUS__SCALE__AVAILABLE',PAIRED='PAIRED_FULL__SCALE_JOINT_AVAILABLE')
variants<-list(AS_STORED=historical);masks<-list(AS_STORED=rep(TRUE,length(historical)))
for(label in names(references)){mask<-d$counts[[references[[label]]]]>0;v<-historical;v[!mask]<-NA_real_
 stopifnot(identical(v[mask],historical[mask]));variants[[label]]<-v;masks[[label]]<-mask
}
w<-s$weights;groups<-split(seq_len(nrow(w)),interaction(w$footprint,w$city_id,w$zone,drop=TRUE));rows<-list();j<-0L
for(label in names(variants))for(ix in groups){z<-w[ix,];v<-variants[[label]][z$cell_index];raw<-historical[z$cell_index];removed<-!masks[[label]][z$cell_index];ok<-is.finite(v)
 j<-j+1L;rows[[j]]<-data.frame(coverage=label,footprint=z$footprint[1],city_id=z$city_id[1],zone=z$zone[1],mean=if(any(ok))sum(v[ok]*z$weight_km2[ok])/sum(z$weight_km2[ok])else NA_real_,finite_area_km2=sum(z$weight_km2[ok]),total_area_km2=sum(z$weight_km2),excluded_area_km2=sum(z$weight_km2[removed]),excluded_positive_original_area_km2=sum(z$weight_km2[removed&is.finite(raw)&raw>0]))
}
means<-do.call(rbind,rows);write.csv(means,file.path(out,'archived_coverage_city_means.csv'),row.names=FALSE)
results<-list();j<-0L;set.seed(cfg$seed+4)
for(label in names(variants))for(foot in unique(means$footprint)){
 z<-means[means$coverage==label&means$footprint==foot,];r<-z[z$zone=='RECLAIMED',];n<-z[z$zone=='OTHER_URBAN',];q<-merge(r,n,by='city_id',suffixes=c('_rec','_other'));q<-q[is.finite(q$mean_rec)&is.finite(q$mean_other),];if(!nrow(q))next
 rec<-mean(q$mean_rec);other<-mean(q$mean_other);boot<-replicate(cfg$bootstrap_replicates,{ix<-sample.int(nrow(q),nrow(q),TRUE);mean(q$mean_rec[ix]-q$mean_other[ix])})
 j<-j+1L;results[[j]]<-data.frame(coverage=label,footprint=foot,cities=nrow(q),reclaimed=rec,other_urban=other,difference=rec-other,percent=100*(rec/other-1),difference_lower=unname(quantile(boot,.025)),difference_upper=unname(quantile(boot,.975)),excluded_reclaimed_area_percent=100*sum(q$excluded_area_km2_rec)/sum(q$total_area_km2_rec),excluded_other_area_percent=100*sum(q$excluded_area_km2_other)/sum(q$total_area_km2_other),excluded_positive_original_area_km2=sum(q$excluded_positive_original_area_km2_rec+q$excluded_positive_original_area_km2_other))
}
results<-do.call(rbind,results);write.csv(results,file.path(out,'archived_coverage_contrasts.csv'),row.names=FALSE)
# Independently rebuild a native-cell sample of every coverage variant.
set.seed(cfg$seed+5);ix<-sample.int(length(historical),min(50L,length(historical)));sample_checks<-0L
for(label in names(references)){expected<-historical[ix];count<-d$counts[[references[[label]]]][ix];expected[count==0]<-NA_real_
 stopifnot(identical(expected,variants[[label]][ix]));sample_checks<-sample_checks+length(ix)
}
main<-read.csv(file.path(out,'contrasts.csv'));main<-main[main$variant==cfg$historical_surface_label&main$weighting=='EQUAL_CITY',]
stored<-results[results$coverage=='AS_STORED',];ix<-match(stored$footprint,main$footprint);stopifnot(max(abs(stored$percent-main$percent[ix]))<1e-9)
write_json(list(status='PASS',sampled_coverage_values=sample_checks,historical_values_unchanged_on_retained_cells=TRUE,historical_values_sha256=digest(historical,algo='sha256'),reference_membership=as.list(references),interpretation='Coverage-only diagnostic using saved complete FULL or paired prediction support; no normalization, species score substitution, new model fit or fixed city list'),file.path(out,'archived_coverage_verification.json'),auto_unbox=TRUE,pretty=TRUE)
