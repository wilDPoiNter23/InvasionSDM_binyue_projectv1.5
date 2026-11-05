# Compare three land zones within each supplied full-model surface independently.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
stopifnot(fromJSON(file.path(cfg$baseline_root,'verification.json'))$status=='PASS')
files<-list.files(file.path(cfg$baseline_root,'cells'),pattern='[.]csv$',full.names=TRUE)
gates<-read.csv(file.path(cfg$baseline_root,'city_geometry_status.csv'))
ids<-gates$city_id[gates$status=='EXTRACTED']
if(!is.null(cfg$exclude_city_keys))ids<-ids[!tolower(trimws(ids))%in%tolower(trimws(cfg$exclude_city_keys))]
zones<-c('OUTSIDE_RING','NON_RECLAIMED_CITY','RECLAIMED_CITY')
cache<-lapply(files,read.csv);cache<-Filter(function(d)unique(d$city_id)%in%ids,cache)
stopifnot(setequal(vapply(cache,function(d)unique(d$city_id),character(1)),ids))
cells<-sort(unique(unlist(lapply(cache,function(d)d$cell))));source<-list();checks<-list()
template<-rast(cfg$surfaces[[1]]);man<-list();set.seed(cfg$seed)
for(group in names(cfg$surfaces)){
 path<-cfg$surfaces[[group]];r<-rast(path);stopifnot(compareGeom(r,template,stopOnError=FALSE),nlyr(r)==1L)
 value<-as.numeric(extract(r,cells)[[1]]);stopifnot(all(value[is.finite(value)]>=0&value[is.finite(value)]<=1))
 man[[group]]<-data.frame(group=group,path=path,sha256=digest(file=path,algo='sha256'))
 for(e in cache){ix<-match(e$cell,cells);good<-is.finite(value[ix])&is.finite(e$area_km2)&e$area_km2>0
  for(zone in zones){take<-which(good&e$zone==zone);w<-e$area_km2[take];v<-value[ix[take]]
   mean<-if(length(take))weighted.mean(v,w)else NA_real_
   source[[length(source)+1L]]<-data.frame(city_id=unique(e$city_id),group=group,zone=zone,mean_suitability=mean,
    finite_area_km2=sum(w),native_cell_equivalents=sum(e$fraction[take]),native_cells=length(unique(e$cell[take])))
   if(length(take)){check<-drop(crossprod(w,v))/sum(w);stopifnot(abs(mean-check)<1e-12)}
  }
 }
 # Re-read a frozen native-cell sample separately from the full value vector.
 take<-sort(sample.int(length(cells),min(100L,length(cells))))
 reread_table<-extract(r,xyFromCell(r,cells[take]));reread<-as.numeric(reread_table[[ncol(reread_table)]])
 stopifnot(identical(is.finite(reread),is.finite(value[take])),max(abs(reread-value[take]),na.rm=TRUE)<1e-12)
 checks[[group]]<-list(sampled_cells=length(take),source_sha256=man[[group]]$sha256)
}
d<-do.call(rbind,source);write.csv(d,file.path(out,'zone_source_means.csv'),row.names=FALSE)
write.csv(do.call(rbind,man),file.path(out,'surface_sources.csv'),row.names=FALSE)
effects<-list();samples<-list();rebased<-list();set.seed(cfg$seed)
for(group in names(cfg$surfaces))for(minimum in c(0,1,5)){
 z<-d[d$group==group,];keep<-sort(unique(z$city_id[vapply(seq_len(nrow(z)),function(i){q<-z[z$city_id==z$city_id[i],]
  nrow(q)==3L&&all(is.finite(q$mean_suitability))&&all(q$finite_area_km2>0)&&all(q$native_cell_equivalents>=minimum)},logical(1))]))
 n<-length(keep);if(n<2L)next
 y<-w<-matrix(NA_real_,n,3,dimnames=list(keep,zones))
 for(j in 1:3){q<-z[z$zone==zones[j],];ix<-match(keep,q$city_id);y[,j]<-q$mean_suitability[ix];w[,j]<-q$finite_area_km2[ix]}
 y<-cbind(y,CITY_TOTAL=rowSums(y[,2:3,drop=FALSE]*w[,2:3,drop=FALSE])/rowSums(w[,2:3,drop=FALSE]))
 for(j in 1:4)samples[[length(samples)+1L]]<-data.frame(city_id=keep,group=group,minimum_cell_equivalents=minimum,zone=colnames(y)[j],mean_suitability=y[,j])
 draw<-replicate(cfg$bootstrap_replicates,sample.int(n,n,replace=TRUE))
 for(j in 1:4){b<-colMeans(matrix(y[,j][draw],nrow=n))/colMeans(matrix(y[,1][draw],nrow=n))*100;ci<-quantile(b,c(.025,.975),names=FALSE)
  rebased[[length(rebased)+1L]]<-data.frame(group=group,minimum_cell_equivalents=minimum,n_cities=n,zone=colnames(y)[j],mean_index=mean(y[,j]),outside_mean=mean(y[,1]),outside_reference_100=100*mean(y[,j])/mean(y[,1]),lower=ci[1],upper=ci[2],summary_rule='Ratio of equal-city means, not mean of city ratios')}
 definitions<-list(CITY_MINUS_RING=c(4,1),NON_RECLAIMED_MINUS_RING=c(2,1),RECLAIMED_MINUS_RING=c(3,1),RECLAIMED_MINUS_NON_RECLAIMED=c(3,2))
 for(label in names(definitions)){ab<-definitions[[label]];delta<-y[,ab[1]]-y[,ab[2]]
  b<-colMeans(matrix(delta[draw],nrow=n));ci<-quantile(b,c(.025,.975),names=FALSE)
  independent<-vapply(seq_len(ncol(draw)),function(j)mean(delta[draw[,j]]),numeric(1))
  stopifnot(max(abs(b-independent))<1e-12)
  effects[[length(effects)+1L]]<-data.frame(group=group,minimum_cell_equivalents=minimum,n_cities=n,contrast=label,
   target_mean=mean(y[,ab[1]]),reference_mean=mean(y[,ab[2]]),difference=mean(delta),lower=ci[1],upper=ci[2])
 }
}
write.csv(do.call(rbind,effects),file.path(out,'zone_effects.csv'),row.names=FALSE)
write.csv(do.call(rbind,samples),file.path(out,'matched_city_means.csv'),row.names=FALSE)
write.csv(do.call(rbind,rebased),file.path(out,'common_reference_summary.csv'),row.names=FALSE)
write_json(list(status='PASS_FULL_SURFACE_ZONE_ARITHMETIC',checks=checks,configuration_sha256=digest(file=a[1],algo='sha256'),
 primary_support='Any positive finite area in all three zones within each surface; 1/5-cell restrictions supplied as sensitivities',
 species_definition='Uses each stored surface unchanged; no additional fixed species intersection and no FULL/ENV mask intersection',
 interpretation='New reclamation geometry; within-surface spatial contrasts, not reproduction of historical zone geometry or causal effects'),
 file.path(out,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
