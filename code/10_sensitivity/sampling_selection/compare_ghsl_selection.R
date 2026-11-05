# Compare source-city inclusion groups under a common GHSL boundary definition.
# Usage: Rscript compare_ghsl_selection.R configuration.json
suppressPackageStartupMessages({library(jsonlite);library(digest)})
cfg<-fromJSON(commandArgs(TRUE)[1]);out<-cfg$output_root
if(dir.exists(out))stop('Existing comparisons retained; use a fresh output_root.')
dir.create(out,recursive=TRUE,showWarnings=FALSE);set.seed(20261019)
u<-read.csv(cfg$unit_covariates,stringsAsFactors=FALSE,colClasses=c(unit_id='character'))
cities<-read.csv(cfg$crosswalk,stringsAsFactors=FALSE,colClasses=c(ghsl_ids='character'),na.strings='')
counts<-read.csv(cfg$gbif_counts,stringsAsFactors=FALSE)
stopifnot(nrow(cities)==135,!anyDuplicated(u$unit_id),!anyDuplicated(cities$source_key))
split_ids<-function(x)as.integer(strsplit(x,';',fixed=TRUE)[[1]])
stopifnot(!anyDuplicated(paste(counts$ID_UC_G0,counts$window)))
for(window in unique(counts$window)){
 d<-counts[counts$window==window,];name<-paste0('gbif_',window)
 u[[name]]<-vapply(u$unit_id,function(s){
  ids<-split_ids(s);z<-d[match(ids,d$ID_UC_G0),]
  if(nrow(z)==length(ids)&&all(z$status %in% c('QUERIED','CACHED'))&&all(is.finite(z$count)))sum(z$count)else NA_real_
 },numeric(1))
 u[[paste0(name,'_density')]]<-u[[name]]/u$area_km2
}
u$continent<-vapply(u$unit_id,function(s){x<-unique(cities$continent[cities$ghsl_ids %in% s]);stopifnot(length(x)==1);x},character(1))
u$absolute_latitude<-abs(u$latitude)
source<-merge(cities,u,by.x='ghsl_ids',by.y='unit_id',all.x=TRUE,sort=FALSE,suffixes=c('_source','_unit'))
source$group<-source$source_group;source$continent<-source$continent_source
stopifnot(nrow(source)==135)
write.csv(u,file.path(out,'unique_centre_analysis_data.csv'),row.names=FALSE,na='')
write.csv(source,file.path(out,'all_135_source_records.csv'),row.names=FALSE,na='')

primary<-c('latitude','area_km2','mapped_reclamation_ha','mapped_reclamation_fraction',
 'ghsl_gdp_average_ppp_2020','ghsl_population_2020','ghsl_built_fraction_2020',
 'night_light_mean_2023','gbif_all_available')
secondary<-c('absolute_latitude','ghsl_population_density_2020','ghsl_built_surface_km2_2020',
 'project_gdp_mean_2020','project_population_mean_2020','project_built_mean_2025',
 'gbif_all_available_density','gbif_years2000_2024','gbif_years2000_2024_density',
 'gbif_precise2000_2024','gbif_precise2000_2024_density')
variables<-c(primary,secondary);stopifnot(all(variables %in% names(u)))
smd<-function(a,b){v<-sqrt((var(a)+var(b))/2);if(is.finite(v)&&v>0)(mean(a)-mean(b))/v else NA_real_}
describe<-function(z){if(length(z))c(mean=mean(z),median=median(z),q25=unname(quantile(z,.25)),q75=unname(quantile(z,.75)))else rep(NA_real_,4)}
frames<-list(source_records=source[!is.na(source$ghsl_ids),],unique_centres=u)
results<-list();continents<-list()
for(frame in names(frames)){
 d<-frames[[frame]];d<-d[d$group %in% c('Linked','Unlinked'),]
 for(v in variables){
  a<-d[d$group=='Linked',v];b<-d[d$group=='Unlinked',v];a<-a[is.finite(a)];b<-b[is.finite(b)]
  transform<-if(v %in% c('latitude','absolute_latitude'))'identity'else'log1p'
  stopifnot(transform=='identity'||all(c(a,b)>=0))
  aa<-if(transform=='log1p')log1p(a)else a;bb<-if(transform=='log1p')log1p(b)else b
  enough<-length(a)>2&&length(b)>2
  boot<-if(enough&&frame=='unique_centres')replicate(1999,smd(sample(aa,replace=TRUE),sample(bb,replace=TRUE)))else NA_real_
  ci<-if(sum(is.finite(boot))>1900)quantile(boot,c(.025,.975),na.rm=TRUE,names=FALSE)else c(NA_real_,NA_real_)
  da<-describe(a);db<-describe(b)
  results[[length(results)+1L]]<-data.frame(frame=frame,variable=v,family=if(v %in% primary)'primary'else'secondary',
   n_linked=length(a),n_unlinked=length(b),linked_mean=da[1],linked_median=da[2],linked_q25=da[3],linked_q75=da[4],
   unlinked_mean=db[1],unlinked_median=db[2],unlinked_q25=db[3],unlinked_q75=db[4],
   median_ratio=if(length(b)&&median(b)>0)median(a)/median(b)else NA_real_,
   raw_standardized_difference=if(enough)smd(a,b)else NA_real_,
   standardized_difference=if(enough)smd(aa,bb)else NA_real_,lower=ci[1],upper=ci[2],transform=transform,
   p=if(enough&&frame=='unique_centres')suppressWarnings(wilcox.test(a,b,exact=FALSE)$p.value)else NA_real_)
 }
 tab<-table(d$continent,d$group)
 p<-if(frame=='unique_centres')fisher.test(tab,simulate.p.value=TRUE,B=99999)$p.value else NA_real_
 continents[[frame]]<-list(table=as.data.frame(tab),p=p)
}
res<-do.call(rbind,results);res$q<-NA_real_
for(frame in names(frames)){
 ix<-which(res$frame==frame&res$family=='primary');p<-c(res$p[ix],continents[[frame]]$p)
 adj<-p.adjust(p,method='BH',n=length(primary)+1);res$q[ix]<-adj[seq_along(ix)]
 continents[[frame]]$q<-tail(adj,1)
 ix<-which(res$frame==frame&res$family=='secondary');res$q[ix]<-p.adjust(res$p[ix],method='BH',n=length(secondary))
}
write.csv(res,file.path(out,'covariate_group_comparisons.csv'),row.names=FALSE,na='')
cc<-do.call(rbind,lapply(names(continents),function(n){z<-continents[[n]];data.frame(frame=n,z$table,p=z$p,q=z$q)}))
write.csv(cc,file.path(out,'continent_comparisons.csv'),row.names=FALSE)
coverage<-read.csv(cfg$raster_coverage,stringsAsFactors=FALSE,colClasses=c(unit_id='character'))
coverage<-merge(coverage,u[,c('unit_id','group')],by='unit_id')
write.csv(coverage,file.path(out,'raster_coverage_by_group.csv'),row.names=FALSE)
wellcovered<-coverage[coverage$valid_area_fraction>=.8,]
coverage_check<-do.call(rbind,lapply(unique(coverage$variable),function(v){
 ids<-wellcovered$unit_id[wellcovered$variable==v];d<-u[u$unit_id %in% ids,]
 a<-d[d$group=='Linked',v];b<-d[d$group=='Unlinked',v]
 a<-a[is.finite(a)];b<-b[is.finite(b)]
 enough<-sum(is.finite(a))>2&&sum(is.finite(b))>2
 data.frame(variable=v,n_linked=sum(is.finite(a)),n_unlinked=sum(is.finite(b)),
  smd=if(enough)smd(log1p(a),log1p(b))else NA_real_,
  p=if(enough)suppressWarnings(wilcox.test(a,b,exact=FALSE)$p.value)else NA_real_)
}))
write.csv(coverage_check,file.path(out,'coverage_80_percent_sensitivity.csv'),row.names=FALSE,na='')
write_json(list(status='COMPARISONS_COMPLETED',source_pool=135,linked_source=106,unlinked_source=29,
 mapped_source=as.list(table(source$group[!is.na(source$ghsl_ids)])),unique_centres=as.list(table(u$group)),
 unresolved=cities$source_city[is.na(cities$ghsl_ids)],
 primary_variables=primary,primary_family_size=10,
 statistics='Wilcoxon rank-sum tests; BH across nine numeric primary comparisons plus continent. Secondary BH family separate. Standardized mean differences after log1p, except latitude. Bootstrap 95% percentile intervals use 1999 resamples of independent urban centres.',
 inference_frame='Unique urban centres are primary inferential units; source-record results retain original sample-frame weighting as sensitivity.',
 source_record_intervals='Descriptive summaries only; no p-values or bootstrap intervals because shared centres repeat source records',
 city_identity='106 linked / 29 unlinked source records; this is not a reconstructed occurrence-based 108 / 27 split',
 gbif_scope='Independent Plantae count queries, not selected invasive-species records. Reporting volume is a sampling proxy, not completeness.',
 no_model_refits=TRUE),file.path(out,'comparison_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
inputs<-unlist(cfg[c('unit_covariates','crosswalk','gbif_counts','raster_coverage')])
write.csv(data.frame(input=names(inputs),sha256=vapply(inputs,function(p)digest(file=p,algo='sha256'),character(1))),file.path(out,'comparison_input_checksums.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,'comparison_sessionInfo.txt'))
