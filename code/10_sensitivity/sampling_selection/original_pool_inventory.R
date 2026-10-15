# Inventory exact and candidate record links without inventing excluded-city covariates.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
pool<-read.csv(cfg$original_pool,stringsAsFactors=FALSE);current<-read.csv(cfg$current_cities,stringsAsFactors=FALSE)
normalise<-function(x)trimws(gsub('[^a-z0-9]+',' ',tolower(x)))
stopifnot(nrow(pool)==135,nrow(current)==108,!anyDuplicated(normalise(pool$city)),!anyDuplicated(normalise(current$city)))
current$author_excluded<-tolower(trimws(current$city))%in%tolower(trimws(cfg$exclude_city_keys))
current$source_key<-normalise(current$city);pool$key<-normalise(pool$city)
aliases<-cfg$aliases;link<-current$source_key
for(name in names(aliases))link[link==name]<-aliases[[name]]
current$candidate_source_key<-link;current$source_match<-link%in%pool$key
current$link_type<-ifelse(current$source_key%in%pool$key,'EXACT_NORMALISED',ifelse(current$source_match,'CANDIDATE_ALIAS','OUTSIDE_SOURCE_POOL'))
active<-current[!current$author_excluded,]
pool$retained_exact<-pool$key%in%active$source_key
pool$retained_candidate<-pool$key%in%active$candidate_source_key
pool$continent<-ifelse(grepl('Africa',pool$source_region),'Africa',ifelse(grepl('Asia',pool$source_region),'Asia',
 ifelse(grepl('Europe',pool$source_region),'Europe',ifelse(pool$source_region=='South America','South America',ifelse(pool$source_region=='Oceania','Oceania','North America')))))
pool$numeric_reclamation<-is.finite(pool$reclamation_numeric)
write.csv(pool,file.path(out,'source_pool_membership.csv'),row.names=FALSE);write.csv(current,file.path(out,'current_record_links.csv'),row.names=FALSE)
counts<-coverage<-list();tests<-list();set.seed(cfg$seed)
for(mode in c('retained_exact','retained_candidate')){
 retained<-pool[[mode]];tab<-table(group=ifelse(retained,'Linked','Not linked'),continent=pool$continent)
 counts[[mode]]<-transform(as.data.frame(tab),link_definition=mode)
 test<-fisher.test(tab,simulate.p.value=TRUE,B=99999)
 tests[[mode]]<-data.frame(link_definition=mode,linked=sum(retained),not_linked=sum(!retained),fisher_p=test$p.value,permutations=99999)
 for(group in c(FALSE,TRUE)){q<-pool[retained==group,];coverage[[length(coverage)+1L]]<-data.frame(link_definition=mode,linked=group,n=nrow(q),
  latitude_present=sum(nzchar(as.character(q$latitude_raw))&!is.na(q$latitude_raw)),numeric_reclamation=sum(q$numeric_reclamation),
  city_area_available=0,GDP_available=0,population_available=0,built_available=0,nightlight_available=0,all_plant_effort_available=0)}
}
write.csv(do.call(rbind,counts),file.path(out,'continent_counts.csv'),row.names=FALSE)
write.csv(do.call(rbind,tests),file.path(out,'continent_association.csv'),row.names=FALSE)
write.csv(do.call(rbind,coverage),file.path(out,'source_coverage.csv'),row.names=FALSE)
write_json(list(status='SOURCE_POOL_INVENTORY_COMPLETED',source_n=135,current_original_n=108,current_after_author_exclusion=nrow(active),
 candidate_linked_source_n=sum(pool$retained_candidate),unlinked_source_n=sum(!pool$retained_candidate),
 excluded_current_keys=cfg$exclude_city_keys,outside_original_pool=active$city[!active$source_match],
 input_sha256=list(pool=digest(file=cfg$original_pool,algo='sha256'),current=digest(file=cfg$current_cities,algo='sha256')),
 interpretation='Candidate aliases require identity review; this is not an approved108vs27 analysis. Qualitative negligible areas and missing effort are not set to zero.'),
 file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
