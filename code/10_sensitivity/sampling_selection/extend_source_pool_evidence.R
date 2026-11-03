# Reconcile source-city coverage with retained historical screening tables.
# Usage: Rscript extend_source_pool_evidence.R configuration.json
suppressPackageStartupMessages({library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1]); out <- cfg$output_root
stopifnot(!dir.exists(out)); dir.create(out, recursive=TRUE)
normalise <- function(x) {
 x <- iconv(tolower(trimws(x)), to='ASCII//TRANSLIT')
 trimws(gsub('[^a-z0-9]+', ' ', x))
}
read_input <- function(path) read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
p <- read_input(cfg$pool); cur <- read_input(cfg$current)
early <- read_input(cfg$historical_taxa); filtered <- read_input(cfg$historical_filtered)
invasive <- read_input(cfg$historical_invasive)
stopifnot(nrow(p)==135, !anyDuplicated(p$key), sum(p$retained_candidate)==106)
key <- normalise(p$city); early_key <- normalise(early$City)
stopifnot(!anyDuplicated(early_key))
ix <- match(key,early_key); cx <- match(p$key,cur$candidate_source_key)
num <- function(x) suppressWarnings(as.numeric(gsub('[^0-9.-]', '', as.character(x))))
p$group <- ifelse(p$retained_candidate,'Linked','Unlinked')
p$source_latitude <- num(p$latitude_raw)
p$historical_latitude <- num(early$Latitude[ix])
p$latitude <- p$source_latitude
missing <- !is.finite(p$latitude) & is.finite(p$historical_latitude)
p$latitude[missing] <- p$historical_latitude[missing]
p$latitude_source <- ifelse(is.finite(p$source_latitude),'Source table',ifelse(missing,'Historical geocoded table','Unavailable'))
p$historical_regional_taxa_count <- early$taxon_count[ix]
p$historical_after_amount_filter <- key %in% normalise(filtered$City)
ii <- match(key,normalise(invasive$City))
p$historical_regional_invasive_count <- invasive$taxon_count[ii]
p$amount_class <- ifelse(is.finite(p$reclamation_numeric),'Numeric',ifelse(tolower(trimws(p$reclamation_raw))=='negligible','Negligible','Missing'))
fields <- c(city_area='area_km2',GDP='GDP2020_2.5min',population='Pop2020_2.5min',built_up='BUILT_S_E2025_2.5min',night_light='VIIRS_2023_2.5min')
for(nm in names(fields)) p[[nm]] <- ifelse(p$retained_candidate,cur[[fields[[nm]]]][cx],NA_real_)
p$reclamation_fraction <- p$reclamation_numeric/(100*p$city_area)
p$all_plant_record_count <- NA_real_
write.csv(p,file.path(out,'source_pool_extended.csv'),row.names=FALSE,na='')
variables <- c('latitude','city_area','reclamation_numeric','reclamation_fraction','GDP','population','built_up','night_light','all_plant_record_count')
coverage <- do.call(rbind,lapply(variables,function(v)do.call(rbind,lapply(c('Linked','Unlinked'),function(g){
 z<-p[p$group==g,v]; data.frame(variable=v,group=g,n_group=length(z),n_available=sum(is.finite(z)),fraction_available=mean(is.finite(z)))
}))))
coverage <- rbind(coverage,do.call(rbind,lapply(c('Linked','Unlinked'),function(g)data.frame(variable='continent',group=g,n_group=sum(p$group==g),n_available=sum(p$group==g & !is.na(p$continent)),fraction_available=1))))
write.csv(coverage,file.path(out,'covariate_coverage.csv'),row.names=FALSE)
balance <- do.call(rbind,lapply(variables,function(v){
 a<-p[p$group=='Linked',v]; b<-p[p$group=='Unlinked',v]; a<-a[is.finite(a)]; b<-b[is.finite(b)]
 enough<-length(a)>2 && length(b)>2
 data.frame(variable=v,n_linked=length(a),n_unlinked=length(b),mean_linked=if(length(a))mean(a)else NA_real_,mean_unlinked=if(length(b))mean(b)else NA_real_,
  standardized_difference=if(enough)(mean(a)-mean(b))/sqrt((var(a)+var(b))/2)else NA_real_,
  status=if(enough)'PARTIAL_COVERAGE_DESCRIPTIVE'else'NOT_ESTIMABLE_MISSING_COMPARATOR')
}))
write.csv(balance,file.path(out,'available_balance.csv'),row.names=FALSE,na='')
amount<-as.data.frame(table(group=p$group,amount_class=p$amount_class))
write.csv(amount,file.path(out,'source_amount_coverage.csv'),row.names=FALSE)
unlinked<-p[!p$retained_candidate,]
write.csv(unlinked[,c('city','country_raw','amount_class','latitude','latitude_source','historical_regional_taxa_count','historical_after_amount_filter','historical_regional_invasive_count')],file.path(out,'unlinked_historical_evidence.csv'),row.names=FALSE,na='')
code<-readLines(cfg$preparation_script,warn=FALSE)
hit<-grep('filter\\(reclamation_area != "Negligible"\\)|JOIN flora2 ON city.id = flora2.region_id|filter\\(status == "Invasive"\\)',code)
write.csv(data.frame(line=hit,code=trimws(code[hit])),file.path(out,'screening_code_evidence.csv'),row.names=FALSE)
inputs<-unlist(cfg[c('pool','current','historical_taxa','historical_filtered','historical_invasive','preparation_script')])
write.csv(data.frame(input=names(inputs),sha256=vapply(inputs,function(f)digest(file=f,algo='sha256'),character(1))),file.path(out,'input_checksums.csv'),row.names=FALSE)
write_json(list(status='SOURCE_LINEAGE_AND_PARTIAL_COVERAGE_COMPLETED',source_n=nrow(p),linked=sum(p$retained_candidate),unlinked=nrow(unlinked),
 historical_regional_matches_unlinked=sum(is.finite(unlinked$historical_regional_taxa_count)),
 recovered_latitudes_unlinked=sum(is.finite(unlinked$latitude)),
 numeric_amount_linked=sum(p$retained_candidate & p$amount_class=='Numeric'),
 negligible_unlinked=sum(unlinked$amount_class=='Negligible'),missing_amount_unlinked=sum(unlinked$amount_class=='Missing'),
 historical_filter_found=length(hit)>0,complete_ten_variable_balance=FALSE,
 interpretation=c('Historical counts are regional GloNAF matches, not georeferenced city occurrence counts',
 'Retained code explicitly filters negligible reclamation in an earlier pipeline; final 108-city assembly is not fully reconstructed',
 'Recovered historical coordinates provide partial latitude coverage only',
 'Missing predictor values and all-plant recording effort are not imputed',
 'Source linkage must not be equated with a causal selection model')),
 file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
stopifnot(sum(amount$Freq)==135,sum(coverage$variable=='continent')==2,
 all(is.na(p$GDP[!p$retained_candidate])),all(is.na(p$all_plant_record_count)))
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
