# Test prior mapped reclamation against subsequent first city records.
suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE);sf_use_s2(FALSE)
key<-function(x)tolower(trimws(x))
panel<-read.csv(file.path(cfg$record_root,'prospective_window_support.csv'),stringsAsFactors=FALSE)
first<-read.csv(file.path(cfg$record_root,'city_species_first_year.csv'),stringsAsFactors=FALSE)
roster<-read.csv(cfg$roster,stringsAsFactors=FALSE)$species;stopifnot(length(roster)==813L,!anyDuplicated(roster))
first<-first[first$species%in%roster,];first$key<-key(first$city_id)
minimum_start<-if(is.null(cfg$minimum_outcome_start))1991 else cfg$minimum_outcome_start
panel<-panel[panel$outcome_start>=minimum_start,]
panel$key<-key(panel$city_id)
if(!is.null(cfg$exclude_city_keys))panel<-panel[!panel$key%in%key(cfg$exclude_city_keys),]
stopifnot(all(panel$prior_exposure_end<panel$outcome_start));panel$new_count<-vapply(seq_len(nrow(panel)),function(i)sum(first$key==panel$key[i]&first$first_record_year>=panel$outcome_start[i]&first$first_record_year<=panel$outcome_end[i]),integer(1))
stopifnot(all(panel$new_count<=panel$recorded_species),all(panel$new_count>=0))
gate<-read.csv(cfg$geometry_eligibility,stringsAsFactors=FALSE);keep<-key(gate$city_id[gate$eligible])
areas<-list();sources<-list()
for(path in list.files(cfg$cohort_root,pattern='[.]rds$',full.names=TRUE)){
 z<-readRDS(path);city<-unique(z$city_id);stopifnot(length(city)==1L)
 if(!key(city)%in%keep)next
 stopifnot(st_crs(z)==st_crs(6933));end<-as.integer(as.character(z$reported_category));stopifnot(all(end%in%seq(1990,2020,5)))
 for(year in seq(1990,2020,5)){q<-st_geometry(z)[end==year];area<-if(length(q))sum(as.numeric(st_area(st_union(q))))/1e4 else 0
  areas[[length(areas)+1L]]<-data.frame(key=key(city),city_id=city,endpoint=year,area_ha=area)}
 sources[[length(sources)+1L]]<-data.frame(path=path,sha256=digest(file=path,algo='sha256'))
}
exposure<-do.call(rbind,areas);stopifnot(!anyDuplicated(exposure[,c('key','endpoint')]),all(is.finite(exposure$area_ha)&exposure$area_ha>=0))
lookup<-function(k,year)exposure$area_ha[match(paste(k,year),paste(exposure$key,exposure$endpoint))]
panel$lag_area_ha<-lookup(panel$key,panel$prior_exposure_end)
panel$lead_area_ha<-lookup(panel$key,panel$outcome_end+5)
panel$duration<-panel$outcome_end-panel$outcome_start+1
panel$period<-factor(panel$outcome_start);panel$log_lag<-log1p(panel$lag_area_ha);panel$log_lead<-log1p(panel$lead_area_ha)
panel$log_history<-log1p(panel$historical_records);panel$log_intensity<-log1p(panel$target_records)
write.csv(exposure,file.path(out,'dated_reclamation_area.csv'),row.names=FALSE)
write.csv(panel,file.path(out,'all_city_period_support.csv'),row.names=FALSE)
write.csv(do.call(rbind,sources),file.path(out,'cohort_sources.csv'),row.names=FALSE)
results<-list();support<-list();fitted_rows<-list()
for(history in c(1,10))for(lead in c(FALSE,TRUE))for(effort in c(FALSE,TRUE)){
 case<-paste0('H',history,if(lead)'_LAG_AND_LEAD'else'_LAG',if(effort)'_M1'else'_M0')
 d<-panel[is.finite(panel$log_lag)&panel$recording_supported&panel$historical_records>=history,]
 if(lead)d<-d[is.finite(d$log_lead),]
 city_counts<-table(d$key);positive<-tapply(d$new_count,d$key,sum)
 ids<-intersect(names(city_counts[city_counts>=2]),names(positive[positive>0]));d<-d[d$key%in%ids,];d$city<-factor(d$key);d$period<-droplevels(d$period)
 G<-length(unique(d$city));status<-'INSUFFICIENT_SUPPORT'
 if(G>=20L){
  rhs<-c('log_lag',if(lead)'log_lead','log_history',if(effort)'log_intensity','city','period','offset(log(duration))')
  model<-glm(reformulate(rhs,response='new_count'),family=poisson(),data=d,control=glm.control(maxit=100))
  status<-'CONVERGENCE_OR_RANK_FAILED'
  if(model$converged&&all(is.finite(coef(model)))){
   X<-model.matrix(model);mu<-fitted(model);p<-ncol(X);n<-nrow(X)
   stopifnot(n>p+10,all(mu>0),max(abs(colSums(X*(d$new_count-mu))))<1e-4)
   bread<-solve(crossprod(X,X*mu));score<-rowsum(X*(d$new_count-mu),d$city)
   V<-bread%*%crossprod(score)%*%bread*(G/(G-1))*((n-1)/(n-p));se<-sqrt(diag(V));critical<-qt(.975,G-1)
   # Verify the estimating equations and the independent IRLS crossproduct route.
   expected_information<-crossprod(X*sqrt(mu));stopifnot(max(abs(expected_information-crossprod(X,X*mu)))<1e-6)
   for(term in c('log_lag',if(lead)'log_lead')){b<-coef(model)[term];s<-se[term];lo<-b-critical*s;hi<-b+critical*s
    results[[length(results)+1L]]<-data.frame(case=case,term=term,estimate=b,se=s,lower=lo,upper=hi,p=2*pt(-abs(b/s),G-1),
     n_cities=G,n_period_rows=n,new_record_species=sum(d$new_count),pearson_dispersion=sum(residuals(model,type='pearson')^2)/df.residual(model),
     rate_change_doubling_area_plus_one=100*expm1(log(2)*b),rate_change_lower=100*expm1(log(2)*lo),rate_change_upper=100*expm1(log(2)*hi))}
   saveRDS(model,file.path(out,paste0(case,'.rds')));fitted_rows[[case]]<-transform(d,case=case);status<-'FITTED_POISSON_CITY_PERIOD_FE_CLUSTER_CITY'
  }
 }
 support[[length(support)+1L]]<-data.frame(case=case,n_cities=G,n_rows=nrow(d),status=status)
}
write.csv(do.call(rbind,support),file.path(out,'model_support.csv'),row.names=FALSE)
if(length(results))write.csv(do.call(rbind,results),file.path(out,'lead_lag_coefficients.csv'),row.names=FALSE)
if(length(fitted_rows))write.csv(do.call(rbind,fitted_rows),file.path(out,'fitted_city_period_data.csv'),row.names=FALSE)
write_json(list(status='LEAD_LAG_RECORD_ASSOCIATION_COMPLETE',roster_n=813,roster_sha256=digest(file=cfg$roster,algo='sha256'),
 minimum_outcome_start=minimum_start,primary='H1_LAG_M0; H1_LAG_M1 recording-intensity sensitivity; all H10 and lead checks retained',
 outcome='First recorded selected species in a city during a later window, not verified arrival or establishment',
 sample='Mapped-cohort cities with recorded targets, historical recording support, at least two periods and a positive outcome sum',
 uncertainty='City-cluster sandwich HC1 with finite-group t intervals; Poisson mean with dispersion reported',
 no_sdm_refit=TRUE,recalibrated_ipi=FALSE,causal_identification=FALSE,configuration_sha256=digest(file=a[1],algo='sha256')),
 file.path(out,'receipt.json'),auto_unbox=TRUE,pretty=TRUE)
