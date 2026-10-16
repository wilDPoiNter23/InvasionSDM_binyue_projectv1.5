# Estimate cohort and land-use associations with crossed and nested intercepts.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==2L);cfg<-fromJSON(a[1]);group<-a[2]
script<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)[1]);source(file.path(dirname(script),'coastal_bayes_core.R'))
out<-file.path(cfg$output_root,'models',group);stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
files<-list.files(file.path(cfg$output_root,'species',group),pattern='[.]csv$',full.names=TRUE);stopifnot(length(files)==684)
d<-do.call(rbind,lapply(files,read.csv));alln<-nrow(d);d<-d[is.finite(d$mean_suitability)&d$finite_area_km2>0,]
stopifnot(nrow(d)>1000,!anyDuplicated(paste(d$species,d$zone_id)))
z<-read.csv(file.path(cfg$output_root,'zones.csv'));mu<-tapply(z$age_index_years,z$city_id,mean)
d$age_within_5<-(d$age_index_years-mu[d$city_id])/5;d$age_between_5<-(mu[d$city_id]-mean(mu))/5
center<-mean(log(z$area_km2));scale<-sd(log(z$area_km2));d$log_area_z<-(log(d$area_km2)-center)/scale
uses<-names(sort(table(d$landuse),decreasing=TRUE));d$landuse<-factor(d$landuse,levels=uses)
X<-model.matrix(~age_within_5+age_between_5+landuse+log_area_z,d)
groups<-list(city=as.integer(factor(d$city_id)),species=as.integer(factor(d$species)),zone=as.integer(factor(d$zone_id)))
write.csv(data.frame(variable=names(d),class=vapply(d,function(x)class(x)[1],character(1))),file.path(out,'input_schema.csv'),row.names=FALSE)
saveRDS(d,file.path(out,'model_input.rds'));write.csv(aggregate(mean_suitability~city_id+zone_id+period_endpoint+landuse,d,mean),file.path(out,'zone_species_mean.csv'),row.names=FALSE)
run_case<-function(label,epsilon,prior_scale,variance_rate){
 dest<-file.path(out,label);dir.create(dest);y<-qlogis((d$mean_suitability+epsilon)/(1+2*epsilon))
 coastal_compile()
 worker<-function(chain){cat(group,label,'chain',chain,'\n');flush.console();coastal_gibbs(X,y,groups,warmup=cfg$warmup,samples=cfg$samples,thin=cfg$thin,seed=cfg$seed+chain,prior_scale=prior_scale,variance_rate=variance_rate)}
 fits<-if(.Platform$OS.type=='unix')parallel::mclapply(1:4,worker,mc.cores=4L,mc.set.seed=FALSE)else lapply(1:4,worker)
 stopifnot(all(vapply(fits,is.list,logical(1))))
 saveRDS(fits,file.path(dest,'posterior_chains.rds'));B<-do.call(rbind,lapply(fits,`[[`,'beta'));V<-do.call(rbind,lapply(fits,`[[`,'variance'))
 mats<-lapply(fits,function(f)cbind(f$beta,sqrt(f$variance)));mat<-do.call(rbind,mats);colnames(mat)<-c(colnames(X),paste0('sd_',c('residual',names(groups))))
 summ<-do.call(rbind,lapply(seq_len(ncol(mat)),function(j){v<-mat[,j];ch<-lapply(mats,function(m)m[,j]);q<-quantile(v,c(.025,.975));data.frame(term=colnames(mat)[j],mean=mean(v),lower=q[1],upper=q[2],probability_positive=mean(v>0),rank_split_rhat=coastal_rhat(ch),ess_initial_positive=coastal_ess(ch))}))
 write.csv(summ,file.path(dest,'posterior_summary.csv'),row.names=FALSE)
 # Marginalise intercept variation using deterministic normal quantiles.
 choose<-seq(1,nrow(B),length.out=min(1200,nrow(B)));B<-B[choose,,drop=FALSE];V<-V[choose,,drop=FALSE];normal<-qnorm(((1:41)-.5)/41)
 base<-z;basis<-function(use,age){q<-data.frame(age_within_5=(age-mean(mu))/5,age_between_5=0,landuse=factor(use,levels=uses),log_area_z=0);model.matrix(~age_within_5+age_between_5+landuse+log_area_z,q,contrasts.arg=list(landuse=contrasts(d$landuse)))}
 pred<-function(use,age){xx<-basis(use,age);eta<-drop(B%*%xx[1,]);sd<-sqrt(rowSums(V));v<-rowMeans(plogis(outer(eta,rep(1,41))+outer(sd,normal)));v*(1+2*epsilon)-epsilon}
 ages<-sort(unique(z$age_index_years));grid<-list();ctr<-list();ref<-uses[1]
 for(use in uses)for(age in ages){v<-pred(use,age);q<-quantile(v,c(.025,.975));grid[[length(grid)+1L]]<-data.frame(landuse=use,age_index_years=age,mean=mean(v),lower=q[1],upper=q[2])}
 summarize<-function(v,type,from,to,age){q<-quantile(v,c(.025,.975));data.frame(type=type,from=from,to=to,age_index_years=age,difference=mean(v),lower=q[1],upper=q[2],probability_positive=mean(v>0))}
 for(age in ages)if(age+10<=max(ages)){old<-pred(ref,age+10);young<-pred(ref,age);ctr[[length(ctr)+1L]]<-summarize(old-young,'TEN_YEAR_COHORT_DIFFERENCE',age,age+10,age)}
 mid<-median(ages);for(use in uses[-1])ctr[[length(ctr)+1L]]<-summarize(pred(use,mid)-pred(ref,mid),'USE_VS_REFERENCE',ref,use,mid)
 write.csv(do.call(rbind,grid),file.path(dest,'age_use_predictions.csv'),row.names=FALSE);write.csv(do.call(rbind,ctr),file.path(dest,'response_scale_contrasts.csv'),row.names=FALSE)
 # Predictive comparison on the fitted transformed scale.
 ppc<-do.call(rbind,lapply(seq_along(fits),function(i){f<-fits[[i]];err<-f$residual;data.frame(chain=i,conditional_residual_mean=mean(err),conditional_residual_sd=sd(err),posterior_residual_sd=mean(sqrt(f$variance[,1])),residual_correlation_age=cor(err,d$age_within_5))}))
 write.csv(ppc,file.path(dest,'predictive_residual_check.csv'),row.names=FALSE)
 status<-if(all(summ$rank_split_rhat<1.01)&all(summ$ess_initial_positive>=400))'PASS_CHAIN_DIAGNOSTICS'else'REQUIRES_CHAIN_REVIEW'
 write_json(list(status=status,epsilon=epsilon,beta_prior_sd=prior_scale,variance_prior_shape=2,variance_prior_rate=variance_rate,
 rows=nrow(d),cities=length(unique(d$city_id)),zones=length(unique(d$zone_id)),species=length(unique(d$species)),reference_use=ref,
 model='Transformed Gaussian; normal fixed effects; city and nested city-zone intercepts crossed with species intercepts',
 age_reference_year=cfg$age_reference_year,age_interpretation='Cross-sectional five-year formation cohorts, not longitudinal change',
 uncertainty='Conditional on saved SDM predictions; posterior sampling covers regression parameters'),file.path(dest,'receipt.json'),pretty=TRUE,auto_unbox=TRUE)
 status
}
statuses<-list(primary=run_case('primary',.0005,2,1),prior_sensitivity=run_case('prior_sensitivity',.0005,4,.25),transform_sensitivity=run_case('transform_sensitivity',.001,2,1))
write_json(list(status='BAYESIAN_FITS_COMPLETED',cases=statuses,all_rows=alln,finite_rows=nrow(d),species_files=length(files),seed=cfg$seed,
 manifest_sha256=digest(file=cfg$paired_manifest,algo='sha256')),file.path(out,'completion.json'),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
