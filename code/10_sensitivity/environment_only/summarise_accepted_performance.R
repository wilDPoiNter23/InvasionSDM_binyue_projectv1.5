#!/usr/bin/env Rscript
# Partition- and route-specific comparisons of accepted selected-unit evaluations.
suppressPackageStartupMessages({library(ggplot2);library(digest)})
a=commandArgs(TRUE)
if(length(a)!=3L)stop('Usage: summarise_accepted_performance.R ACCEPTED_EVALUATIONS ATTRITION_DETAILS OUTPUT_ROOT')
source=a[1];details_path=a[2];out=a[3]
for(d in c('tables','figures','qc','logs'))dir.create(file.path(out,d),recursive=TRUE,showWarnings=FALSE)
target=file.path(out,'tables/paired_selected_performance_summary.csv');if(file.exists(target))stop('Performance summary exists; inspect before rerunning')
x=read.csv(source,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding='UTF-8-BOM');details=read.csv(details_path,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM')
stopifnot(length(unique(x$species))==684,nrow(x)==8208,!anyDuplicated(x[c('species','group','metric','evaluation_stage')]))
x$value=as.numeric(x$value)
f=x[x$group=='FULL',];e=x[x$group=='ENV',]
key=function(z)paste(z$species,z$metric,z$evaluation_stage,sep='|')
idx=match(key(f),key(e));stopifnot(!anyNA(idx),length(unique(idx))==nrow(e))
pair=data.frame(species=f$species,metric=f$metric,evaluation_stage=f$evaluation_stage,full_value=f$value,env_value=e$value[idx],full_unit=f$projection_unit,env_unit=e$projection_unit[idx],full_model=f$selected_model,env_model=e$selected_model[idx],full_role=f$comparison_role,env_role=e$comparison_role[idx],full_mapping=f$unit_mapping_state,env_mapping=e$unit_mapping_state[idx],full_availability=f$metric_availability,env_availability=e$metric_availability[idx])
pair$route_stratum=ifelse(pair$full_unit==pair$env_unit,paste0('same_',pair$full_unit),paste(pair$full_unit,pair$env_unit,sep='_TO_'))
algorithm=function(unit,model)ifelse(unit=='accepted_ensemble',sub('.*_(EM[^_]+)_.*','\\1',model),ifelse(unit=='accepted_single_model',sub('.*_','',model),'UNRESOLVED'))
pair$full_algorithm=algorithm(pair$full_unit,pair$full_model);pair$env_algorithm=algorithm(pair$env_unit,pair$env_model)
pair$algorithm_matched=pair$full_algorithm==pair$env_algorithm
pair$included_exact_finite=pair$full_mapping=='EXACT_SELECTED_UNIT'&pair$env_mapping=='EXACT_SELECTED_UNIT'&pair$full_availability=='AVAILABLE'&pair$env_availability=='AVAILABLE'&is.finite(pair$full_value)&is.finite(pair$env_value)
pair$metric_difference=ifelse(pair$included_exact_finite,pair$env_value-pair$full_value,NA_real_)
pair$comparison_scope=ifelse(pair$evaluation_stage=='calibration','DESCRIPTIVE_TRAINING_PARTITION',ifelse(startsWith(pair$route_stratum,'same_'),'MATCHED_PARTITION_SAME_UNIT_TYPE','MIXED_UNIT_DESCRIPTIVE_ONLY'))
di=match(pair$species,details$accepted_taxon);stopifnot(!anyNA(di))
pair$occurrence_record_count=details$occurrence_record_count[di];pair$matched_full_predictor_count=details$matched_full_predictor_count[di]
write.csv(pair,file.path(out,'tables/paired_selected_performance_source.csv'),row.names=FALSE,na='')
coverage=aggregate(included_exact_finite~metric+evaluation_stage+route_stratum,pair,sum)
names(coverage)[4]='available_exact_pairs';coverage=merge(coverage,aggregate(list(candidate_pairs=rep(1L,nrow(pair))),pair[c('metric','evaluation_stage','route_stratum')],sum),by=c('metric','evaluation_stage','route_stratum'))
write.csv(coverage,file.path(out,'qc/paired_performance_coverage.csv'),row.names=FALSE,na='')
set.seed(20261014);B=1999L;results=list();distributions=list();i=0L
for(metric in unique(pair$metric))for(partition in unique(pair$evaluation_stage)) {
  base=pair[pair$metric==metric&pair$evaluation_stage==partition,]
  route_options=c('ALL_EXACT_FINITE_DESCRIPTIVE',unique(base$route_stratum),paste0(unique(base$route_stratum),'_ALGORITHM_MATCHED'))
  for(stratum in route_options) {
    matched_algorithm=endsWith(stratum,'_ALGORITHM_MATCHED');route=if(matched_algorithm)sub('_ALGORITHM_MATCHED$','',stratum)else stratum
    z=if(stratum=='ALL_EXACT_FINITE_DESCRIPTIVE')base else base[base$route_stratum==route,]
    if(matched_algorithm)z=z[z$algorithm_matched,]
    candidates=nrow(z);z=z[z$included_exact_finite,];n=nrow(z)
    if(n<3L)next
    d=z$metric_difference;ci=quantile(replicate(B,mean(sample(d,n,replace=TRUE))),c(.025,.975))
    tt=tryCatch(t.test(z$env_value,z$full_value,paired=TRUE),error=function(e)NULL)
    wt=tryCatch(wilcox.test(z$env_value,z$full_value,paired=TRUE,exact=FALSE),error=function(e)NULL)
    comparable=startsWith(stratum,'same_');interpretation=if(partition=='calibration')'Descriptive calibration comparison; not held-out performance'else if(stratum=='ALL_EXACT_FINITE_DESCRIPTIVE')'Pooled exact finite selected units; observed unit types and route composition are recorded in the source table; limited coverage does not establish population predictive gain'else if(comparable)'Same partition and selected-unit type; algorithm agreement recorded separately; original species designs still require provenance'else 'Mixed unit types; pooled descriptive difference does not establish matched predictive performance'
    i=i+1L
    results[[i]]=data.frame(metric=metric,evaluation_stage=partition,route_stratum=stratum,candidate_pairs=candidates,n_pairs=n,full_mean=mean(z$full_value),env_mean=mean(z$env_value),full_median=median(z$full_value),env_median=median(z$env_value),mean_env_minus_full=mean(d),median_env_minus_full=median(d),difference_CI_low=ci[1],difference_CI_high=ci[2],paired_t=if(is.null(tt))NA_real_ else unname(tt$statistic),paired_t_df=if(is.null(tt))NA_real_ else unname(tt$parameter),paired_t_p=if(is.null(tt))NA_real_ else tt$p.value,wilcoxon_V=if(is.null(wt))NA_real_ else unname(wt$statistic),wilcoxon_p=if(is.null(wt))NA_real_ else wt$p.value,cohens_dz=if(sd(d)>0)mean(d)/sd(d)else NA_real_,bootstrap_replicates=B,seed=20261014,interpretation=interpretation)
    for(group in c('FULL','ENV')){v=z[[if(group=='FULL')'full_value'else'env_value']];distributions[[paste(i,group)]]=data.frame(metric=metric,evaluation_stage=partition,route_stratum=stratum,group=group,n=n,mean=mean(v),sd=sd(v),minimum=min(v),q025=quantile(v,.025),q25=quantile(v,.25),median=median(v),q75=quantile(v,.75),q975=quantile(v,.975),maximum=max(v))}
  }
}
summary=do.call(rbind,results);write.csv(summary,target,row.names=FALSE,na='')
write.csv(do.call(rbind,distributions),file.path(out,'tables/selected_performance_distributions.csv'),row.names=FALSE,na='')
plot=pair[pair$included_exact_finite,];plot$metric_partition=paste(plot$metric,plot$evaluation_stage,sep=' / ')
p=ggplot(plot,aes(x=route_stratum,y=metric_difference))+geom_hline(yintercept=0,colour='grey40',linewidth=.35)+geom_boxplot(outlier.shape=NA,fill='#B9D6D8')+geom_jitter(width=.08,height=0,alpha=.35,size=.6)+facet_wrap(~metric_partition,scales='free_y',ncol=2)+coord_flip()+theme_classic(base_size=8,base_family='Arial')+labs(x='Selected-unit route stratum',y='ENV minus FULL metric',subtitle='Exact mapped finite pairs; calibration is descriptive training-partition performance')
ggsave(file.path(out,'figures/selected_performance_differences.pdf'),p,width=9,height=6,device=cairo_pdf)
ggsave(file.path(out,'figures/selected_performance_differences.png'),p,width=9,height=6,dpi=300)
write.csv(data.frame(input_path=c(source,details_path),sha256=c(digest(file=source,algo='sha256'),digest(file=details_path,algo='sha256'))),file.path(out,'qc/paired_performance_input_checksums.csv'),row.names=FALSE)
write.csv(data.frame(check=c('candidate_species','candidate_metric_partition_pairs','exact_finite_pairs','partial_or_missing_excluded','bootstrap_replicates','seed'),value=c(684,nrow(pair),sum(pair$included_exact_finite),sum(!pair$included_exact_finite),B,20261014)),file.path(out,'qc/paired_performance_acceptance_checks.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(out,'logs/performance_sessionInfo.txt'))
cat('Selected performance summaries written with partition and route scope.\n')
quit(save='no',status=0)
