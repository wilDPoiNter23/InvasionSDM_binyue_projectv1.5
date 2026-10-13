#!/usr/bin/env Rscript
# Verify saved selected-unit performance summaries without loading graphics.
a=commandArgs(TRUE)
if(length(a)!=1L)stop('Usage: verify_review_performance.R OUTPUT_ROOT')
out=a[1];pairs=read.csv(file.path(out,'tables/paired_selected_performance_source.csv'));summary=read.csv(file.path(out,'tables/paired_selected_performance_summary.csv'));coverage=read.csv(file.path(out,'qc/paired_performance_coverage.csv'))
stopifnot(nrow(pairs)==4104,length(unique(pairs$species))==684,all(!is.finite(pairs$metric_difference[!pairs$included_exact_finite])))
checks=list()
for(i in seq_len(nrow(summary))){r=summary[i,];p=pairs[pairs$metric==r$metric & pairs$evaluation_stage==r$evaluation_stage,];algorithm=endsWith(r$route_stratum,'_ALGORITHM_MATCHED');route=if(algorithm)sub('_ALGORITHM_MATCHED$','',r$route_stratum)else r$route_stratum;if(route!='ALL_EXACT_FINITE_DESCRIPTIVE')p=p[p$route_stratum==route,];if(algorithm)p=p[p$algorithm_matched,];p=p[p$included_exact_finite,];stopifnot(nrow(p)==r$n_pairs,abs(mean(p$metric_difference)-r$mean_env_minus_full)<1e-12,r$difference_CI_low<=r$difference_CI_high,r$bootstrap_replicates==1999,r$seed==20261014);tt=t.test(p$env_value,p$full_value,paired=TRUE);stopifnot(abs(tt$p.value-r$paired_t_p)<1e-12);checks[[i]]=data.frame(metric=r$metric,partition=r$evaluation_stage,stratum=r$route_stratum,n=r$n_pairs,max_mean_error=abs(mean(p$metric_difference)-r$mean_env_minus_full),status='PASS')}
for(metric in c('TSS','ROC_AUC')){stopifnot(sum(pairs$included_exact_finite & pairs$metric==metric & pairs$evaluation_stage=='validation' & pairs$route_stratum=='same_accepted_single_model')==19,sum(pairs$included_exact_finite & pairs$metric==metric & pairs$evaluation_stage=='calibration' & pairs$route_stratum=='same_accepted_ensemble')==404,sum(pairs$included_exact_finite & pairs$metric==metric & pairs$evaluation_stage=='evaluation')==0)}
write.csv(do.call(rbind,checks),file.path(out,'qc/independent_performance_summary_checks.csv'),row.names=FALSE)
cat('Numeric-only performance summary checks PASS. No missing or partial metric was used as zero.\n')
quit(save='no',status=0)
