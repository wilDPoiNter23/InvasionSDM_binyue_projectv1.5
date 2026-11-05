# Reuse the existing model and sampler for selected cases with longer chains.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
arguments<-commandArgs(TRUE);stopifnot(length(arguments)==2L)
caller<-sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)[1])
expressions<-parse(file.path(dirname(caller),'coastal_bayes_fit.R'))
for(expression in expressions){
 if(is.call(expression)&&identical(expression[[1]],as.name('<-'))&&identical(expression[[2]],as.name('statuses')))break
 eval(expression,envir=.GlobalEnv)
}
settings<-list(primary=c(.0005,2,1),prior_sensitivity=c(.0005,4,.25),transform_sensitivity=c(.001,2,1))
selected<-unlist(cfg$selected_cases);stopifnot(length(selected)>0,all(selected%in%names(settings)))
statuses<-setNames(lapply(selected,function(label){s<-settings[[label]];run_case(label,s[1],s[2],s[3])}),selected)
write_json(list(status='BAYESIAN_FITS_COMPLETED',cases=statuses,all_rows=alln,finite_rows=nrow(d),species_files=length(files),seed=cfg$seed,warmup=cfg$warmup,samples=cfg$samples,selected_case_execution=TRUE,manifest_sha256=digest(file=cfg$paired_manifest,algo='sha256')),file.path(out,'completion.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
