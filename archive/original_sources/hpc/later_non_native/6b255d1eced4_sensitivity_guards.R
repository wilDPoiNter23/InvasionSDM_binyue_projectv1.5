# Original source: sensitivity_guards.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/sensitivity_guards.R
# Original source SHA-256: 6b255d1eced4ee08b6cd64b34fcfcd7117c25a39f11ceb0c0fa1f9f9b37e412e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
prune_formula=function(f,excluded){
 tt=terms(f);lab=attr(tt,'term.labels');keep=vapply(lab,function(t)!any(all.vars(str2lang(t))%in%excluded),logical(1));response=if(attr(tt,'response'))paste(deparse(f[[2]]),collapse='')else NULL
 reformulate(lab[keep],response=response,intercept=attr(tt,'intercept'),env=environment(f))
}
predictor_subset=function(p,h,variant){stopifnot(!anyDuplicated(p),variant%in%c('ENV','CLEAN_ENV','CLEAN_FULL'));if(variant=='ENV'&&'ID'%in%p)stop('ID requires paired clean route');exclude=if(variant=='CLEAN_FULL')'ID' else c(h,if(variant=='CLEAN_ENV')'ID');p[!p%in%exclude]}
load_saved=function(p){if(grepl('\\.rds$',p))return(readRDS(p));e=new.env(parent=globalenv());nn=load(p,e);stopifnot(length(nn)==1);get(nn[1],e)}
formula_signature=function(f){t=terms(f);list(response=if(attr(t,'response'))paste(deparse(f[[2]]),collapse='')else NULL,intercept=attr(t,'intercept'),terms=attr(t,'term.labels'))}
normalise_option_formulas=function(op){for(k in seq_along(op@options))for(n in names(op@options[[k]]@args.values)){v=op@options[[k]]@args.values[[n]];if('formula'%in%names(v)&&inherits(v$formula,'formula'))v$formula=formula_signature(v$formula);op@options[[k]]@args.values[[n]]=v};op}
options_equal=function(actual,expected)isTRUE(all.equal(normalise_option_formulas(actual),normalise_option_formulas(expected),ignore.environment=TRUE))
