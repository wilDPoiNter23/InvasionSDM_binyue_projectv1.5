# Model design helpers
# Input: stored predictor vectors, formulas and model options
# Output: environmental predictor subsets and unchanged-design checks
prune_formula=function(f,excluded){
 tt=terms(f);lab=attr(tt,'term.labels');keep=vapply(lab,function(t)!any(all.vars(str2lang(t))%in%excluded),logical(1));response=if(attr(tt,'response'))paste(deparse(f[[2]]),collapse='')else NULL
 reformulate(lab[keep],response=response,intercept=attr(tt,'intercept'),env=environment(f))
}
predictor_subset=function(p,h,variant){stopifnot(!anyDuplicated(p),variant%in%c('ENV','CLEAN_ENV','CLEAN_FULL'));if(variant=='ENV'&&'ID'%in%p)stop('ID requires paired clean route');exclude=if(variant=='CLEAN_FULL')'ID' else c(h,if(variant=='CLEAN_ENV')'ID');p[!p%in%exclude]}
load_saved=function(p){if(grepl('\\.rds$',p))return(readRDS(p));e=new.env(parent=globalenv());nn=load(p,e);stopifnot(length(nn)==1);get(nn[1],e)}
formula_signature=function(f){t=terms(f);list(response=if(attr(t,'response'))paste(deparse(f[[2]]),collapse='')else NULL,intercept=attr(t,'intercept'),terms=attr(t,'term.labels'))}
normalise_option_formulas=function(op){for(k in seq_along(op@options))for(n in names(op@options[[k]]@args.values)){v=op@options[[k]]@args.values[[n]];if('formula'%in%names(v)&&inherits(v$formula,'formula'))v$formula=formula_signature(v$formula);op@options[[k]]@args.values[[n]]=v};op}
options_equal=function(actual,expected)isTRUE(all.equal(normalise_option_formulas(actual),normalise_option_formulas(expected),ignore.environment=TRUE))
