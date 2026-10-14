#!/usr/bin/env Rscript
# Refit saved equations and independently verify DAG total effects by matrix inversion.
suppressPackageStartupMessages(library(jsonlite))
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
if(file.exists(file.path(out,"verification.json")))stop("Existing verification retained")
edges<-read.csv(file.path(out,"edges.csv"));effects<-read.csv(file.path(out,"effects.csv"));draws<-read.csv(file.path(out,"draws.csv"),check.names=FALSE)
support<-read.csv(file.path(out,"support.csv"));checks<-list()
raw<-read.csv(cfg$city_predictors,check.names=FALSE);frozen<-read.csv(file.path(out,"frozen_predictor_pcs.csv"))
loadings<-read.csv(file.path(out,"fixed_pc_loadings.csv"),stringsAsFactors=FALSE)
for(domain in unique(loadings$domain)){
 fields<-loadings$variable[loadings$domain==domain]
 stopifnot(!any(fields %in% c("risk","reclamation_area","area_km2","richness","target_record_n")))
 X<-raw[,fields,drop=FALSE];cc<-complete.cases(X)
 decomposition<-svd(scale(X[cc,,drop=FALSE]))
 scores<-decomposition$u[,1]*decomposition$d[1]
 existing<-frozen[[domain]][cc];orientation<-if(cor(scores,existing)<0)-1 else 1
 stopifnot(identical(is.finite(frozen[[domain]]),cc),max(abs(orientation*scores-existing))<1e-8)
}
nodes<-c("recl","soc","marine","soil","y")
for(nm in unique(edges$case)){
 e<-edges[edges$case==nm,];d<-read.csv(file.path(out,paste0("data_",nm,".csv")),check.names=FALSE)
 for(response in unique(e$response)){
  q<-e[e$response==response,];stopifnot("city_area" %in% q$predictor)
  X<-cbind(1,as.matrix(d[,q$predictor,drop=FALSE]));beta<-drop(solve(crossprod(X),crossprod(X,d[[response]])))[-1]
  stopifnot(max(abs(beta-q$estimate))<1e-8)
 }
 A<-matrix(0,length(nodes),length(nodes),dimnames=list(nodes,nodes))
 for(i in seq_len(nrow(e)))if(e$predictor[i] %in% nodes)A[e$response[i],e$predictor[i]]<-e$estimate[i]
 total<-solve(diag(length(nodes))-A)["y","recl"]
 z<-effects[effects$case==nm,];reported<-z$estimate[z$effect=="total"]
 stopifnot(abs(total-reported)<1e-10,abs(sum(z$estimate[z$effect %in% c("direct","via_soc","via_soil","via_marine_soil","via_soc_soil","via_soc_marine_soil")])-reported)<1e-10)
 b<-draws[draws$case==nm,];ok<-complete.cases(b)
 stopifnot(all(abs(b$total[ok]-b$direct[ok]-b$via_soc[ok]-b$via_soil[ok]-b$via_marine_soil[ok]-b$via_soc_soil[ok]-b$via_soc_marine_soil[ok])<1e-10))
 for(effect in z$effect){ci<-quantile(b[[effect]][ok],c(.025,.975));row<-z[z$effect==effect,];stopifnot(max(abs(ci-c(row$lower,row$upper)))<1e-10)}
 checks[[nm]]<-data.frame(case=nm,n=nrow(d),matrix_total=total,reported_total=reported)
}
binary<-support[support$response_type=="binary"&support$status=="ADAPTED_EXPLORATORY_COMPONENT_SYSTEM",]
if(nrow(binary)){
 stopifnot(nrow(binary)==8L,length(unique(binary$n))==1L)
 keys<-lapply(binary$case,function(nm)sort(read.csv(file.path(out,paste0("data_",nm,".csv")))$key))
 stopifnot(all(vapply(keys,function(k)identical(k,keys[[1]]),logical(1))))
}
write.csv(do.call(rbind,checks),file.path(out,"numerical_verification.csv"),row.names=FALSE)
write_json(list(status="PASS_COMPONENT_PATH_NUMERICAL_QA",cases=length(checks),
 independent_methods=c("Predictor-only SVD PC reconstruction","Normal-equation coefficient refit","Scalar DAG matrix inverse","All six directed path sums","Bootstrap percentile reconstruction"),
 causal_or_original_sem_acceptance=FALSE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_COMPONENT_PATH_NUMERICAL_QA",length(checks),"\n")
