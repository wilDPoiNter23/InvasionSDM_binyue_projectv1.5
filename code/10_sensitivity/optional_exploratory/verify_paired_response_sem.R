#!/usr/bin/env Rscript
# Refit saved equations and independently verify DAG total effects by matrix inversion.
suppressPackageStartupMessages(library(jsonlite))
cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
if(file.exists(file.path(out,"verification.json")))stop("Existing verification retained")
edges<-read.csv(file.path(out,"edges.csv"));effects<-read.csv(file.path(out,"effects.csv"));draws<-read.csv(file.path(out,"draws.csv"),check.names=FALSE)
support<-read.csv(file.path(out,"support.csv"));checks<-list()
species_count<-if(is.null(cfg$expected_species_count))684L else as.integer(cfg$expected_species_count)
scales<-read.csv(file.path(out,"scales.csv"))
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
set.seed(cfg$seed)
for(nm in unique(edges$case)){
 e<-edges[edges$case==nm,];d<-read.csv(file.path(out,paste0("data_",nm,".csv")),check.names=FALSE)
 if(support$response_type[support$case==nm]=="binary"){
  stopifnot(all(e$frozen_species_n==species_count),all(d$frozen_species_n==species_count))
  scale_y<-scales[scales$case==nm&scales$variable=="y",]
  raw_y<-d$y*scale_y$sd+scale_y$mean
  expected_y<-if(grepl("_mean_area$",nm))log1p(d$suitable_species_area_km2/species_count)else d$mean_species_suitable_fraction
  stopifnot(max(abs(raw_y-expected_y))<1e-10,all(effects$frozen_species_n[effects$case==nm]==species_count))
 }
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
 # Consume the same city-index draw stream and independently refit selected draws.
 for(replicate in seq_len(nrow(b))){
  rows<-sample.int(nrow(d),nrow(d),replace=TRUE)
  if(replicate<=3L){
   sampled_edges<-e
   for(response in unique(e$response)){
    q<-e[e$response==response,];X<-cbind(1,as.matrix(d[rows,q$predictor,drop=FALSE]))
    beta<-drop(qr.solve(X,d[[response]][rows]))[-1]
    sampled_edges$estimate[sampled_edges$response==response]<-beta
   }
   edge<-function(response,predictor)sampled_edges$estimate[sampled_edges$response==response&sampled_edges$predictor==predictor]
   expected<-c(direct=edge("y","recl"),via_soc=edge("soc","recl")*edge("y","soc"),
    via_soil=edge("soil","recl")*edge("y","soil"),via_marine_soil=edge("marine","recl")*edge("soil","marine")*edge("y","soil"),
    via_soc_soil=edge("soc","recl")*edge("soil","soc")*edge("y","soil"),
    via_soc_marine_soil=edge("soc","recl")*edge("marine","soc")*edge("soil","marine")*edge("y","soil"))
   expected<-c(expected,indirect=sum(expected[-1]),total=sum(expected))
   stopifnot(max(abs(as.numeric(b[replicate,names(expected)])-expected))<1e-10)
  }
 }
 stopifnot(all(abs(b$total[ok]-b$direct[ok]-b$via_soc[ok]-b$via_soil[ok]-b$via_marine_soil[ok]-b$via_soc_soil[ok]-b$via_soc_marine_soil[ok])<1e-10))
 for(effect in z$effect){ci<-quantile(b[[effect]][ok],c(.025,.975));row<-z[z$effect==effect,];stopifnot(max(abs(ci-c(row$lower,row$upper)))<1e-10)}
 checks[[nm]]<-data.frame(case=nm,n=nrow(d),matrix_total=total,reported_total=reported)
}
binary<-support[support$response_type=="binary"&support$status=="ADAPTED_EXPLORATORY_COMPONENT_SYSTEM",]
if(nrow(binary)){
 stopifnot(nrow(binary)==8L,length(unique(binary$n))==1L,all(binary$frozen_species_n==species_count))
 keys<-lapply(binary$case,function(nm)sort(read.csv(file.path(out,paste0("data_",nm,".csv")))$key))
 stopifnot(all(vapply(keys,function(k)identical(k,keys[[1]]),logical(1))))
}
write.csv(do.call(rbind,checks),file.path(out,"numerical_verification.csv"),row.names=FALSE)
write_json(list(status="PASS_COMPONENT_PATH_NUMERICAL_QA",cases=length(checks),
 independent_methods=c("Predictor-only SVD PC reconstruction","Normal-equation coefficient refit","Scalar DAG matrix inverse","All six directed path sums","Bootstrap percentile reconstruction","Seeded city-index stream with independent QR refits for first three replicates per case"),
 binary_species_count=if(nrow(binary))species_count else NULL,binary_denominator_and_response_transform_checked=TRUE,
 causal_or_original_sem_acceptance=FALSE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("PASS_COMPONENT_PATH_NUMERICAL_QA",length(checks),"\n")
