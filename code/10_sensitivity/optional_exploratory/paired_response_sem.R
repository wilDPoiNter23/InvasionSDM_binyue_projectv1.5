#!/usr/bin/env Rscript
# Compare responses in an adapted linear component system with fixed predictor PCs.
pc_groups <- function(p)list(anthro_pc1=c("BUILT_S_E2025_2.5min","VIIRS_2023_2.5min","GDP2020_2.5min","Pop2020_2.5min"),
 climate_pc1=grep("^wc2\\.1_2\\.5m_bio_",names(p),value=TRUE),
 soil_pc1=c("bdod_mean_2024_2_5min_aligned","phh2o_mean_2024_2_5min_aligned","soc_mean_2024_2_5min_aligned","sand_mean_2024_2_5min_aligned","silt_mean_2024_2_5min_aligned","ocd_mean_2024_2_5min_aligned","nitrogen_mean_2024_2_5min_aligned","cfvo_mean_2024_2_5min_aligned"),
 marine_pc1=c("salinity_mean","salinity_range","temperature_mean","temperature_range"))
fixed_predictor_pcs <- function(p){
 qc<-list();loadings<-list();scores<-list()
 for(nm in names(pc_groups(p))){
  fields<-pc_groups(p)[[nm]];if(length(fields)<2L||any(!fields %in% names(p)))stop("Raw predictor domain incomplete")
  X<-p[,fields,drop=FALSE];X<-X[,vapply(X,function(v)sd(v,na.rm=TRUE)>1e-8,logical(1)),drop=FALSE]
  cc<-complete.cases(X);fit<-prcomp(scale(X[cc,,drop=FALSE]),center=TRUE,scale.=TRUE)
  score<-rep(NA_real_,nrow(p));score[cc]<-fit$x[,1]
  old<-p[[nm]];ok<-cc&is.finite(old)
  association<-if(sum(ok)>=3L&&sd(old[ok])>0)cor(score[ok],old[ok])else NA_real_
  orientation<-if(is.finite(association)&&association<0)-1 else 1
  difference<-if(any(ok))max(abs(orientation*score[ok]-old[ok]))else Inf
  matches<-identical(is.finite(score),is.finite(old))&&is.finite(difference)&&difference<1e-8
  if(matches){score<-orientation*score;rotation<-orientation*fit$rotation[,1]}else{
   orientation<-if(fit$rotation[which.max(abs(fit$rotation[,1])),1]<0)-1 else 1
   score<-orientation*score;rotation<-orientation*fit$rotation[,1]
  }
  scores[[nm]]<-score;qc[[nm]]<-data.frame(domain=nm,n_complete=sum(cc),maximum_difference=difference,
   status=if(matches)"VERIFIED_EXISTING_UNSUPERVISED_PC"else "ADAPTED_RECOMPUTED_PREDICTOR_ONLY_PC")
  loadings[[nm]]<-data.frame(domain=nm,variable=rownames(fit$rotation),loading=rotation)
 }
 for(nm in names(scores))p[[nm]]<-scores[[nm]]
 list(data=p,qc=do.call(rbind,qc),loadings=do.call(rbind,loadings))
}
component_formulas <- function(intensity=FALSE)list(
 y=if(intensity)y~recl+soc+clim+soil+city_area+intensity else y~recl+soc+clim+soil+city_area,
 marine=marine~recl+soc+clim+city_area,soil=soil~recl+soc+clim+marine+city_area,
 soc=soc~recl+clim+city_area,recl=recl~clim+city_area)
component_fit <- function(d,intensity=FALSE){
 fits <- lapply(component_formulas(intensity),function(f)lm(f,data=d))
 if(any(vapply(fits,function(m)any(!is.finite(coef(m)))||m$rank!=length(coef(m)),logical(1))))stop("Rank or coefficient gate failed")
 edges <- do.call(rbind,lapply(names(fits),function(response){m<-fits[[response]];q<-summary(m)$coefficients
  data.frame(response=response,predictor=rownames(q)[-1],estimate=q[-1,1],se=q[-1,2],p=q[-1,4],row.names=NULL)}))
 b <- function(response,predictor)edges$estimate[edges$response==response&edges$predictor==predictor]
 effects <- c(direct=b("y","recl"),via_soc=b("soc","recl")*b("y","soc"),
  via_soil=b("soil","recl")*b("y","soil"),via_marine_soil=b("marine","recl")*b("soil","marine")*b("y","soil"),
  via_soc_soil=b("soc","recl")*b("soil","soc")*b("y","soil"),
  via_soc_marine_soil=b("soc","recl")*b("marine","soc")*b("soil","marine")*b("y","soil"))
 effects <- c(effects,indirect=sum(effects[-1]),total=sum(effects))
 list(edges=edges,effects=effects,fit=list(n=nrow(d),r2=vapply(fits,function(m)summary(m)$r.squared,numeric(1))))
}
main <- function(){
 suppressPackageStartupMessages({library(jsonlite);library(digest)})
 cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE);out<-cfg$output_root
 species_count<-if(is.null(cfg$expected_species_count))684L else as.integer(cfg$expected_species_count)
 if(length(species_count)!=1L||is.na(species_count)||species_count<1L||species_count>684L)stop("Invalid declared species count")
 complete_status<-if(is.null(cfg$binary_complete_status))paste0("COMPLETE_FIXED_",species_count)else cfg$binary_complete_status
 if(dir.exists(out))stop("Existing component sensitivity retained")
 dir.create(out,recursive=TRUE)
 key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
 p<-read.csv(cfg$city_predictors,stringsAsFactors=FALSE,check.names=FALSE);p$key<-key(p$city)
 stopifnot(nrow(p)==108L,!anyDuplicated(p$key),cfg$minimum_cities>=50L)
 pc<-fixed_predictor_pcs(p);p<-pc$data
 write.csv(pc$qc,file.path(out,"pc_numerical_provenance.csv"),row.names=FALSE)
 write.csv(pc$loadings,file.path(out,"fixed_pc_loadings.csv"),row.names=FALSE)
 write.csv(p[,c("key",names(pc_groups(p)))],file.path(out,"frozen_predictor_pcs.csv"),row.names=FALSE)
 base<-data.frame(key=p$key,city=p$city,recl=log1p(p$reclamation_area),city_area=log1p(p$area_km2),
  soc=p$anthro_pc1,clim=p$climate_pc1,soil=p$soil_pc1,marine=p$marine_pc1)
 if(any(p$reclamation_area<0,na.rm=TRUE)||any(p$area_km2<=0,na.rm=TRUE))stop("Area predictor gate failed")
 cases<-list();receipts<-list()
 if(!is.null(cfg$observed_richness)){
  r<-read.csv(cfg$observed_richness,stringsAsFactors=FALSE)
  r<-r[r$scope=="RAW_SELECTED_INVASIVE"&r$window=="common2010_2024",];r$key<-key(r$city_id)
  if(anyDuplicated(r$key)||!setequal(r$key,p$key))stop("Observed city identity mismatch")
  z<-merge(base,r[,c("key","richness","target_record_n")],by="key",all.x=TRUE)
  z$y<-log1p(z$richness);z$intensity<-log1p(z$target_record_n)
  z<-z[complete.cases(z[,c("recl","city_area","soc","clim","soil","marine","y","intensity")])&z$target_record_n>0,]
  cases$OBSERVED_M0<-list(data=z,intensity=FALSE,type="observed",species_n=NA_integer_)
  cases$OBSERVED_M1<-list(data=z,intensity=TRUE,type="observed",species_n=NA_integer_)
  receipts$observed_sha256<-digest(file=cfg$observed_richness,algo="sha256")
 }
 if(!is.null(cfg$binary_city_responses)){
  r<-read.csv(cfg$binary_city_responses,stringsAsFactors=FALSE);r<-r[r$region=="CITY",];r$key<-key(r$city_id)
  if(anyDuplicated(paste(r$key,r$group,r$threshold))||any(!r$key %in% base$key)||anyNA(r$frozen_species_n)||any(r$frozen_species_n!=species_count))stop("Binary response contract failed: declared species denominator or city identity differs")
  available<-list()
  for(g in c("FULL","ENV"))for(t in c("T10","MAX_TSS")){
   a<-r[r$group==g&r$threshold==t&r$status==complete_status,]
   if(!nrow(a))stop("Missing binary group-threshold case")
   z<-merge(base,a,by="key",all.x=TRUE)
   z$fraction<-z$mean_species_suitable_fraction;z$mean_area<-log1p(z$suitable_species_area_km2/species_count)
   if(any(z$fraction<0|z$fraction>1,na.rm=TRUE)||any(z$suitable_species_area_km2<0,na.rm=TRUE))stop("Binary response value gate failed")
   available[[paste(g,t,sep="_")]]<-z
  }
  common<-Reduce(intersect,lapply(available,function(z)z$key[complete.cases(z[,c("recl","city_area","soc","clim","soil","marine","fraction","mean_area")])]))
  if(!is.null(cfg$expected_binary_cities)&&length(common)!=cfg$expected_binary_cities)stop("Frozen binary city support contract failed")
  write.csv(data.frame(key=sort(common)),file.path(out,"frozen_binary_city_keys.csv"),row.names=FALSE)
  for(nm in names(available))for(endpoint in c("fraction","mean_area")){
   z<-available[[nm]];z<-z[match(sort(common),z$key),];z$y<-z[[endpoint]]
   cases[[paste(nm,endpoint,sep="_")]]<-list(data=z,intensity=FALSE,type="binary",species_n=species_count)
  }
  receipts$binary_sha256<-digest(file=cfg$binary_city_responses,algo="sha256")
  receipts$binary_species_count<-species_count;receipts$binary_complete_status<-complete_status
  if(!is.null(cfg$core_species_roster))receipts$core_species_roster_sha256<-digest(file=cfg$core_species_roster,algo="sha256")
 }
 if(!length(cases))stop("No declared response sources")
 set.seed(cfg$seed);edges<-list();effects<-list();draws<-list();support<-list();datasets<-list();scales<-list()
 for(nm in names(cases)){
  ca<-cases[[nm]];z<-ca$data;state<-"INSUFFICIENT_SUPPORT"
  if(nrow(z)>=cfg$minimum_cities){
   vars<-c("y","recl","soc","clim","soil","marine","city_area",if(ca$intensity)"intensity")
   scale_table<-data.frame(variable=vars,mean=vapply(z[,vars],mean,numeric(1)),sd=vapply(z[,vars],sd,numeric(1)))
   if(any(!is.finite(scale_table$sd)|scale_table$sd<=0))stop("Constant component variable")
   z[,vars]<-scale(z[,vars]);a<-component_fit(z,ca$intensity)
   a$edges$case<-nm;a$edges$frozen_species_n<-ca$species_n;edges[[nm]]<-a$edges
   B<-as.integer(cfg$bootstrap_replicates);if(B<99L)stop("Bootstrap support must include at least 99 replicates")
   boot<-matrix(NA_real_,B,length(a$effects),dimnames=list(NULL,names(a$effects)))
   for(b in seq_len(B)){
    fit<-tryCatch(component_fit(z[sample.int(nrow(z),nrow(z),replace=TRUE),],ca$intensity),error=function(e)NULL)
    if(!is.null(fit))boot[b,]<-fit$effects
   }
   success<-complete.cases(boot);if(mean(success)<.95)stop("Bootstrap convergence support below 95 percent")
   ci<-apply(boot[success,,drop=FALSE],2,quantile,c(.025,.975))
   effects[[nm]]<-data.frame(case=nm,frozen_species_n=ca$species_n,effect=names(a$effects),estimate=as.numeric(a$effects),lower=ci[1,],upper=ci[2,],successful_bootstrap=sum(success),requested_bootstrap=B,row.names=NULL)
   draws[[nm]]<-data.frame(case=nm,replicate=seq_len(B),boot,check.names=FALSE)
   datasets[[nm]]<-data.frame(case=nm,z,check.names=FALSE);scale_table$case<-nm;scales[[nm]]<-scale_table
   state<-"ADAPTED_EXPLORATORY_COMPONENT_SYSTEM"
  }
  support[[nm]]<-data.frame(case=nm,response_type=ca$type,frozen_species_n=ca$species_n,n=nrow(z),status=state,intensity_adjustment=ca$intensity)
 }
 write.csv(do.call(rbind,support),file.path(out,"support.csv"),row.names=FALSE)
 for(item in c("edges","effects","draws","scales"))if(length(get(item)))write.csv(do.call(rbind,get(item)),file.path(out,paste0(item,".csv")),row.names=FALSE)
 # Keep heterogeneous response metadata in separate per-case tables.
 for(nm in names(datasets))write.csv(datasets[[nm]],file.path(out,paste0("data_",nm,".csv")),row.names=FALSE)
 receipts$predictor_sha256<-digest(file=cfg$city_predictors,algo="sha256")
 receipts$configuration_sha256<-digest(file=commandArgs(TRUE)[1],algo="sha256")
 write_json(list(status="ADAPTED_UNSUPERVISED_PC_COMPONENT_SENSITIVITY",inputs=receipts,
  bootstrap="City resampling; original per-case standardization fixed for all replicates",formulas=lapply(component_formulas(),function(f)paste(deparse(f),collapse=" ")),
  path_pruning=FALSE,city_area_in_every_equation=TRUE,causal_interpretation=FALSE,original_sem_reproduction=FALSE,
  binary_species_scope=if(is.null(cfg$binary_city_responses))"NOT_REQUESTED"else if(species_count==684L)"STRICT_PAIRED_684"else "ADAPTED_FIXED_ELIGIBILITY_CORE",
  limitations=c("Fixed city PCs use unsupervised predictor variation; old risk-supervised composite weights are excluded",
   "Full SDM response retains predictor-to-response construction reuse; environmental-only response reduces human-predictor reuse",
   "Observed richness remains conditional on detection and recording; target-record intensity is endogenous",
   "Bootstrap intervals are exploratory and do not correct spatial dependence, confounding or model selection",
   "PCA input and loadings provenance must be verified separately; no causal mediation identification is claimed",
   "A smaller fixed eligibility core changes the taxonomic estimand and does not reproduce the strict 684-species response")),file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE)
 writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
}
if(sys.nframe()==0L)main()
