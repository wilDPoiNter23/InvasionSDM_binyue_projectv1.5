#!/usr/bin/env Rscript
# Describe city-clipped mapped reclamation configuration using cached cohort geometry.
configuration_metrics <- function(areas_ha,cutoff){
 retained<-areas_ha[areas_ha>=cutoff];A<-sum(retained);total<-sum(areas_ha)
 data.frame(cutoff_ha=cutoff,n_retained=length(retained),total_mapped_area_ha=total,retained_area_ha=A,
  retained_area_fraction=if(total>0)A/total else NA_real_,largest_patch_fraction=if(A>0)max(retained)/A else NA_real_,
  effective_patch_number=if(A>0)A^2/sum(retained^2)else NA_real_,
  support=if(A>0)"MAPPED_COMPONENTS_RETAINED"else "NO_COMPONENT_ABOVE_ANALYST_CUTOFF")
}
configuration_associations <- function(metrics,cfg,out,canonical_city_ids){
 if(is.null(cfg$observed_residual_root))return(NULL)
 key<-function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
 results<-list();samples<-list()
 for(model in c("M0","M1")){
  d<-read.csv(file.path(cfg$observed_residual_root,paste0("group_data_",model,".csv")),stringsAsFactors=FALSE)
  stopifnot(all(c("key","city","residual","city_area","recl","richness","intensity") %in% names(d)),!anyDuplicated(d$key),
   all(d$key==key(d$city)),all(d$key %in% key(canonical_city_ids)))
  controls<-c("city_area",if(model=="M1")"intensity");C<-cbind(1,as.matrix(d[,controls,drop=FALSE]))
  correct_residual<-d$richness-drop(C%*%qr.solve(C,d$richness))
  if(max(abs(correct_residual-d$residual))>1e-8)stop("Response must be area-effort-only Gaussian residual, not a count residual adjusted for focal reclamation")
  for(cutoff in c(1,10,100))for(metric in c("n_retained","effective_patch_number","largest_patch_fraction")){
   m<-metrics[metrics$cutoff_ha==cutoff,];m$key<-key(m$city_id)
   if(anyDuplicated(m$key)||any(!m$key %in% key(canonical_city_ids)))stop("Configuration or observed city identity differs")
   a<-merge(m,d,by="key",all.x=TRUE);a$x<-if(metric=="largest_patch_fraction")a[[metric]]else log1p(a[[metric]])
   # Zero retained area supplies no configuration index; it is not a biological absence.
   a<-a[which(a$retained_area_ha>0&is.finite(a$x)&is.finite(a$residual)&is.finite(a$city_area)&is.finite(a$recl)),]
   row<-data.frame(model=model,cutoff_ha=cutoff,metric=metric,n=nrow(a),status="FITTED",estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,p=NA_real_)
   if(nrow(a)<cfg$minimum_association_cities)row$status<-"INSUFFICIENT_CITY_SUPPORT"else if(sd(a$x)==0||sd(a$residual)==0)row$status<-"CONSTANT_VARIABLE"else{
    X<-cbind(intercept=1,configuration=as.numeric(scale(a$x)),city_area=a$city_area,reclamation_amount=a$recl);y<-as.numeric(scale(a$residual))
    fit<-lm.fit(X,y)
    if(fit$rank!=ncol(X))row$status<-"RANK_DEFICIENT"else{
     inverse<-solve(crossprod(X));h<-rowSums((X%*%inverse)*X)
     if(any(h>=1-1e-10))stop("HC3 leverage gate failed")
     covariance<-inverse%*%crossprod(X,X*(fit$residuals/(1-h))^2)%*%inverse
     se<-sqrt(covariance[2,2]);estimate<-fit$coefficients[2];df<-nrow(a)-ncol(X)
     row$estimate<-estimate;row$se<-se;row$lower<-estimate-qt(.975,df)*se;row$upper<-estimate+qt(.975,df)*se;row$p<-2*pt(-abs(estimate/se),df)
    }
   }
   results[[length(results)+1L]]<-row
   samples[[length(samples)+1L]]<-data.frame(model=rep(model,nrow(a)),cutoff_ha=rep(cutoff,nrow(a)),metric=rep(metric,nrow(a)),key=a$key,x=a$x,
    residual=a$residual,city_area=a$city_area,recl=a$recl)
  }
 }
 r<-do.call(rbind,results);r$p_bh<-NA_real_
 for(model in c("M0","M1")){ix<-which(r$model==model);r$p_bh[ix]<-p.adjust(r$p[ix],"BH",n=9L)}
 write.csv(r,file.path(out,"associations.csv"),row.names=FALSE,na="")
 write.csv(do.call(rbind,samples),file.path(out,"association_samples.csv"),row.names=FALSE)
 r
}
configuration_main <- function(){
 suppressPackageStartupMessages({library(sf);library(jsonlite);library(digest)})
 cfg<-fromJSON(commandArgs(TRUE)[1],simplifyVector=TRUE);out<-cfg$output_root
 if(dir.exists(out))stop("Fresh configuration output root required")
 dir.create(out,recursive=TRUE);dir.create(file.path(out,"components"));sf_use_s2(FALSE)
 cities<-st_transform(st_make_valid(st_read(cfg$cities,quiet=TRUE)),6933);cities$city_id<-as.character(cities[[cfg$city_id]])
 stopifnot(nrow(cities)==cfg$expected_cities,!anyDuplicated(cities$city_id))
 coverage<-read.csv(cfg$coverage_table,stringsAsFactors=FALSE);coverage<-coverage[coverage$layer=="coastal_change",]
 stopifnot(!anyDuplicated(coverage$city_id),setequal(coverage$city_id,cities$city_id))
 bad<-coverage$city_id[!is.finite(coverage$mapped_area_km2)|!is.finite(coverage$overlap_excess_km2)|coverage$overlap_excess_km2>pmax(.001,.001*coverage$mapped_area_km2)]
 result<-list();gates<-list();components<-list();sources<-list()
 for(i in seq_len(nrow(cities))){
  city<-cities$city_id[i];path<-file.path(cfg$cohort_checkpoint_root,sprintf("city_%03d.rds",i))
  gate<-tryCatch({
   if(city %in% bad)stop("COHORT_GEOMETRY_OVERLAP_GATE")
   if(!file.exists(path))stop("NO_MAPPED_COHORT_CHECKPOINT")
   z<-readRDS(path)
   if(is.null(z)||!nrow(z)||!all(z$city_id==city))stop("EMPTY_OR_MISMATCHED_COHORT")
   endpoint<-suppressWarnings(as.numeric(as.character(z$reported_category)))
   if(length(endpoint)!=nrow(z)||anyNA(endpoint)||any(!endpoint %in% cfg$reported_cohort_endpoints))stop("UNEXPECTED_COHORT_ENDPOINT")
   # Period boundaries are dissolved before connected polygon parts are counted.
   q<-st_union(st_geometry(st_transform(st_make_valid(z),6933)))
   q<-suppressWarnings(st_intersection(q,st_geometry(cities[i,])))
   q<-st_make_valid(q);q<-suppressWarnings(st_collection_extract(q,"POLYGON"))
   if(!length(q)||all(st_is_empty(q)))stop("NO_MAPPED_POLYGON_SUPPORT")
   parts<-suppressWarnings(st_cast(st_union(q),"POLYGON"));parts<-parts[!st_is_empty(parts)]
   areas<-as.numeric(st_area(parts))/10000
   if(any(!is.finite(areas)|areas<=0)||any(!st_is_valid(parts)))stop("COMPONENT_VALIDITY_GATE")
   A<-sum(as.numeric(st_area(q)))/10000;error<-abs(sum(areas)-A)/A
   if(!is.finite(error)||error>1e-8)stop("POLYGON_AREA_CONSERVATION_GATE")
   boundary<-lengths(st_intersects(parts,st_boundary(st_geometry(cities[i,]))))>0
   index<-data.frame(city_id=city,component=seq_along(areas),area_ha=areas,touches_city_boundary=boundary)
   components[[city]]<-index
   saveRDS(st_sf(index,geometry=parts),file.path(out,"components",sprintf("city_%03d.rds",i)))
   rows<-do.call(rbind,lapply(c(1,10,100),function(cutoff){row<-configuration_metrics(areas,cutoff);row$city_id<-city
    row$retained_boundary_components<-sum(boundary&areas>=cutoff);row}))
   result[[city]]<-rows;sources[[city]]<-data.frame(city_id=city,file=path,sha256=digest(file=path,algo="sha256"))
   data.frame(city_id=city,status="CONFIGURATION_AVAILABLE",reason="",component_n=length(areas),relative_area_error=error)
  },error=function(e)data.frame(city_id=city,status="EXCLUDED_GEOMETRY",reason=conditionMessage(e),component_n=NA_integer_,relative_area_error=NA_real_))
  gates[[i]]<-gate;cat(city,gate$status,"\n");flush.console()
 }
 write.csv(do.call(rbind,gates),file.path(out,"city_geometry_status.csv"),row.names=FALSE)
 if(!length(result))stop("No eligible mapped reclamation geometry")
 metrics<-do.call(rbind,result);write.csv(metrics,file.path(out,"configuration.csv"),row.names=FALSE,na="")
 write.csv(do.call(rbind,components),file.path(out,"component_areas.csv"),row.names=FALSE)
 write.csv(do.call(rbind,sources),file.path(out,"cohort_source_checksums.csv"),row.names=FALSE)
 association<-configuration_associations(metrics,cfg,out,cities$city_id)
 sidecars<-if(tolower(tools::file_ext(cfg$cities))=="shp")list.files(dirname(cfg$cities),
  pattern=paste0("^",tools::file_path_sans_ext(basename(cfg$cities)),"[.]"),full.names=TRUE)else character()
 inputs<-unique(c(cfg$cities,sidecars,cfg$coverage_table,if(!is.null(cfg$observed_residual_root))file.path(cfg$observed_residual_root,paste0("group_data_",c("M0","M1"),".csv"))))
 write.csv(data.frame(file=inputs,sha256=vapply(inputs,function(p)digest(file=p,algo="sha256"),character(1))),file.path(out,"input_checksums.csv"),row.names=FALSE)
 write_json(list(status="DESCRIPTIVE_MAPPED_RECLAMATION_CONFIGURATION",configuration_sha256=digest(file=commandArgs(TRUE)[1],algo="sha256"),
  equal_area_epsg=6933,cutoffs_ha=c(1,10,100),cutoff_type="Analyst size-sensitivity thresholds; source minimum mapping unit unverified",
  nominal_source_imagery_resolution_m=30,geometry="City-clipped union of all reported periods; GEOS POLYGON parts; polygon holes retained",
  largest_fraction="Largest retained component divided by retained total area",effective_number="Retained total area squared divided by sum of squared retained component areas",
  no_retained_support="Configuration ratios missing, not biological absence",cohort_endpoints=cfg$reported_cohort_endpoints,
  association=if(is.null(association))"NOT_REQUESTED"else "Exploratory standardized Gaussian residual association with log city area and log canonical reclamation amount controls; HC3 intervals",
  habitat_fragmentation_inferred=FALSE,causal_interpretation=FALSE,
  limitations=c("Mapped components are not verified habitat patches or separate reclamation projects",
   "City boundaries clip components and may change their areas or connectivity; boundary-touching parts are flagged",
   "Majority filtering, unknown minimum mapping unit and export generalization may alter small components",
   "Source geometry spans1985-2020; canonical city reclamation amount may have a different time or footprint definition",
   "Residual association intervals condition on preceding area-effort baselines; spatial dependence and causal confounding remain")),file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE)
 writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
}
if(sys.nframe()==0L)configuration_main()
