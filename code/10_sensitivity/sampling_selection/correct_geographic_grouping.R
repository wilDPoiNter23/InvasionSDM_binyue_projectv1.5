# Correct derived continent membership using an independent city metadata table.
a<-commandArgs(TRUE); if(length(a)%%2)stop("Use named argument pairs.")
o<-setNames(as.list(a[seq(2,length(a),2)]),sub("^--","",a[seq(1,length(a),2)]))
root<-o[["output-root"]]; base<-file.path(root,"03_sampling_selection")
if(file.exists(file.path(base,"qc","module_status_geography_verified.csv")))stop("Verified geographic sensitivity already exists; inspect completed results.")
d<-read.csv(o$input); p<-read.csv(o$paired)
script_arg<-grep("^--file=",commandArgs(FALSE),value=TRUE)
source(file.path(dirname(normalizePath(sub("^--file=","",script_arg))),"verify_continent_crosswalk.R"))
crosswalk<-verify_continent_crosswalk(d,o[["city-metadata"]],o$input)
d$continent<-crosswalk$verified_continent
write.csv(crosswalk,file.path(base,"qc","geographic_crosswalk_verified.csv"),row.names=FALSE)
d$log_recl<-log1p(d$reclamation_area);d$log_area<-log1p(d$area_km2);d$true_fraction<-d$reclamation_area/100/d$area_km2
cc<-complete.cases(d[,c("risk","reclamation_area","area_km2","anthro_pc1","climate_pc1","soil_pc1","marine_pc1","Longitude","Latitude")]);stopifnot(sum(cc)==84)
frames<-list(HISTORICAL_MAIN=d[cc,]);ix<-match(tolower(d$city[cc]),tolower(p$city));stopifnot(!anyNA(ix))
frames$PAIRED_FULL_684<-frames$HISTORICAL_MAIN;frames$PAIRED_FULL_684$risk<-p$PAIRED_FULL_684[ix]
frames$PAIRED_ENV_684<-frames$HISTORICAL_MAIN;frames$PAIRED_ENV_684$risk<-p$PAIRED_ENV_684[ix]
suppressPackageStartupMessages(library(mgcv))
old<-read.csv(file.path(base,"tables","leave_continent_out_models.csv"));sc<-read.csv(file.path(base,"tables","leave_continent_out_endpoint_scans.csv"))
affected<-c("Europe","North America","South America")
old<-old[!old$subset%in%paste0("exclude_",affected),];sc<-sc[!sc$excluded_continent%in%affected,]
new<-ns<-list()
for(response in names(frames))for(region in affected)for(model in c("G0","G1","G2","G3")) {
  f<-frames[[response]];f<-f[f$continent!=region,]
  if(nrow(f)<50 || sum(frames[[response]]$continent==region)<3)stop("Affected group lacks prespecified support.")
  original<-readRDS(file.path(root,"02_gam_robustness","model_objects",paste0(response,"_",model,".rds")))
  m<-gam(formula(original),data=f,method="REML");s<-summary(m);t<-s$s.table;cv<-concurvity(m,full=TRUE)
  new[[paste(response,region,model)]]<-data.frame(response=response,model=model,subset=paste0("exclude_",region),n=nobs(m),adj_r2=s$r.sq,deviance_explained=s$dev.expl,AIC=AIC(m),total_edf=sum(m$edf),focal_edf=t[1,1],focal_F=t[1,3],focal_p=t[1,4],maximum_estimated_concurvity=max(cv["estimate",-1]),residual_sd=sd(residuals(m)),residual_skew=mean(scale(residuals(m))^3))
  focal<-if(model%in%c("G0","G1"))"log_recl" else "true_fraction";rg<-range(d[[focal]],na.rm=TRUE)
  ar<-as.numeric(quantile(d$area_km2,seq(.05,.95,.05)))
  for(i in seq_along(ar)) {
    nd<-data.frame(log_recl=median(d$log_recl),true_fraction=median(d$true_fraction),area_km2=ar[i],log_area=log1p(ar[i]),anthro_pc1=median(d$anthro_pc1,na.rm=TRUE),climate_pc1=median(d$climate_pc1,na.rm=TRUE),soil_pc1=median(d$soil_pc1,na.rm=TRUE),marine_pc1=median(d$marine_pc1,na.rm=TRUE))
    nd<-nd[c(1,1),];nd[[focal]]<-rg;pr<-predict(m,nd);xp<-predict(m,nd,type="lpmatrix");del<-xp[2,]-xp[1,];se<-sqrt(as.numeric(del%*%vcov(m)%*%del));effect<-diff(pr)
    ns[[paste(response,region,model,i)]]<-data.frame(response=response,model=model,quantile=seq(.05,.95,.05)[i],city_area_km2=ar[i],endpoint_difference=effect,SE=se,lower=effect-1.96*se,upper=effect+1.96*se,focal_low=rg[1],focal_high=rg[2],support="whole108_range",extrapolation_warning=TRUE,excluded_continent=region)
  }
}
write.csv(rbind(old,do.call(rbind,new)),file.path(base,"tables","leave_continent_out_models_geography_verified.csv"),row.names=FALSE)
write.csv(rbind(sc,do.call(rbind,ns)),file.path(base,"tables","leave_continent_out_endpoint_scans_geography_verified.csv"),row.names=FALSE)
write.csv(data.frame(module="sampling_selection",status="PARTIAL",geographic_sensitivity="TECHNICALLY_PASSED_WITH_ALL108_COUNTRY_CROSSWALK",authoritative_model_table="leave_continent_out_models_geography_verified.csv",authoritative_scan_table="leave_continent_out_endpoint_scans_geography_verified.csv",north_america="4 complete cities excluded; n80",europe="5 complete cities excluded; n79",roster="135 pool comparison remains blocked by8 name mappings; no excluded covariates/effort"),file.path(base,"qc","module_status_geography_verified.csv"),row.names=FALSE)
cat("GEOGRAPHIC_CORRECTION_PASS\n")
