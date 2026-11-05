# Independent verification and figure rendering from saved city results.
a<-commandArgs(TRUE)
if(length(a)!=2L || a[1]!="--output-root") stop("Use --output-root directory.")
root<-a[2]
gam<-file.path(root,"02_gam_robustness")
suppressPackageStartupMessages({library(mgcv);library(ggplot2)})
t<-read.csv(file.path(gam,"tables","model_comparison.csv"))
s<-read.csv(file.path(gam,"tables","endpoint_scan.csv"))
s$extrapolation_warning<-TRUE
write.csv(s,file.path(gam,"tables","endpoint_scan.csv"),row.names=FALSE)
write.csv(data.frame(item=c("G3_support","conditional_scan_support","unit_definition","sampling_roster"),limitation=c("Unique values support estimation; no claim of uniform joint data coverage","Nearest20 area-neighbour range is a restricted descriptive sensitivity; it does not certify joint convex-hull support. All scans retain an extrapolation warning.","True fraction uses ha/100/km2; archived ha/km2 ratio is 100 times the fraction; original G0 remains unchanged","135 named source cities verified, but 8 retained-name mappings remain unresolved; no 108-vs27 claims")),file.path(gam,"qc","scientific_limitations.csv"),row.names=FALSE)
r<-read.csv(file.path(gam,"tables","residuals_and_influence.csv"))
v<-read.csv(file.path(gam,"qc","historical_reproduction.csv"))
b<-read.csv(file.path(gam,"tables","bootstrap_sign_change_intervals.csv"))
checks<-data.frame(check=c("historical_numeric_targets","12_models_n84","all_model_objects","common_city_sample","bootstrap_120_records","finite_residuals","scan_456_records","spatial_n84"),pass=c(all(v$pass),nrow(t)==12&&all(t$n==84),length(list.files(file.path(gam,"model_objects"),pattern="rds$"))==12,length(unique(r$city))==84&&nrow(r)==1008,nrow(b)==120,all(is.finite(r$residual)),nrow(s)==456,all(read.csv(file.path(root,"05_spatial_diagnostics","tables","moran_permutation_results.csv"))$n==84)))
write.csv(checks,file.path(gam,"qc","independent_verification.csv"),row.names=FALSE)
if(!all(checks$pass)) stop("Independent output verification failed.")
bm<-do.call(rbind,lapply(split(b,b$model),function(z)data.frame(model=z$model[1],replicates=nrow(z),successful=sum(z$success),any_flip=sum(z$success&z$flips>0),fraction_any_flip=mean(z$success&z$flips>0),lower_bracket_median=median(z$lower_km2,na.rm=TRUE),upper_bracket_median=median(z$upper_km2,na.rm=TRUE))))
ci<-t(vapply(seq_len(nrow(bm)),function(i)binom.test(bm$any_flip[i],bm$replicates[i])$conf.int,numeric(2)))
bm$flip_fraction_lower<-ci[,1];bm$flip_fraction_upper<-ci[,2]
write.csv(bm,file.path(gam,"tables","bootstrap_summary.csv"),row.names=FALSE)
s$support<-ifelse(s$support=="whole108_range","Whole input range","Local area neighbours")
s$response<-factor(s$response,levels=c("HISTORICAL_MAIN","PAIRED_FULL_684","PAIRED_ENV_684"),labels=c("Archived GAM risk","Paired FULL (684)","Paired ENV (684)"))
response_labeller<-labeller(response=c(HISTORICAL_MAIN="Archived GAM risk",PAIRED_FULL_684="Paired FULL (684)",PAIRED_ENV_684="Paired ENV (684)"))
ggsave(file.path(gam,"figures","endpoint_scan.png"),ggplot(s,aes(city_area_km2,endpoint_difference,colour=model))+geom_hline(yintercept=0,colour="grey50")+geom_line()+facet_grid(response~support,scales="free_y")+scale_x_log10()+labs(x="City area (km2; logarithmic axis)",y="Predicted high-minus-low endpoint difference",colour="Model")+theme_bw(),width=11,height=7,dpi=200)
gp<-file.path(root,"03_sampling_selection","tables","leave_continent_out_endpoint_scans_geography_verified.csv")
if(!file.exists(gp))gp<-file.path(root,"03_sampling_selection","tables","leave_continent_out_endpoint_scans_geography_corrected.csv")
if(!file.exists(gp))gp<-file.path(root,"03_sampling_selection","tables","leave_continent_out_endpoint_scans.csv")
g<-read.csv(gp)
ggsave(file.path(root,"03_sampling_selection","figures","geographic_endpoint_scans.png"),ggplot(g[g$model=="G0",],aes(city_area_km2,endpoint_difference,colour=excluded_continent))+geom_hline(yintercept=0,colour="grey50")+geom_line()+facet_wrap(~response,scales="free_y",labeller=response_labeller)+scale_x_log10()+labs(x="City area (km2)",y="Predicted endpoint difference",colour="Excluded continent")+theme_bw(),width=10,height=5,dpi=200)
ggsave(file.path(root,"03_sampling_selection","figures","geographic_endpoint_scans.pdf"),last_plot(),width=10,height=5)
plots<-list(
  endpoint_scan=ggplot(s,aes(city_area_km2,endpoint_difference,colour=model))+geom_hline(yintercept=0,colour="grey50")+geom_line()+facet_grid(response~support,scales="free_y")+scale_x_log10()+labs(x="City area (km2; logarithmic axis)",y="Predicted high-minus-low endpoint difference",colour="Model")+theme_bw(),
  residual_qq=ggplot(r,aes(sample=residual))+stat_qq()+stat_qq_line()+facet_grid(response~model,scales="free",labeller=response_labeller)+theme_bw()+labs(x="Normal theoretical quantile",y="Deviance residual quantile"),
  model_comparison=ggplot(t,aes(model,AIC,colour=model))+geom_point(size=3)+facet_wrap(~response,scales="free_y",labeller=response_labeller)+theme_bw()+labs(x="Pre-specified model",y="AIC (compare within response)"),
  bootstrap_stability=ggplot(bm,aes(model,fraction_any_flip))+geom_col(fill="#30769b")+geom_errorbar(aes(ymin=flip_fraction_lower,ymax=flip_fraction_upper),width=.15)+scale_y_continuous(limits=c(0,1))+theme_bw()+labs(x="Archived GAM response model",y="Fraction of 30 resamples with an endpoint-sign flip",caption="Bars describe bootstrap stability; error bars reflect finite-resample precision."))
inf<-read.csv(file.path(gam,"tables","highest_influence_cities.csv"));inf<-inf[inf$model=="G0",]
plots$influence<-ggplot(inf,aes(reorder(city,influence_score),influence_score))+geom_col(fill="#30769b")+coord_flip()+facet_wrap(~response,scales="free",labeller=response_labeller)+theme_bw()+labs(x="City",y="Residual-leverage influence score")
for(nm in names(plots))for(ext in c("png","pdf"))ggsave(file.path(gam,"figures",paste0(nm,".",ext)),plots[[nm]],width=11,height=7,dpi=200)
mr<-read.csv(file.path(root,"05_spatial_diagnostics","tables","moran_permutation_results.csv"))
write.csv(data.frame(method="symmetric binary weights with 499 fixed-seed permutation tests",scope="new sensitivity method, not numerical reproduction of old correlograms",multiplicity="unadjusted exploratory tests",supersedes="old n83 Supplementary Figure8 and Table4"),file.path(root,"05_spatial_diagnostics","qc","method_comparison.csv"),row.names=FALSE)
receipt<-read.csv(file.path(gam,"qc","input_receipt.csv"));stopifnot(unname(tools::md5sum(receipt$input))==receipt$md5)
d<-read.csv(receipt$input);curves<-list()
for(response in unique(t$response)) {
  m<-readRDS(file.path(gam,"model_objects",paste0(response,"_G0.rds")))
  for(q in c(.1,.25,.5,.75,.9)) {
    nd<-data.frame(log_recl=seq(min(log1p(d$reclamation_area)),max(log1p(d$reclamation_area)),length.out=400),area_km2=as.numeric(quantile(d$area_km2,q)),anthro_pc1=median(d$anthro_pc1,na.rm=TRUE),climate_pc1=median(d$climate_pc1,na.rm=TRUE),soil_pc1=median(d$soil_pc1,na.rm=TRUE),marine_pc1=median(d$marine_pc1,na.rm=TRUE))
    pr<-predict(m,nd,se.fit=TRUE,type="response")
    curves[[paste(response,q)]]<-data.frame(response=response,area_quantile=q,city_area_km2=nd$area_km2,reclamation_km2=expm1(nd$log_recl)/100,fitted=pr$fit,lower=pr$fit-1.96*pr$se.fit,upper=pr$fit+1.96*pr$se.fit,extrapolation_warning=TRUE)
  }
}
cu<-do.call(rbind,curves);write.csv(cu,file.path(gam,"tables","G0_conditional_curve_source.csv"),row.names=FALSE)
cp<-ggplot(cu,aes(reclamation_km2,fitted,colour=factor(area_quantile)))+geom_line()+facet_wrap(~response,scales="free_y",labeller=response_labeller)+scale_x_log10()+theme_bw()+labs(x="Reclamation area (km2; logarithmic axis)",y="Conditional fitted suitability",colour="City area quantile",caption="Whole-input-range curves may extrapolate. Archived risk differs from fresh polygon extraction; source concordance remains unresolved.")
for(ext in c("png","pdf"))ggsave(file.path(gam,"figures",paste0("G0_conditional_curves.",ext)),cp,width=11,height=5,dpi=200)
write.csv(data.frame(module="gam_robustness",status="TECHNICALLY_PASSED",responses=3,models=12,scientific_acceptance="not established"),file.path(gam,"qc","module_status.csv"),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(gam,"qc","sessionInfo.txt"))
cat("INDEPENDENT_VERIFICATION_PASS\n")
