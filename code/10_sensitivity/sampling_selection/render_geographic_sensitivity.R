# Render the country-verified geographic sensitivity from saved endpoint scans.
a<-commandArgs(TRUE);if(length(a)!=2L||a[1]!="--output-root")stop("Use --output-root directory.")
base<-file.path(a[2],"03_sampling_selection")
source_path<-file.path(base,"tables","leave_continent_out_endpoint_scans_geography_verified.csv")
d<-read.csv(source_path);d<-d[d$model=="G0",]
stopifnot(nrow(d)==285L,all(c("Europe","North America","South America")%in%unique(d$excluded_continent)))
write.csv(d,file.path(base,"tables","geographic_endpoint_plot_source_geography_verified.csv"),row.names=FALSE)
suppressPackageStartupMessages(library(ggplot2))
labels<-c(HISTORICAL_MAIN="Archived GAM risk",PAIRED_FULL_684="Paired FULL (684)",PAIRED_ENV_684="Paired ENV (684)")
p<-ggplot(d,aes(city_area_km2,endpoint_difference,colour=excluded_continent))+geom_hline(yintercept=0,colour="grey50")+
  geom_line()+facet_wrap(~response,scales="free_y",labeller=labeller(response=labels))+scale_x_log10()+
  labs(x="City area (km2)",y="Predicted high-minus-low endpoint difference",colour="Excluded continent",
    subtitle="G0 sensitivity with country-verified continent groups",
    caption="Europe exclusion n79; North America n80; South America n79. Conditional endpoint scans may extrapolate; archived response concordance remains unresolved.")+
  theme_bw(base_size=10)+theme(legend.position="bottom")
name<-"geographic_endpoint_scans_geography_verified"
ggsave(file.path(base,"figures",paste0(name,".png")),p,width=11,height=5,dpi=220)
ggsave(file.path(base,"figures",paste0(name,".pdf")),p,width=11,height=5,device="pdf")
cat("COUNTRY_VERIFIED_GEOGRAPHIC_FIGURE_PASS\n")
