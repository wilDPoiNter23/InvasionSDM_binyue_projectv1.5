# Render saved city residuals and distance-bin Moran diagnostics without fitting.
a <- commandArgs(TRUE)
if(length(a)!=2L || a[1]!="--output-root") stop("Use --output-root directory.")
base <- file.path(a[2],"05_spatial_diagnostics")
suppressPackageStartupMessages(library(ggplot2))
r <- read.csv(file.path(base,"tables","residual_map_source.csv"))
m <- read.csv(file.path(base,"tables","moran_permutation_results.csv"))
friendly <- c(HISTORICAL_MAIN="Archived\nGAM risk",PAIRED_FULL_684="Paired FULL",PAIRED_ENV_684="Paired ENV")
label_response <- function(x) factor(x,levels=names(friendly),labels=unname(friendly))
r$response_label <- label_response(r$response)
map <- ggplot(r,aes(Longitude,Latitude,colour=residual))+geom_point(size=1.8)+
  facet_grid(response_label~model)+scale_colour_gradient2(low="#296a9e",mid="white",high="#b13a35")+
  coord_fixed(ratio=1.4,xlim=c(-180,180),ylim=c(-60,75))+
  labs(x="Longitude",y="Latitude",colour="Deviance\nresidual")+
  theme_bw(base_size=10)+theme(strip.text.y.right=element_text(angle=0),legend.position="bottom",panel.grid.minor=element_blank())
m <- m[grepl("^distance_bin_",m$neighbourhood),]
stopifnot(nrow(m)==96L,all(is.finite(m$distance_lower_km)),all(is.finite(m$distance_upper_km)))
m$midpoint_km <- (m$distance_lower_km+m$distance_upper_km)/2
m$significance <- ifelse(m$permutation_p<.05,"P < 0.05","P >= 0.05")
write.csv(m,file.path(base,"tables","moran_distance_correlogram_source.csv"),row.names=FALSE)
m$response_label <- label_response(m$response)
cor <- ggplot(m,aes(midpoint_km,Moran_I))+geom_hline(yintercept=0,colour="grey60")+
  geom_line(colour="#30769b")+geom_point(aes(shape=significance),size=2)+
  facet_grid(response_label~model,scales="free_y")+scale_shape_manual(values=c("P < 0.05"=16,"P >= 0.05"=1))+
  labs(x="Distance-bin midpoint (km)",y="Residual Moran's I",shape="Unadjusted permutation P",
    subtitle="Symmetric binary distance-bin weights; 499 permutations per bin",
    caption="New spatial sensitivity method; not a numerical reproduction of the earlier correlogram. Tests are exploratory and unadjusted.")+
  theme_bw(base_size=10)+theme(strip.text.y.right=element_text(angle=0),legend.position="bottom",panel.grid.minor=element_blank())
save_pair <- function(p,name,width,height) {
  png_device <- function(filename,width,height,bg="white",...) grDevices::png(filename,width=width,height=height,units="in",res=220,type="cairo",bg=bg)
  ggsave(file.path(base,"figures",paste0(name,".png")),p,width=width,height=height,dpi=220,device=png_device)
  ggsave(file.path(base,"figures",paste0(name,".pdf")),p,width=width,height=height,device="pdf")
}
save_pair(map,"residual_maps",12,5.4)
save_pair(cor,"moran_distance_correlogram",12,7)
cat("SAVED_TABLE_SPATIAL_FIGURES_PASS\n")
