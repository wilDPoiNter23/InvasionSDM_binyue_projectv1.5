# Permutation Moran diagnostics with explicit symmetric neighbourhoods.
run_spatial <- function(residual_table) {
  result <- list(); groups <- split(residual_table,paste(residual_table$response,residual_table$model))
  for (key in names(groups)) {
    z <- groups[[key]]; n <- nrow(z)
    lon <- z$Longitude*pi/180; lat <- z$Latitude*pi/180
    dl <- outer(lon,lon,"-"); da <- outer(lat,lat,"-")
    h <- sin(da/2)^2 + outer(cos(lat),cos(lat))*sin(dl/2)^2
    h[h<0]<-0; h[h>1]<-1
    distance <- 6371.0088*2*asin(sqrt(h))
    diag(distance)<-Inf
    weights <- list()
    for(k in c(4L,8L,12L)) {w<-matrix(0,n,n);for(i in seq_len(n)) w[i,order(distance[i,])[seq_len(k)]]<-1; weights[[paste0("symmetric_knn_",k)]]<-1*((w+t(w))>0)}
    for(radius in c(1000,2500,5000)) weights[[paste0("distance_",radius,"km")]]<-1*(distance<=radius)
    br <- seq(0,as.numeric(quantile(distance[upper.tri(distance)],.6)),length.out=9)
    for(i in 1:8) weights[[paste0("distance_bin_",i)]]<-1*(distance>br[i]&distance<=br[i+1])
    r <- z$residual-mean(z$residual)
    for(name in names(weights)) {
      w <- weights[[name]]; s0 <- sum(w); if(s0==0) next
      statistic <- function(x) n/s0*sum(w*outer(x,x))/sum(x*x)
      obs<-statistic(r); set.seed(seed); perm<-replicate(499,statistic(sample(r)))
      result[[paste(key,name)]]<-data.frame(response=z$response[1],model=z$model[1],n=n,neighbourhood=name,Moran_I=obs,expected_I=-1/(n-1),permutation_p=(1+sum(abs(perm+1/(n-1))>=abs(obs+1/(n-1))))/500,permutations=499,seed=seed,undirected_pairs=s0/2,islands=sum(rowSums(w)==0),distance_lower_km=if(grepl("distance_bin_",name))br[as.integer(sub("distance_bin_","",name))] else NA_real_,distance_upper_km=if(grepl("distance_bin_",name))br[as.integer(sub("distance_bin_","",name))+1] else NA_real_,multiple_testing="unadjusted exploratory diagnostics")
    }
  }
  t<-do.call(rbind,result); write_table(t,"spatial","moran_permutation_results.csv")
  write_table(residual_table,"spatial","residual_map_source.csv")
  ggsave(out("spatial","figures","residual_maps.png"),ggplot(residual_table,aes(Longitude,Latitude,colour=residual))+geom_point(size=2)+facet_grid(response~model)+scale_colour_gradient2()+coord_quickmap()+theme_bw()+labs(colour="Deviance residual"),width=12,height=7,dpi=200)
  write_table(data.frame(module="spatial_diagnostics",status="TECHNICALLY_PASSED",n=84,replaces="Supplementary Figure 8 and Supplementary Table 4 old n83 residual diagnostics",weighting="symmetric binary; not row-standardised",interpretation="association diagnostics; no elimination of sampling bias"),"spatial","module_status.csv","qc")
}
if(sys.nframe()==0L) {
  a<-commandArgs(TRUE)
  if(length(a)!=2L || a[1]!="--output-root") stop("Use --output-root directory.")
  root<-a[2]; seed<-20261014L
  paths<-c(spatial="05_spatial_diagnostics")
  out<-function(module,kind,name) file.path(root,paths[[module]],kind,name)
  write_table<-function(x,module,name,kind="tables") write.csv(x,out(module,kind,name),row.names=FALSE)
  suppressPackageStartupMessages(library(ggplot2))
  run_spatial(read.csv(file.path(root,"02_gam_robustness","tables","residuals_and_influence.csv")))
}
