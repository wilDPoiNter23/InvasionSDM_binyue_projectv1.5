#!/usr/bin/env Rscript
# Compare the existing reclamation GAM definition across city suitability responses.
suppressPackageStartupMessages(library(mgcv))
args <- commandArgs(TRUE)
if(length(args)!=3L) stop('Usage: Rscript fit_city_association.R GAM_INPUT CITY_COMPARISON OUTPUT_DIR')
city_df <- read.csv(args[1],check.names=FALSE)
paired <- read.csv(args[2],check.names=FALSE)
key <- function(x) trimws(gsub('[^a-z0-9]+',' ',tolower(x)))
stopifnot(!anyDuplicated(key(city_df$city)),!anyDuplicated(key(paired$city_id)))
idx <- match(key(city_df$city),key(paired$city_id))
if(anyNA(idx)) stop('Some GAM cities have no matching polygon extraction')
city_df$full_paired <- paired$full_mean[idx]
city_df$env_paired <- paired$env_mean[idx]
city_df$log_recl <- log1p(city_df$reclamation_area)
vars <- c('risk','full_paired','env_paired','log_recl','area_km2','anthro_pc1','climate_pc1','soil_pc1','marine_pc1')
keep <- complete.cases(city_df[,vars])
if(sum(keep)<40) stop('Too few complete matched city observations')
out <- args[3]; dir.create(out,recursive=TRUE,showWarnings=FALSE)
formula <- risk ~ te(log_recl,area_km2,k=c(5,5)) + s(anthro_pc1,k=5) +
  s(climate_pc1,k=5) + s(soil_pc1,k=5) + s(marine_pc1,k=5)
probs <- seq(0.05,0.95,by=0.05)
area <- unname(quantile(city_df$area_km2,probs,na.rm=TRUE))
recl <- seq(min(city_df$log_recl,na.rm=TRUE),max(city_df$log_recl,na.rm=TRUE),length.out=400)
summaries <- scans <- terms <- list()
groups <- c(HISTORICAL_MAIN='risk',FULL_PAIRED='full_paired',ENV_PAIRED='env_paired')
for(group in names(groups)) {
  dat <- city_df[keep,]; dat$risk <- dat[[groups[[group]]]]
  model <- gam(formula,data=dat,method='REML')
  sm <- summary(model)
  summaries[[group]] <- data.frame(response=group,n_cities=nrow(dat),adjusted_R2=sm$r.sq,
                                   deviance_explained=sm$dev.expl,AIC=AIC(model))
  terms[[group]] <- cbind(response=group,term=rownames(sm$s.table),as.data.frame(sm$s.table),row.names=NULL)
  scan <- data.frame(response=group,area_quantile=probs,area_km2=area,endpoint_difference=NA_real_)
  for(i in seq_along(area)) {
    nd <- data.frame(log_recl=recl,area_km2=area[i],
                     anthro_pc1=median(city_df$anthro_pc1,na.rm=TRUE),
                     climate_pc1=median(city_df$climate_pc1,na.rm=TRUE),
                     soil_pc1=median(city_df$soil_pc1,na.rm=TRUE),
                     marine_pc1=median(city_df$marine_pc1,na.rm=TRUE))
    pred <- predict(model,newdata=nd,type='response')
    scan$endpoint_difference[i] <- tail(pred,1)-pred[1]
  }
  scan$direction <- ifelse(scan$endpoint_difference>0,'positive',ifelse(scan$endpoint_difference<0,'negative','zero'))
  scans[[group]] <- scan
}
agreement <- do.call(rbind,lapply(c('HISTORICAL_MAIN','FULL_PAIRED'),function(g) {
  data.frame(reference=g,comparison='ENV_PAIRED',area_quantiles=length(probs),
             matching_directions=sum(scans[[g]]$direction==scans[['ENV_PAIRED']]$direction))
}))
write.csv(do.call(rbind,summaries),file.path(out,'paired_city_GAM_summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,terms),file.path(out,'paired_city_GAM_terms.csv'),row.names=FALSE)
write.csv(do.call(rbind,scans),file.path(out,'paired_city_GAM_area_scan.csv'),row.names=FALSE)
write.csv(agreement,file.path(out,'paired_city_GAM_direction_comparison.csv'),row.names=FALSE)
write.csv(city_df[keep,],file.path(out,'paired_city_GAM_input.csv'),row.names=FALSE)
cat('Compared unchanged GAM definitions on',sum(keep),'matched cities.\n')
