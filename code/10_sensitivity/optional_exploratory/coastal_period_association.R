#!/usr/bin/env Rscript
# Compare suitability across reported five-year cohorts within cities.
suppressPackageStartupMessages({library(jsonlite); library(digest)})
a <- commandArgs(TRUE)
if(length(a)!=1L) stop("Supply a private JSON configuration")
cfg <- fromJSON(a[1],simplifyVector=FALSE)
stopifnot(isTRUE(cfg$period_source_verified))
out <- cfg$association_output
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"association_complete.json"))) stop("Existing association retained")
source <- file.path(cfg$screen_output,"native_cell_category_suitability.csv")
d <- read.csv(source)
d <- d[d$layer=="coastal_change" & is.finite(d$mean_suitability),]
d$period_endpoint <- as.numeric(d$reported_category)
stopifnot(all(d$period_endpoint %in% seq(1990,2020,5)))
results <- list(); city_terms <- list(); pair_results <- list(); pairs <- list()
set.seed(cfg$seed)
for(response in unique(d$response)) for(scope in c("all_mapped","native_support")) {
  q <- d[d$response==response,]
  if(scope=="native_support") q <- q[q$effective_cells>=2 & q$native_cells>=3,]
  eligible <- names(which(table(q$city_id)>=2))
  q <- q[q$city_id %in% eligible,]
  terms <- lapply(split(q,q$city_id),function(z) {
    w <- z$mapped_area_km2/sum(z$mapped_area_km2)
    x <- (z$period_endpoint-1990)/5; y <- z$mean_suitability
    dx <- x-sum(w*x); dy <- y-sum(w*y)
    data.frame(city_id=z$city_id[1],n_periods=nrow(z),xy=sum(w*dx*dy),xx=sum(w*dx^2))
  })
  t <- if(length(terms))do.call(rbind,terms) else data.frame()
  n <- nrow(t); reportable <- n>=20
  if(n) {t$response <- response;t$scope <- scope;city_terms[[paste(response,scope)]] <- t}
  beta <- if(n && sum(t$xx)>0)sum(t$xy)/sum(t$xx) else NA_real_
  boot <- if(reportable) replicate(cfg$bootstrap_replicates,{
    ix <- sample.int(n,n,replace=TRUE); sum(t$xy[ix])/sum(t$xx[ix])
  }) else numeric()
  ci <- if(length(boot))quantile(boot,c(.025,.975),na.rm=TRUE) else c(NA,NA)
  results[[paste(response,scope)]] <- data.frame(response=response,scope=scope,n_cities=n,
    n_city_periods=nrow(q),slope_per_five_year_cohort=beta,ci_low=ci[1],ci_high=ci[2],
    bootstrap_replicates=length(boot),status=if(reportable)"EXPLORATORY_ASSOCIATION" else "INSUFFICIENT_CITY_SUPPORT")
  pair <- lapply(split(q,q$city_id),function(z) {
    old <- z[z$period_endpoint<=2000,]; recent <- z[z$period_endpoint>=2015,]
    if(!nrow(old)||!nrow(recent))return(NULL)
    old_mean <- weighted.mean(old$mean_suitability,old$mapped_area_km2)
    recent_mean <- weighted.mean(recent$mean_suitability,recent$mapped_area_km2)
    data.frame(city_id=z$city_id[1],old_mean=old_mean,recent_mean=recent_mean,
      recent_minus_old=recent_mean-old_mean)
  })
  p <- if(length(Filter(Negate(is.null),pair)))do.call(rbind,pair) else data.frame()
  np <- nrow(p)
  bp <- if(np>=20)replicate(cfg$bootstrap_replicates,mean(p$recent_minus_old[sample.int(np,np,TRUE)])) else numeric()
  pc <- if(length(bp))quantile(bp,c(.025,.975)) else c(NA,NA)
  pair_results[[paste(response,scope)]] <- data.frame(response=response,scope=scope,n_cities=np,
    recent_minus_old=if(np)mean(p$recent_minus_old) else NA_real_,ci_low=pc[1],ci_high=pc[2],
    status=if(np>=20)"EXPLORATORY_PAIRED_CONTRAST" else "INSUFFICIENT_CITY_SUPPORT")
  if(np){p$response<-response;p$scope<-scope;pairs[[paste(response,scope)]]<-p}
}
write.csv(do.call(rbind,results),file.path(out,"period_association.csv"),row.names=FALSE)
write.csv(do.call(rbind,pair_results),file.path(out,"old_recent_contrast.csv"),row.names=FALSE)
if(length(city_terms))write.csv(do.call(rbind,city_terms),file.path(out,"within_city_sufficient_statistics.csv"),row.names=FALSE)
if(length(pairs))write.csv(do.call(rbind,pairs),file.path(out,"old_recent_city_source.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
write_json(list(status="COHORT_ASSOCIATIONS_ONLY",source_sha256=digest(file=source,algo="sha256"),
  precise_completion_dates=FALSE,longitudinal_invasion_response=FALSE,
  caveat="Static modelled suitability; five-year source cohorts; land-use confounding and shared native cells remain"),
  file.path(out,"association_complete.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Within-city cohort associations calculated with city bootstrap.\n")
