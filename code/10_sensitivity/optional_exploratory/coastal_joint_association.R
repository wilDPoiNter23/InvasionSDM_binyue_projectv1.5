#!/usr/bin/env Rscript
# Compare static suitability within matched coastal cohorts and land uses.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a <- commandArgs(TRUE)
if(length(a)!=1L)stop("Supply a private JSON configuration")
cfg <- fromJSON(a[1],simplifyVector=FALSE)
stopifnot(isTRUE(cfg$period_source_verified),cfg$bootstrap_replicates>=999L,
  cfg$minimum_cities>=20L,cfg$minimum_contrast_periods>=2L)
out <- cfg$association_output
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"joint_association_complete.json")))stop("Existing association retained")
stopifnot(file.exists(file.path(cfg$joint_output,"extraction_complete.json")))
source <- file.path(cfg$joint_output,"joint_native_summary.csv")
d <- read.csv(source,stringsAsFactors=FALSE,check.names=FALSE)
city <- read.csv(file.path(cfg$joint_output,"joint_city_summary.csv"),stringsAsFactors=FALSE,check.names=FALSE)
stopifnot(!anyDuplicated(paste(d$city_id,d$period_endpoint,d$landuse)),all(d$area_km2>0))
set.seed(cfg$seed)
interval <- function(x) {
  x <- x[is.finite(x)]
  if(length(x)<.9*cfg$bootstrap_replicates)return(c(NA_real_,NA_real_))
  as.numeric(quantile(x,c(.025,.975)))
}
slopes <- list(); slope_terms <- list(); contrasts <- list(); contrast_gates <- list(); contrast_terms <- list()
for(scope in c("all_mapped","native_support")) {
  q <- d
  if(scope=="native_support")q <- q[q$native_cells>=3 & q$effective_cells>=2,]
  q$x <- (q$period_endpoint-1990)/5
  q$w <- q$area_km2/ave(q$area_km2,q$city_id,FUN=sum)
  for(response in names(cfg$responses)) {
    q$y <- q[[response]]
    stopifnot(all(is.finite(q$y)))
    # City-by-use intercepts remove both city and within-city use differences.
    terms <- lapply(split(q,paste(q$city_id,q$landuse,sep="|")),function(z) {
      dx <- z$x-weighted.mean(z$x,z$w)
      dy <- z$y-weighted.mean(z$y,z$w)
      data.frame(city_id=z$city_id[1],xx=sum(z$w*dx^2),xy=sum(z$w*dx*dy))
    })
    t <- if(length(terms))aggregate(cbind(xx,xy)~city_id,do.call(rbind,terms),sum) else data.frame(city_id=character(),xx=numeric(),xy=numeric())
    t <- t[t$xx>1e-12,]
    n <- nrow(t)
    beta <- if(n>=cfg$minimum_cities)sum(t$xy)/sum(t$xx) else NA_real_
    b <- if(is.finite(beta))replicate(cfg$bootstrap_replicates,{
      ix <- sample.int(n,n,TRUE);sum(t$xy[ix])/sum(t$xx[ix])
    }) else numeric()
    ci <- interval(b)
    slopes[[paste(scope,response)]] <- data.frame(scope=scope,response=response,n_informative_cities=n,
      n_city_cohort_use=nrow(q),slope_per_later_five_year_cohort=beta,ci_low=ci[1],ci_high=ci[2],
      valid_bootstraps=sum(is.finite(b)),status=if(is.finite(beta)&&all(is.finite(ci)))"EXPLORATORY_WITHIN_CITY_USE_ASSOCIATION" else "INSUFFICIENT_SUPPORT")
    if(n){t$scope<-scope;t$response<-response;slope_terms[[paste(scope,response)]]<-t}
    uses <- sort(unique(q$landuse))
    if(length(uses)<2L)next
    for(pair in combn(uses,2,simplify=FALSE)) {
      left <- q[q$landuse==pair[1],c("city_id","period_endpoint","y","area_km2")]
      right <- q[q$landuse==pair[2],c("city_id","period_endpoint","y","area_km2")]
      m <- merge(left,right,by=c("city_id","period_endpoint"),suffixes=c("_a","_b"))
      if(nrow(m)) {
        m$difference <- m$y_b-m$y_a
        m$pair_area <- pmin(m$area_km2_a,m$area_km2_b)
      }
      p <- if(nrow(m))do.call(rbind,lapply(split(m,m$city_id),function(z) {
        if(nrow(z)<cfg$minimum_contrast_periods)return(NULL)
        data.frame(city_id=z$city_id[1],n_matched_periods=nrow(z),
          difference=weighted.mean(z$difference,z$pair_area))
      })) else NULL
      np <- if(is.null(p))0L else nrow(p)
      key <- paste(scope,response,pair[1],pair[2],sep="|")
      contrast_gates[[key]] <- data.frame(scope=scope,response=response,use_a=pair[1],use_b=pair[2],
        n_matched_cities=np,n_matched_city_periods=nrow(m),
        status=if(np>=cfg$minimum_cities)"SUPPORTED" else "OMITTED_INSUFFICIENT_MATCHED_CITIES")
      if(np<cfg$minimum_cities)next
      boot <- replicate(cfg$bootstrap_replicates,mean(p$difference[sample.int(np,np,TRUE)]))
      ci <- interval(boot)
      contrasts[[key]] <- data.frame(scope=scope,response=response,use_a=pair[1],use_b=pair[2],n_cities=np,
        use_b_minus_use_a=mean(p$difference),ci_low=ci[1],ci_high=ci[2],
        status="EXPLORATORY_MATCHED_CITY_COHORT_CONTRAST")
      p$scope<-scope;p$response<-response;p$use_a<-pair[1];p$use_b<-pair[2];contrast_terms[[key]]<-p
    }
  }
}
write.csv(do.call(rbind,slopes),file.path(out,"within_city_use_cohort_association.csv"),row.names=FALSE)
if(length(slope_terms))write.csv(do.call(rbind,slope_terms),file.path(out,"within_city_use_sufficient_statistics.csv"),row.names=FALSE)
if(length(contrast_gates))write.csv(do.call(rbind,contrast_gates),file.path(out,"use_contrast_support.csv"),row.names=FALSE)
if(length(contrasts))write.csv(do.call(rbind,contrasts),file.path(out,"supported_use_contrasts.csv"),row.names=FALSE)
if(length(contrast_terms))write.csv(do.call(rbind,contrast_terms),file.path(out,"use_contrast_city_source.csv"),row.names=FALSE)
# Between-city adjustment uses union responses and original joint composition.
composition <- aggregate(area_km2~city_id+landuse,d,sum)
composition$fraction <- composition$area_km2/ave(composition$area_km2,composition$city_id,FUN=sum)
uses <- sort(unique(composition$landuse))
reference <- names(which.max(tapply(composition$area_km2,composition$landuse,sum)))
city$cohort <- (city$mean_endpoint-1990)/5
city$log_mapped_area <- log(city$joint_area_km2)
city$log_city_area <- log(city$city_area_km2)
city$lon_sin <- sin(city$longitude*pi/180)
city$lon_cos <- cos(city$longitude*pi/180)
city$latitude_scaled <- city$latitude/90
covariates <- c("log_mapped_area","log_city_area","compactness","lon_sin","lon_cos","latitude_scaled")
for(j in seq_along(uses)) {
  if(uses[j]==reference)next
  name <- paste0("use_fraction_",j)
  z <- composition[composition$landuse==uses[j],]
  city[[name]] <- z$fraction[match(city$city_id,z$city_id)]
  city[[name]][is.na(city[[name]])] <- 0
  covariates <- c(covariates,name)
}
between <- list(); between_gates <- list()
for(scope in c("all_mapped","native_support"))for(response in names(cfg$responses))for(adjustment in c("descriptive","basic","full_composition")) {
  q <- city
  if(scope=="native_support")q <- q[q$native_cells>=3 & q$effective_cells>=2,]
  q$y <- q[[response]]
  q <- q[complete.cases(q[,c("y","cohort",covariates)]),]
  declared <- switch(adjustment,descriptive=character(),basic=covariates[!grepl("^use_fraction_",covariates)],full_composition=covariates)
  active <- declared[vapply(q[declared],function(x)length(unique(x))>1L,logical(1))]
  formula <- reformulate(c("cohort",active),response="y")
  x <- model.matrix(formula,q)
  threshold <- max(30L,5L*ncol(x))
  status <- if(nrow(q)<threshold)"OMITTED_INSUFFICIENT_CITIES_PER_PARAMETER" else
    if(qr(x)$rank<ncol(x))"OMITTED_RANK_DEFICIENT" else "SUPPORTED"
  between_gates[[paste(scope,response,adjustment)]] <- data.frame(scope=scope,response=response,adjustment=adjustment,n_cities=nrow(q),
    parameters=ncol(x),minimum_cities=threshold,status=status,formula=paste(deparse(formula),collapse=" "))
  if(status!="SUPPORTED")next
  fit <- lm.fit(x,q$y)
  estimate <- fit$coefficients["cohort"]
  b <- replicate(cfg$bootstrap_replicates,{
    ix <- sample.int(nrow(q),nrow(q),TRUE)
    xb <- x[ix,,drop=FALSE]
    if(qr(xb)$rank<ncol(xb))NA_real_ else lm.fit(xb,q$y[ix])$coefficients["cohort"]
  })
  ci <- interval(b)
  between[[paste(scope,response,adjustment)]] <- data.frame(scope=scope,response=response,adjustment=adjustment,n_cities=nrow(q),
    slope_per_later_mean_five_year_cohort=estimate,ci_low=ci[1],ci_high=ci[2],valid_bootstraps=sum(is.finite(b)),
    status=if(all(is.finite(ci)))"EXPLORATORY_BETWEEN_CITY_STATIC_ASSOCIATION" else "UNSTABLE_BOOTSTRAP")
}
write.csv(city,file.path(out,"between_city_model_source.csv"),row.names=FALSE)
write.csv(do.call(rbind,between_gates),file.path(out,"between_city_support.csv"),row.names=FALSE)
if(length(between))write.csv(do.call(rbind,between),file.path(out,"between_city_static_association.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"association_sessionInfo.txt"))
write_json(list(status="EXPLORATORY_STATIC_ASSOCIATIONS_ONLY",source_sha256=digest(file=source,algo="sha256"),
  extraction_receipt_sha256=digest(file=file.path(cfg$joint_output,"extraction_complete.json"),algo="sha256"),
  seed=cfg$seed,bootstrap_replicates=cfg$bootstrap_replicates,composition_reference=reference,
  composition_codes=setNames(as.list(uses),paste0("use_fraction_",seq_along(uses))),
  limitations=c("Five-year cohorts are not precise completion dates","Static suitability is not longitudinal invasion",
    "Shared native cells and spatial dependence remain","City bootstrap does not quantify SDM uncertainty",
    "Land use may be a mediator; adjusted contrasts are conditional descriptive comparisons",
    "Between-city contrasts retain unmeasured geographic confounding",
    "Descriptive and basic between-city models retain land-use confounding","Multiple exploratory contrasts have no confirmatory multiplicity control")),
  file.path(out,"joint_association_complete.json"),pretty=TRUE,auto_unbox=TRUE)
