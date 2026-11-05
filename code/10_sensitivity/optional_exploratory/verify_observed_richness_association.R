#!/usr/bin/env Rscript
# Check saved count likelihoods, stationary points and curvature independently.
suppressPackageStartupMessages(library(jsonlite))
cfg <- fromJSON(commandArgs(TRUE)[1],simplifyVector=FALSE)
directory <- if(is.null(cfg$association_directory))"association"else cfg$association_directory
if(length(directory)!=1L||!grepl("^[A-Za-z0-9_-]+$",directory))stop("Invalid association directory name")
out <- file.path(cfg$output_root,directory)
receipt <- file.path(out,"numerical_verification.json")
if(file.exists(receipt))stop("Existing verification retained")
if(!file.exists(file.path(out,"models.csv")))stop("No fitted models to verify")
m <- read.csv(file.path(out,"models.csv"));c <- read.csv(file.path(out,"coefficients.csv"))
if(!is.null(cfg$expected_models) && nrow(m)!=cfg$expected_models)stop("Fitted model count differs from declared numerical verification scope")
d <- read.csv(file.path(out,"city_analysis_data.csv"));stored <- read.csv(file.path(out,"residuals.csv"))
crosswalk <- read.csv(file.path(out,"city_identity_crosswalk.csv"),stringsAsFactors=FALSE)
predictors <- read.csv(cfg$city_predictors,stringsAsFactors=FALSE,check.names=FALSE)
normalize <- function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
stopifnot(nrow(crosswalk)==cfg$expected_city_count,!anyDuplicated(crosswalk$city_id),!anyDuplicated(crosswalk$normalized_city_key),
 !anyDuplicated(crosswalk$predictor_city),setequal(crosswalk$predictor_city,predictors[[cfg$predictor_city_field]]),
 all(normalize(crosswalk$city_id)==normalize(crosswalk$predictor_city)),all(crosswalk$normalized_city_key==normalize(crosswalk$city_id)),
 setequal(unique(d$city_id),crosswalk$city_id))
for(i in seq_len(nrow(crosswalk))){
 original <- predictors[predictors[[cfg$predictor_city_field]]==crosswalk$predictor_city[i],,drop=FALSE]
 bound <- d[d$city_id==crosswalk$city_id[i],,drop=FALSE]
 for(field in c("reclamation_area","area_km2"))stopifnot(all(bound[[field]]==original[[field]]|is.na(bound[[field]])&is.na(original[[field]])))
}
checks <- list()
for(i in seq_len(nrow(m))){
 a <- m[i,];co <- c[c$scope==a$scope&c$window==a$window&c$model==a$model,,drop=FALSE]
 city <- stored$city_id[stored$scope==a$scope&stored$window==a$window&stored$model==a$model]
 z <- d[d$scope==a$scope&d$window==a$window&d$city_id %in% city,,drop=FALSE]
 terms <- co$term[co$term!="(Intercept)"]
 X <- cbind(1,as.matrix(z[,terms,drop=FALSE]));y <- z$richness;k <- ncol(X)
 b <- co$estimate
 nb <- a$family=="zero_truncated_negative_binomial"
 if(nb)b <- c(b,log(a$theta))
 # Calculate truncation normalizers separately from the fitted implementation.
 objective <- function(par){
  mu <- exp(drop(X%*%par[seq_len(k)]))
  if(nb){theta <- exp(par[k+1L]);logp0 <- -theta*log1p(mu/theta)
   lp <- lgamma(y+theta)-lgamma(theta)-lgamma(y+1)+theta*(log(theta)-log(theta+mu))+y*(log(mu)-log(theta+mu))
  }else{logp0 <- -mu;lp <- y*log(mu)-mu-lgamma(y+1)}
  -sum(lp-log(-expm1(logp0)))
 }
 h <- 1e-4;p <- length(b);H <- matrix(0,p,p);g <- numeric(p)
 for(j in seq_len(p)){
  e <- rep(0,p);e[j] <- h
  g[j] <- (objective(b+e)-objective(b-e))/(2*h)
  for(l in seq_len(p)){
   f <- rep(0,p);f[l] <- h
   H[j,l] <- (objective(b+e+f)-objective(b+e-f)-objective(b-e+f)+objective(b-e-f))/(4*h*h)
  }
 }
 aic_error <- abs(2*objective(b)+2*p-a$aic)
 curvature <- min(eigen(H,symmetric=TRUE,only.values=TRUE)$values)
 if(aic_error>=1e-6||max(abs(g))>=.01||curvature<=0||nrow(z)!=a$n)stop(paste("Numerical gate failed",a$scope,a$model,"AIC",aic_error,"score",max(abs(g)),"curvature",curvature))
 se_error <- max(abs(sqrt(diag(solve(H)))[seq_len(k)]-co$se))
 stopifnot(se_error<.01)
 checks[[i]] <- data.frame(scope=a$scope,window=a$window,model=a$model,aic_error=aic_error,
  maximum_gradient=max(abs(g)),minimum_curvature=curvature,maximum_se_error=se_error)
}
write.csv(do.call(rbind,checks),file.path(out,"numerical_checks.csv"),row.names=FALSE)
write_json(list(status="PASS_NUMERICAL_LIKELIHOOD",models=nrow(m),
 independently_recomputed=c("Log likelihood","AIC","Finite-difference score","Finite-difference Hessian","Coefficient standard errors"),
 crosswalk_and_area_binding_verified=TRUE,scientific_acceptance=FALSE),receipt,auto_unbox=TRUE,pretty=TRUE)
cat("PASS_NUMERICAL_LIKELIHOOD",nrow(m),"\n")
