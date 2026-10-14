#!/usr/bin/env Rscript
# Fit parsimonious conditional count associations among observed cities.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args <- commandArgs(TRUE);cfg <- fromJSON(args[1],simplifyVector=FALSE);out <- cfg$output_root
association_directory <- if(is.null(cfg$association_directory))"association"else cfg$association_directory
if(length(association_directory)!=1L||!grepl("^[A-Za-z0-9_-]+$",association_directory))stop("Invalid association directory name")
dest <- file.path(out,association_directory)
if(dir.exists(dest))stop("Existing associations retained")
dir.create(dest)
p <- read.csv(cfg$city_predictors,stringsAsFactors=FALSE,check.names=FALSE)
if(!is.null(cfg$predictor_expected_sha256) && digest(file=cfg$city_predictors,algo="sha256")!=cfg$predictor_expected_sha256)stop("Predictor checksum mismatch")
stopifnot(nrow(p)==cfg$expected_city_count,!anyDuplicated(p[[cfg$predictor_city_field]]))
if(!all(c("reclamation_area","area_km2") %in% names(p)))stop("Missing area predictors")
pc_fields <- c("anthro_pc1","climate_pc1","soil_pc1","marine_pc1")
pc_complete <- if(all(pc_fields %in% names(p)))sum(complete.cases(p[,pc_fields]))else NA_integer_
if(!is.null(cfg$expected_pc_complete) && (is.na(pc_complete)||pc_complete!=cfg$expected_pc_complete))stop("Predictor complete-case contract failed")
p$city_id <- as.character(p[[cfg$predictor_city_field]])
d <- read.csv(file.path(out,"city_observed_richness.csv"),stringsAsFactors=FALSE)
normalize_city <- function(x)chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz",trimws(as.character(x)))
city_ids <- unique(as.character(d$city_id));city_keys <- normalize_city(city_ids);predictor_keys <- normalize_city(p$city_id)
differences <- rbind(data.frame(source="city_geometry",original=city_ids,normalized=city_keys,matched=city_keys %in% predictor_keys),
 data.frame(source="city_predictors",original=p$city_id,normalized=predictor_keys,matched=predictor_keys %in% city_keys))
write.csv(differences,file.path(dest,"city_identity_differences.csv"),row.names=FALSE)
if(length(city_ids)!=cfg$expected_city_count||anyNA(city_keys)||anyNA(predictor_keys)||any(!nzchar(city_keys))||any(!nzchar(predictor_keys))||
 anyDuplicated(city_keys)||anyDuplicated(predictor_keys)||!setequal(city_keys,predictor_keys))stop("Normalized city identity must be a complete one-to-one exact match")
crosswalk <- data.frame(city_id=city_ids,normalized_city_key=city_keys,predictor_city=p$city_id[match(city_keys,predictor_keys)],stringsAsFactors=FALSE)
crosswalk$match_type <- ifelse(crosswalk$city_id==crosswalk$predictor_city,"EXACT","ASCII_CASE_AND_OUTER_WHITESPACE")
write.csv(crosswalk,file.path(dest,"city_identity_crosswalk.csv"),row.names=FALSE)
p$predictor_city_original <- p$city_id
p$normalized_city_key <- predictor_keys
p$city_id <- city_ids[match(predictor_keys,city_keys)]
d <- merge(d,p,by="city_id",all.x=TRUE,sort=FALSE)
if(file.exists(file.path(out,"city_geometry.csv"))){
 geometry <- read.csv(file.path(out,"city_geometry.csv"),stringsAsFactors=FALSE)
 q <- merge(p[,c("city_id","area_km2")],geometry,by="city_id",all=TRUE)
 q$relative_area_difference <- (q$area_km2-q$area_geometry_km2)/q$area_geometry_km2
 write.csv(q,file.path(dest,"city_area_concordance.csv"),row.names=FALSE)
}
if(any(p$reclamation_area<0,na.rm=TRUE)||any(p$area_km2<=0,na.rm=TRUE))stop("Invalid predictor area")
d$reclamation <- log1p(d$reclamation_area);d$area <- log1p(d$area_km2);d$intensity <- log1p(d$target_record_n)
forms <- list(M0=richness~reclamation+area,M1=richness~reclamation+area+intensity)
if(isTRUE(cfg$anthro_adjustment))forms$M2 <- richness~reclamation+area+intensity+anthro_pc1
# Conditioning on positive observation support requires a zero-truncated likelihood.
fit_count <- function(x,f,family){
 X <- model.matrix(f,x);y <- x$richness;k <- ncol(X)
 initial <- glm.fit(X,y,family=poisson())$coefficients
 if(any(!is.finite(initial)))stop("Rank-deficient starting fit")
 nll <- function(b){
  mu <- exp(pmin(30,pmax(-30,drop(X%*%b[seq_len(k)]))))
  if(family=="poisson"){lp <- dpois(y,mu,log=TRUE);p0 <- exp(-mu)}else{
   theta <- exp(b[k+1L]);lp <- dnbinom(y,mu=mu,size=theta,log=TRUE);p0 <- exp(dnbinom(0,mu=mu,size=theta,log=TRUE))
  }
  -sum(lp-log1p(-pmin(p0,1-1e-14)))
 }
 start <- if(family=="poisson")initial else c(initial,log(10))
 score <- function(b){
  mu <- exp(drop(X%*%b[seq_len(k)]))
  if(family=="poisson")return(drop(crossprod(X,mu-y+mu/expm1(mu))))
  theta <- exp(b[k+1L]);p0 <- exp(-theta*log1p(mu/theta))
  beta <- drop(crossprod(X,theta*(mu-y)/(theta+mu)+p0/(1-p0)*theta*mu/(theta+mu)))
  lp_theta <- digamma(y+theta)-digamma(theta)+log(theta)-log(theta+mu)+1-(theta+y)/(theta+mu)
  dlogp0 <- theta*(-log1p(mu/theta)+mu/(theta+mu))
  c(beta,sum(-theta*lp_theta-p0/(1-p0)*dlogp0))
 }
 z <- optim(start,nll,gr=score,method="BFGS",hessian=TRUE,control=list(maxit=2000,reltol=1e-12))
 if(z$convergence!=0L||any(!is.finite(z$par))||max(abs(score(z$par)))>.01||min(eigen(z$hessian,symmetric=TRUE,only.values=TRUE)$values)<=1e-8)stop("Likelihood, score or curvature gate failed")
 se <- sqrt(diag(solve(z$hessian)))[seq_len(k)]
 mu <- exp(drop(X%*%z$par[seq_len(k)]));theta <- if(family=="poisson")Inf else exp(z$par[k+1L])
 p0 <- if(family=="poisson")exp(-mu)else exp(dnbinom(0,mu=mu,size=theta,log=TRUE))
 conditional_mean <- mu/(1-p0)
 variance <- (mu+mu^2*(1+if(is.finite(theta))1/theta else 0))/(1-p0)-conditional_mean^2
 list(coefficients=data.frame(term=colnames(X),estimate=z$par[seq_len(k)],se=se,lower=z$par[seq_len(k)]-1.96*se,upper=z$par[seq_len(k)]+1.96*se),
  dispersion=sum((y-conditional_mean)^2/pmax(variance,1e-8))/(nrow(x)-k),theta=theta,aic=2*z$value+2*length(z$par),fitted=conditional_mean)
}
models <- list();coef <- list();support <- list();residuals <- list()
for(s in unique(d$scope))for(w in unique(d$window)){
 z <- d[d$scope==s & d$window==w,,drop=FALSE]
 # Compare M0 and M1 on identical observed complete cases.
 common <- complete.cases(z[,c("richness","reclamation","area","intensity")]) & z$target_record_n>0
 for(m in names(forms)){
  use <- common
  if(m=="M2")use <- use & complete.cases(z$anthro_pc1)
  x <- z[use,,drop=FALSE];k <- length(all.vars(forms[[m]]))
  state <- "ELIGIBLE";reason <- ""
  if(nrow(x)<max(as.integer(cfg$minimum_model_cities),10L*k)||length(unique(x$richness))<3L||sd(x$reclamation)==0||sd(x$area)==0){state <- "INSUFFICIENT_SUPPORT";reason <- "Minimum cities, count variation or predictor variation gate"}
  if(state=="ELIGIBLE"){
   fit <- tryCatch({a <- fit_count(x,forms[[m]],"poisson");family <- "zero_truncated_poisson";poisson_disp <- a$dispersion
    if(poisson_disp>1.5){a <- fit_count(x,forms[[m]],"nb");family <- "zero_truncated_negative_binomial"};list(a=a,family=family,poisson_disp=poisson_disp)},error=function(e)e)
   if(inherits(fit,"error")){state <- "FIT_GATE_FAILED";reason <- conditionMessage(fit)}else{
    a <- fit$a;co <- a$coefficients;co$scope <- s;co$window <- w;co$model <- m;coef[[length(coef)+1L]] <- co
    models[[length(models)+1L]] <- data.frame(scope=s,window=w,model=m,n=nrow(x),family=fit$family,poisson_dispersion=fit$poisson_disp,dispersion=a$dispersion,theta=a$theta,aic=a$aic)
    residuals[[length(residuals)+1L]] <- data.frame(scope=s,window=w,model=m,city_id=x$city_id,observed=x$richness,fitted=a$fitted,residual=x$richness-a$fitted)
   }
  }
  support[[length(support)+1L]] <- data.frame(scope=s,window=w,model=m,total_cities=nrow(z),observed_cities=sum(z$target_record_n>0),model_cities=nrow(x),status=state,reason=reason)
 }
}
write.csv(do.call(rbind,support),file.path(dest,"model_support.csv"),row.names=FALSE)
if(length(models))write.csv(do.call(rbind,models),file.path(dest,"models.csv"),row.names=FALSE)
if(length(coef))write.csv(do.call(rbind,coef),file.path(dest,"coefficients.csv"),row.names=FALSE)
if(length(residuals))write.csv(do.call(rbind,residuals),file.path(dest,"residuals.csv"),row.names=FALSE)
write.csv(d,file.path(dest,"city_analysis_data.csv"),row.names=FALSE,na="")
write_json(list(status="EXPLORATORY_CONDITIONAL_ASSOCIATION",primary_window="common2010_2024",primary_scope="RAW_SELECTED_INVASIVE",
 formulas=lapply(forms,function(f)paste(deparse(f),collapse=" ")),negative_binomial_trigger="Positive-count Poisson Pearson dispersion > 1.5",
 predictor_sha256=digest(file=cfg$city_predictors,algo="sha256"),predictor_rows=nrow(p),predictor_columns=ncol(p)-3L,complete_pc_cities=pc_complete,
 city_identity="One-to-one ASCII lowercase and outer-whitespace exact keys; geometry display names preserved",
 crosswalk_sha256=digest(file=file.path(dest,"city_identity_crosswalk.csv"),algo="sha256"),
 configuration_sha256=digest(file=args[1],algo="sha256"),
 limitations=c("Target intensity is an endogenous proxy, not independent collecting effort",
 "Positive-count likelihood conditions on observed cities; informative sampling selection is not corrected and unsupported cities have unknown presence",
 "Species counts are bounded by the selected roster; count likelihood is exploratory",
 "No local native-status validation, spatial causal identification or full structural-equation verification",
 "Wald intervals rely on likelihood curvature; geographical dependence may narrow them",
 "Observed richness reflects detection, recording history and biological richness jointly")),file.path(dest,"association.json"),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(dest,"sessionInfo.txt"))
