#!/usr/bin/env Rscript
# Synthetic positive-count associations and support gates.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(TRUE);out <- args[1];script <- args[2]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"config.json")))stop("Existing fixture retained")
set.seed(20261014);n <- 108L
rec <- runif(n,0,200);area <- runif(n,20,2000);intensity <- sample(500:2000,n,replace=TRUE)
mu <- exp(1+.15*log1p(rec)+.1*log1p(area))
y <- rpois(n,mu);while(any(y==0))y[y==0] <- rpois(sum(y==0),mu[y==0])
p <- data.frame(city=paste0("City",seq_len(n)),reclamation_area=rec,area_km2=area,anthro_pc1=rnorm(n))
r <- data.frame(city_id=p$city,scope="RAW_SELECTED_INVASIVE",window="common2010_2024",richness=y,target_record_n=intensity)
nb <- r;nb$scope <- "SYNTHETIC_OVERDISPERSED"
nb$richness <- rnbinom(n,mu=mu,size=2)
while(any(nb$richness==0))nb$richness[nb$richness==0] <- rnbinom(sum(nb$richness==0),mu=mu[nb$richness==0],size=2)
nb$richness[1] <- NA_integer_;nb$target_record_n[1] <- 0L
unsupported <- r;unsupported$scope <- "SYNTHETIC_UNSUPPORTED";unsupported$richness <- NA_integer_;unsupported$target_record_n <- 0L
r <- rbind(r,nb,unsupported)
write.csv(p,file.path(out,"predictors.csv"),row.names=FALSE)
write.csv(r,file.path(out,"city_observed_richness.csv"),row.names=FALSE)
cfg <- list(output_root=out,city_predictors=file.path(out,"predictors.csv"),predictor_city_field="city",expected_city_count=n,minimum_model_cities=30,anthro_adjustment=TRUE)
write_json(cfg,file.path(out,"config.json"),auto_unbox=TRUE)
status <- system2(file.path(R.home("bin"),"Rscript"),c("--vanilla",shQuote(script),shQuote(file.path(out,"config.json"))))
stopifnot(status==0L)
co <- read.csv(file.path(out,"association","coefficients.csv"));m <- read.csv(file.path(out,"association","models.csv"))
stopifnot(nrow(m)==6L,all(is.finite(co$estimate)),all(is.finite(co$se)),all(m$n[m$scope=="RAW_SELECTED_INVASIVE"]==108L),
 all(m$n[m$scope=="SYNTHETIC_OVERDISPERSED"]==107L),all(m$family[m$scope=="SYNTHETIC_OVERDISPERSED"]=="zero_truncated_negative_binomial"))
support <- read.csv(file.path(out,"association","model_support.csv"))
stopifnot(all(support$status[support$scope=="SYNTHETIC_UNSUPPORTED"]=="INSUFFICIENT_SUPPORT"))
# Independent ordinary Poisson comparison is close when truncation probability is tiny.
fit <- glm(y~log1p(rec)+log1p(area),family=poisson())
stopifnot(max(abs(co$estimate[co$model=="M0"&co$scope=="RAW_SELECTED_INVASIVE"]-coef(fit)))<.02)
write_json(list(status="PASS_SYNTHETIC_COUNT_MODELS",models=nrow(m),poisson_high_mean_comparison=TRUE),file.path(out,"synthetic_association_verification.json"),pretty=TRUE,auto_unbox=TRUE)
cat("PASS_SYNTHETIC_COUNT_MODELS",nrow(m),"\n")
