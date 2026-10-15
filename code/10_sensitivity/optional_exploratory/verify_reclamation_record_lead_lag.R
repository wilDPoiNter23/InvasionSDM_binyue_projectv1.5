# Independently reconstruct time ordering, Poisson fits and city-cluster intervals.
suppressPackageStartupMessages(library(jsonlite))
a<-commandArgs(TRUE);stopifnot(length(a)==1L);cfg<-fromJSON(a[1]);out<-cfg$output_root
receipt<-fromJSON(file.path(out,'receipt.json'));stopifnot(receipt$status=='LEAD_LAG_RECORD_ASSOCIATION_COMPLETE')
rows<-read.csv(file.path(out,'fitted_city_period_data.csv'),stringsAsFactors=FALSE)
coef_table<-read.csv(file.path(out,'lead_lag_coefficients.csv'),stringsAsFactors=FALSE)
support<-read.csv(file.path(out,'model_support.csv'),stringsAsFactors=FALSE)
minimum<-if(is.null(cfg$minimum_outcome_start))1991 else cfg$minimum_outcome_start
first<-read.csv(file.path(cfg$record_root,'city_species_first_year.csv'),stringsAsFactors=FALSE)
roster<-read.csv(cfg$roster,stringsAsFactors=FALSE)$species
first<-first[first$species%in%roster,];first$key<-tolower(trimws(first$city_id))
stopifnot(all(rows$outcome_start>=minimum),all(rows$prior_exposure_end<rows$outcome_start),all(rows$target_records>0))
stopifnot(all(rows$new_count<=rows$recorded_species),all(rows$historical_records>=1))
checked<-list()
for(case in unique(coef_table$case)){
 m<-readRDS(file.path(out,paste0(case,'.rds')));d<-rows[rows$case==case,];X<-model.matrix(m);y<-m$y
 stopifnot(nrow(X)==nrow(d),identical(as.numeric(y),as.numeric(d$new_count)),!anyDuplicated(d[,c('key','outcome_start')]))
 independent_counts<-vapply(seq_len(nrow(d)),function(i)sum(first$key==d$key[i]&first$first_record_year>=d$outcome_start[i]&first$first_record_year<=d$outcome_end[i]),integer(1))
 stopifnot(identical(as.integer(d$new_count),independent_counts))
 refit<-glm.fit(X,y,family=poisson(),offset=m$offset,control=glm.control(maxit=100))
 stopifnot(refit$converged,max(abs(refit$coefficients-coef(m)))<1e-6)
 mu<-refit$fitted.values;A<-matrix(0,ncol(X),ncol(X));meat<-A
 for(i in seq_len(nrow(X)))A<-A+mu[i]*tcrossprod(X[i,])
 for(city in unique(d$key)){ix<-which(d$key==city);s<-colSums(X[ix,,drop=FALSE]*(y[ix]-mu[ix]));meat<-meat+tcrossprod(s)}
 G<-length(unique(d$key));n<-nrow(X);p<-ncol(X);B<-chol2inv(chol(A));V<-B%*%meat%*%t(B)*G/(G-1)*(n-1)/(n-p)
 for(term in coef_table$term[coef_table$case==case]){
  j<-match(term,colnames(X));s<-sqrt(V[j,j]);b<-refit$coefficients[j];crit<-qt(.975,G-1);r<-coef_table[coef_table$case==case&coef_table$term==term,]
  stopifnot(nrow(r)==1L,abs(r$estimate-b)<1e-6,abs(r$se-s)<1e-6,max(abs(c(r$lower,r$upper)-c(b-crit*s,b+crit*s)))<1e-5,
   r$n_cities==G,r$n_period_rows==n,abs(r$rate_change_doubling_area_plus_one-100*expm1(log(2)*b))<1e-5)
 }
 checked[[case]]<-list(cities=G,period_rows=n,coefficient_max_error=max(abs(refit$coefficients-coef(m))))
}
stopifnot(setequal(unique(coef_table$case),support$case[grepl('FITTED',support$status)]))
write_json(list(status='PASS_LEAD_LAG_RECORD_NUMERICAL_CHECKS',models=checked,minimum_outcome_start=minimum,
 checks='First-record recount, strict exposure-before-outcome ordering, independent glm.fit and row-wise information/city-score covariance reconstruction',
 interpretation='Observed first-record associations; no proof of biological arrival, detection correction or causality'),
 file.path(out,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
