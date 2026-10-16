# Gaussian crossed-intercept Gibbs sampling with proper normal and variance priors.
coastal_compile<-function(){
 if(exists('coastal_sampler_cpp',mode='function'))return(invisible(NULL))
 Rcpp::cppFunction(code='
 Rcpp::List coastal_sampler_cpp(Rcpp::NumericMatrix X, Rcpp::NumericVector y,
 Rcpp::List groups, int warmup, int samples, int thin, double prior_scale,
 double variance_rate, double fixed_variance) {
 int n=y.size(),p=X.ncol(),G=groups.size(),S=samples/thin;
 Rcpp::NumericVector b(p),res=Rcpp::clone(y),xx(p);
 std::vector<std::vector<int> > ids(G),counts(G);
 std::vector<std::vector<double> > u(G);
 std::vector<double> tau(G,1.0);
 for(int k=0;k<p;k++)for(int i=0;i<n;i++)xx[k]+=X(i,k)*X(i,k);
 for(int g=0;g<G;g++){Rcpp::IntegerVector v=groups[g];int J=Rcpp::max(v);
 ids[g].resize(n);counts[g].assign(J,0);u[g].assign(J,0.0);
 for(int i=0;i<n;i++){ids[g][i]=v[i]-1;counts[g][v[i]-1]++;}}
 double sigma=fixed_variance>0?fixed_variance:1.0;
 Rcpp::NumericMatrix B(S,p),V(S,G+1);
 int row=0;double bp=1.0/(prior_scale*prior_scale);
 for(int it=0;it<warmup+samples;it++){
 for(int k=0;k<p;k++){double sy=0;
 for(int i=0;i<n;i++){res[i]+=X(i,k)*b[k];sy+=X(i,k)*res[i];}
 double v=1.0/(xx[k]/sigma+bp);b[k]=R::rnorm(v*sy/sigma,std::sqrt(v));
 for(int i=0;i<n;i++)res[i]-=X(i,k)*b[k];}
 for(int g=0;g<G;g++){int J=u[g].size();std::vector<double> sum(J,0.0);
 for(int i=0;i<n;i++){res[i]+=u[g][ids[g][i]];sum[ids[g][i]]+=res[i];}
 for(int j=0;j<J;j++){double v=1.0/(counts[g][j]/sigma+1.0/tau[g]);
 u[g][j]=R::rnorm(v*sum[j]/sigma,std::sqrt(v));}
 for(int i=0;i<n;i++)res[i]-=u[g][ids[g][i]];
 // An exact conditional shift improves intercept mixing without changing fits.
 double su=0;for(int j=0;j<J;j++)su+=u[g][j];
 double v=1.0/(J/tau[g]+bp);double shift=R::rnorm(v*(su/tau[g]-b[0]*bp),std::sqrt(v));
 b[0]+=shift;for(int j=0;j<J;j++)u[g][j]-=shift;
 double ss=0;for(int j=0;j<J;j++)ss+=u[g][j]*u[g][j];
 tau[g]=1.0/R::rgamma(2.0+J/2.0,1.0/(variance_rate+ss/2.0));}
 if(fixed_variance<=0){double ss=0;for(int i=0;i<n;i++)ss+=res[i]*res[i];
 sigma=1.0/R::rgamma(2.0+n/2.0,1.0/(variance_rate+ss/2.0));}
 if(it>=warmup && (it-warmup+1)%thin==0){
 for(int k=0;k<p;k++)B(row,k)=b[k];V(row,0)=sigma;
 for(int g=0;g<G;g++)V(row,g+1)=tau[g];row++;}
 if(it%1000==0)Rcpp::checkUserInterrupt();}
 return Rcpp::List::create(Rcpp::Named("beta")=B,Rcpp::Named("variance")=V,
 Rcpp::Named("residual")=res);}',env=.GlobalEnv)
}
coastal_gibbs<-function(X,y,groups,warmup=3000L,samples=6000L,thin=2L,seed=1L,
 prior_scale=2,variance_rate=1,fixed_variance=-1){
 stopifnot(nrow(X)==length(y),all(is.finite(X)),all(is.finite(y)),all(X[,1]==1),
 samples%%thin==0,all(vapply(groups,length,integer(1))==length(y)))
 coastal_compile();set.seed(seed)
 z<-coastal_sampler_cpp(X,y,lapply(groups,as.integer),warmup,samples,thin,prior_scale,variance_rate,fixed_variance)
 colnames(z$beta)<-colnames(X);z
}
coastal_rhat<-function(chains){
 split<-do.call(cbind,lapply(chains,function(x){n<-floor(length(x)/2);cbind(head(x,n),tail(x,n))}))
 calc<-function(m){n<-nrow(m);W<-mean(apply(m,2,var));B<-n*var(colMeans(m));sqrt(((n-1)*W/n+B/n)/W)}
 normrank<-function(m)matrix(qnorm((rank(m,ties.method='average')-.375)/(length(m)+.25)),nrow(m))
 max(calc(normrank(split)),calc(normrank(abs(split-median(split)))))
}
coastal_ess<-function(chains){
 # Conservative per-chain initial-positive-pair autocorrelation estimate.
 sum(vapply(chains,function(x){a<-as.numeric(acf(x,plot=FALSE,lag.max=min(1000,length(x)-1))$acf)[-1]
 m<-floor(length(a)/2);pair<-a[seq(1,2*m,2)]+a[seq(2,2*m,2)];bad<-which(pair<=0)
 if(length(bad))pair<-head(pair,bad[1]-1);length(x)/max(1,1+2*sum(pair))},numeric(1)))
}
