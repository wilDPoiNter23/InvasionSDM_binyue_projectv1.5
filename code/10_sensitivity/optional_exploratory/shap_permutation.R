# Interventional permutation Shapley estimates against a frozen empirical background.
shap_permutation <- function(predict_function,x,background,pairs=16L,seed=20261015L) {
  stopifnot(pairs>=2L,identical(colnames(x),colnames(background)))
  x<-as.matrix(x);background<-as.matrix(background);ne<-nrow(x);nb<-nrow(background);p<-ncol(x)
  baseline<-mean(predict_function(background));fx<-predict_function(x)
  set.seed(seed);orders<-replicate(pairs,sample.int(p),simplify=FALSE)
  pair_values<-array(0,c(ne,p,pairs));checkpoints<-list()
  for(k in seq_len(pairs)) {
    for(order in list(orders[[k]],rev(orders[[k]]))) {
      current<-background[rep(seq_len(nb),times=ne),,drop=FALSE]
      target<-x[rep(seq_len(ne),each=nb),,drop=FALSE];chain<-vector("list",p+1L);chain[[1]]<-current
      for(j in seq_len(p)){current[,order[j]]<-target[,order[j]];chain[[j+1L]]<-current}
      predictions<-predict_function(do.call(rbind,chain))
      values<-vapply(seq_len(p+1L),function(j)colMeans(matrix(predictions[seq.int((j-1L)*ne*nb+1L,j*ne*nb)],nrow=nb)),numeric(ne))
      for(j in seq_len(p))pair_values[,order[j],k]<-pair_values[,order[j],k]+(values[,j+1L]-values[,j])/2
    }
    if(k %in% unique(c(2L,pairs%/%2L,pairs)))checkpoints[[as.character(k)]]<-apply(pair_values[,,seq_len(k),drop=FALSE],c(1,2),mean)
  }
  phi<-apply(pair_values,c(1,2),mean);se<-apply(pair_values,c(1,2),sd)/sqrt(pairs)
  colnames(phi)<-colnames(x);colnames(se)<-colnames(x)
  residual<-fx-baseline-rowSums(phi)
  list(phi=phi,se=se,baseline=baseline,prediction=fx,additivity_residual=residual,
    checkpoints=checkpoints,pair_values=pair_values,permutations=2L*pairs)
}
shap_synthetic_check <- function() {
  b<-matrix(c(0,2,1,3,4,2),ncol=2,dimnames=list(NULL,c("a","b")))
  x<-matrix(c(3,1,4,6),ncol=2,dimnames=list(NULL,c("a","b")))
  linear<-function(z)2*z[,1]-3*z[,2]+.5
  z<-shap_permutation(linear,x,b,pairs=4L,seed=11L)
  expected<-sweep(x,2,colMeans(b),"-");expected<-sweep(expected,2,c(2,-3),"*")
  stopifnot(max(abs(z$phi-expected))<1e-12,max(abs(z$additivity_residual))<1e-12)
  nonlinear<-function(z)z[,1]*z[,2]
  q<-shap_permutation(nonlinear,x,b,pairs=4L,seed=11L)
  stopifnot(max(abs(q$additivity_residual))<1e-12)
  cat("Permutation SHAP linear truth and interaction additivity passed\n")
}
