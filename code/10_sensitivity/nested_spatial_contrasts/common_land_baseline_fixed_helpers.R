# Continuous scores share a frozen community and a joint finite mask.
fixed_score <- function(x, divisor, expected_length) {
  if(length(divisor)!=1L || !is.finite(divisor) || divisor<=0 || length(x)!=expected_length)stop("Score shape or scale differs")
  x<-as.numeric(x)/divisor
  if(any(is.finite(x)&(x<0|x>1)))stop("Scores outside 0..1")
  x
}
fixed_add <- function(state, full, env) {
  if(length(full)!=length(env)||length(full)!=length(state$count))stop("Pair shape differs")
  ok<-is.finite(full)&is.finite(env)
  state$FULL[ok]<-state$FULL[ok]+full[ok];state$ENV[ok]<-state$ENV[ok]+env[ok]
  state$count<-state$count+as.integer(ok);state
}
