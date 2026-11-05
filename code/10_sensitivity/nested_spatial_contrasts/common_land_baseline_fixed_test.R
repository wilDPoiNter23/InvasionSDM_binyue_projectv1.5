# Joint completeness, zero scores, scale and shape boundaries.
a<-commandArgs(TRUE);source(a[1])
z<-list(FULL=numeric(3),ENV=numeric(3),count=integer(3))
z<-fixed_add(z,fixed_score(c(0,1000,NA),1000,3),c(1,NA,.5))
z<-fixed_add(z,c(.2,.3,.4),c(.4,.5,.6))
stopifnot(identical(z$count,c(2L,1L,1L)),all(abs(z$FULL-c(.2,.3,.4))<1e-12),all(abs(z$ENV-c(1.4,.5,.6))<1e-12))
stopifnot(inherits(try(fixed_score(1,0,1),silent=TRUE),"try-error"),inherits(try(fixed_score(c(1,2),1,1),silent=TRUE),"try-error"),inherits(try(fixed_score(1001,1000,1),silent=TRUE),"try-error"))
cat("PASS heterogeneous finite support, genuine zero, scale and shape\n")
