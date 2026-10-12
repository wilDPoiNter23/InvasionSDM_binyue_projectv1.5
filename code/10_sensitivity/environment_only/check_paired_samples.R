#!/usr/bin/env Rscript
# Check aggregate values against direct species predictions at sampled cells.
suppressPackageStartupMessages(library(terra))
args <- commandArgs(TRUE)
if(length(args)!=2L) stop('Usage: Rscript check_paired_samples.R MANIFEST OUTPUT_DIR')
m <- read.csv(args[1],stringsAsFactors=FALSE); N <- nrow(m); out <- args[2]
f <- rast(file.path(out,sprintf('full_paired_%d_mean_suitability.tif',N)))
e <- rast(file.path(out,sprintf('env_only_paired_%d_mean_suitability.tif',N)))
count <- rast(file.path(out,sprintf('paired_%d_valid_species_count.tif',N)))
sites <- spatSample(count,size=64,method='regular',xy=TRUE,values=TRUE)
xy <- as.matrix(sites[,c('x','y')]); cells <- cellFromXY(count,xy)
fv <- ev <- matrix(NA_real_,nrow=nrow(xy),ncol=N)
for(i in seq_len(N)) {
  fv[,i] <- extract(rast(m$full_projection[i]),xy)[,1]/1000
  ev[,i] <- extract(rast(m$env_projection[i]),xy)[,1]/1000
}
valid <- is.finite(fv) & is.finite(ev); expected_count <- rowSums(valid)
stopifnot(any(expected_count>0))
fv[!valid] <- NA_real_; ev[!valid] <- NA_real_
expected_full <- rowMeans(fv,na.rm=TRUE); expected_env <- rowMeans(ev,na.rm=TRUE)
expected_full[expected_count==0] <- NA_real_; expected_env[expected_count==0] <- NA_real_
observed_full <- extract(f,xy)[,1]; observed_env <- extract(e,xy)[,1]; observed_count <- extract(count,xy)[,1]
stopifnot(identical(as.numeric(expected_count),as.numeric(observed_count)),
          identical(is.na(expected_full),is.na(observed_full)),
          identical(is.na(expected_env),is.na(observed_env)),
          identical(is.finite(expected_full),is.finite(observed_full)),
          identical(is.finite(expected_env),is.finite(observed_env)))
df <- abs(expected_full-observed_full); de <- abs(expected_env-observed_env)
stopifnot(all(df[expected_count>0]<1e-10),all(de[expected_count>0]<1e-10))
write.csv(data.frame(cell=cells,x=xy[,1],y=xy[,2],expected_count,observed_count,
                     expected_full,observed_full,expected_env,observed_env,
                     full_absolute_error=df,env_absolute_error=de),
          file.path(out,'paired_numeric_validation.csv'),row.names=FALSE)
cat('Direct species-value checks passed at',nrow(xy),'sampled cells.\n')
