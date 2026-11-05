# Check source identity and numerical agreement before resolving soil names.
# Usage: Rscript verify_soil_predictor_aliases.R configuration.json
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
args<-commandArgs(TRUE);stopifnot(length(args)==1);cfg<-fromJSON(args[1])
r<-rast(cfg$predictor_stack)
inventory<-read.delim(cfg$inventory,check.names=FALSE)
needed<-unique(unlist(strsplit(inventory$environmental_predictor_vector,';',fixed=TRUE)))
files<-list.files(cfg$soil_directory,pattern='tif$',full.names=TRUE)
set.seed(cfg$seed);cells<-sample.int(ncell(r),10000);xy<-xyFromCell(r,cells)
z<-extract(r,xy)
hash<-function(p)digest(readBin(p,'raw',n=file.info(p,extra_cols=FALSE)$size),algo='sha256',serialize=FALSE)
results<-lapply(files,function(f){
 s<-rast(f);original<-file.path(cfg$original_soil_directory,basename(f))
 prefix<-sub('_.*','',basename(f));old<-grep(paste0('^',prefix,'_'),names(r),value=TRUE)
 stopifnot(length(old)==1,compareGeom(r[[1]],s))
 v<-extract(s,xy)[,1];w<-z[[old]];good<-is.finite(v)&is.finite(w)
 a<-hash(f);b<-hash(original);target<-paste0(prefix,'_mean_2024_2_5min_aligned')
 data.frame(source_name=old,target_name=target,source_file=f,original_file=original,
  source_sha256=a,original_sha256=b,files_identical=a==b,geometry_equal=TRUE,
  n_checked=length(cells),n_finite=sum(good),missing_mismatch=sum(is.finite(v)!=is.finite(w)),
  max_abs_difference=if(any(good))max(abs(v[good]-w[good]))else NA_real_,
  required_by_inventory=target%in%needed,
  evidence='build_stack.R renaming rule; source hashes; 10000-cell numerical check')
})
d<-do.call(rbind,results)
dir.create(file.path(cfg$output_root,'tables'),recursive=TRUE,showWarnings=FALSE)
write.csv(d,file.path(cfg$output_root,'tables','soil_layer_identity.csv'),row.names=FALSE)
required<-d[d$required_by_inventory,]
stopifnot(nrow(required)>0,all(required$files_identical),all(required$n_finite>0),
          all(required$missing_mismatch==0),all(required$max_abs_difference==0))
write.csv(required[,c('source_name','target_name','evidence')],
          file.path(cfg$output_root,'tables','predictor_aliases.csv'),row.names=FALSE)
print(d[,c('source_name','files_identical','n_finite','missing_mismatch','max_abs_difference','required_by_inventory')])
cat('PASS: required soil aliases verified. Unused empty layers are not admitted.\n')
