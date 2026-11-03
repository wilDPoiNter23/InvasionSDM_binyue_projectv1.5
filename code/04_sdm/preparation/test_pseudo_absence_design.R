# Verify envelope boundaries, exclusions and independent replicate membership.
source('code/04_sdm/preparation/pseudo_absence_design.R')
p<-cbind(temperature=c(0,1,2,3,4),rain=c(10,11,12,13,14))
b<-sre_bounds(p,0)
stopifnot(identical(outside_sre(rbind(c(0,10),c(4,14),c(5,12),c(2,9)),b),c(FALSE,FALSE,TRUE,TRUE)))
draw<-draw_pa_replicates(1:100,1:5,10,3,42)
stopifnot(all(vapply(draw,length,integer(1))==10),all(!unlist(draw)%in%1:5),
          all(vapply(draw,function(x)!anyDuplicated(x),logical(1))),!identical(draw[[1]],draw[[2]]))
design<-make_pa_table(1:5,draw)
stopifnot(all(colSums(design$table[-(1:5),,drop=FALSE])==10),
          all(design$table[1:5,]),any(!design$table[-(1:5),]),
          all(is.na(design$response[-(1:5)])),all(design$response[1:5]==1))
stopifnot(inherits(try(draw_pa_replicates(1:5,1:4,2,1,1),silent=TRUE),'try-error'))
if(requireNamespace('terra',quietly=TRUE)){
 r<-terra::rast(nrows=10,ncols=10,nlyrs=2)
 terra::values(r)<-cbind(1:100,101:200)
 x<-extract_cells_blockwise(r,c(99,1,51,1),rows_per_block=3)
 stopifnot(identical(unname(x),cbind(c(99,1,51,1),c(199,101,151,101))))
}
cat('PASS: SRE boundary, presence exclusion, counts, unique cells and PA replicate membership.\n')

# Aliases must resolve known names without guessing or duplicating layers.
stopifnot(exists('apply_predictor_aliases',mode='function'))
r<-terra::rast(nrows=2,ncols=2,nlyrs=2)
names(r)<-c('temperature','bdod_0-5cm')
aliases<-data.frame(source_name='bdod_0-5cm',target_name='bdod_mean_2024_2_5min_aligned',evidence='Verified source rule')
renamed<-apply_predictor_aliases(r,aliases)
stopifnot(identical(names(renamed),c('temperature','bdod_mean_2024_2_5min_aligned')))
bad<-r;names(bad)<-c('bdod_mean_2024_2_5min_aligned','bdod_0-5cm')
stopifnot(inherits(try(apply_predictor_aliases(bad,aliases),silent=TRUE),'try-error'))
stopifnot(exists('format_equal_pa',mode='function'))
cat('PASS: explicit predictor aliases and sampling entry point.\n')

# Verify receipts against existing source files without creating any outputs.
root<-'code/04_sdm/preparation'
files<-c('pseudo_absence_design.R','prepare_pseudo_absence_pilot.R')
hashes<-pa_artifact_hashes(root,files)
stopifnot(verify_pa_artifacts(root,hashes,files))
receipt<-jsonlite::fromJSON(jsonlite::toJSON(list(artifact_sha256=hashes),auto_unbox=TRUE))
stopifnot(verify_pa_artifacts(root,receipt$artifact_sha256,files))
changed<-hashes;changed[[1]]<-strrep('0',64)
stopifnot(inherits(try(verify_pa_artifacts(root,changed,files),silent=TRUE),'try-error'),
 inherits(try(verify_pa_artifacts(root,hashes[-1],files),silent=TRUE),'try-error'),
 inherits(try(verify_pa_artifacts(root,NULL,files),silent=TRUE),'try-error'),
 inherits(try(pa_artifact_hashes(root,'../pseudo_absence_design.R'),silent=TRUE),'try-error'))
cat('PASS: receipt round trip, changed or missing artifact detection and relative-path checks.\n')

# The unit-only mode avoids loading optional BIOMOD dependencies.
if('--unit-only'%in%commandArgs(TRUE))quit(save='no',status=0)
# Exercise real BIOMOD formatting only; no fitting functions are called.
r<-terra::rast(nrows=100,ncols=100,xmin=0,xmax=1,ymin=0,ymax=1,crs='EPSG:4326')
xy<-terra::xyFromCell(r,1:terra::ncell(r))
r<-c(r,r);names(r)<-c('temperature','rainfall');terra::values(r)<-xy
presence<-as.vector(outer(45:50,45:49,function(a,b)(a-1)*100+b))
work<-Sys.getenv('INVASIONSDM_PA_TEST_ROOT',unset=tempdir())
dir.create(work,recursive=TRUE,showWarnings=FALSE)
f<-format_equal_pa(xy[c(presence,presence[1]),],r,'test_sre_equal',output_directory=work,seed=11)
is_pres<-!is.na(f@data.species)&f@data.species==1
stopifnot(sum(is_pres)==length(presence),all(colSums(f@PA.table[!is_pres,,drop=FALSE])==length(presence)),
 ncol(f@PA.table)==3,all(f@PA.table[is_pres,]),attr(f,'sampling_spec')$method=='sre_equal')
for(k in 1:3){
 loc<-as.matrix(f@coord[!is_pres&f@PA.table[,k],,drop=FALSE])
 stopifnot(!any(terra::cellFromXY(r,loc)%in%presence))
 bounds<-sre_bounds(terra::extract(r,xy[presence,]),.025)
 stopifnot(all(outside_sre(terra::extract(r,loc),bounds)))
}
cat('PASS: real SRE formatting; equal PA counts and three replicates; zero models fitted.\n')
