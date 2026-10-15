# Fixed-community continuous means at existing native three-zone cells.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);if(length(a)!=1L)stop("Supply private configuration")
script<-sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE));source(file.path(dirname(script),"common_land_baseline_fixed_helpers.R"))
cfg<-fromJSON(a[1]);out<-cfg$output_root
if(dir.exists(out))stop("Fresh output required")
if(fromJSON(file.path(cfg$baseline_root,"verification.json"))$status!="PASS")stop("Verified completed baseline required")
roster<-read.csv(cfg$core_roster,stringsAsFactors=FALSE)
if(digest(file=cfg$core_roster,algo="sha256")!=cfg$core_roster_sha256||nrow(roster)!=261L||anyDuplicated(roster$species))stop("Frozen261 roster identity differs")
gates<-read.csv(file.path(cfg$baseline_root,"city_geometry_status.csv"),stringsAsFactors=FALSE)
stopifnot(nrow(gates)==108L,!anyDuplicated(gates$city_id))
files<-list.files(file.path(cfg$baseline_root,"cells"),pattern="[.]csv$",full.names=TRUE)
city_data<-lapply(files,function(p)read.csv(p,stringsAsFactors=FALSE))
keep<-vapply(city_data,function(x)length(unique(x$city_id))==1L&&unique(x$city_id)%in%gates$city_id[gates$status=="EXTRACTED"],logical(1))
files<-files[keep];city_data<-city_data[keep]
stopifnot(setequal(vapply(city_data,function(x)unique(x$city_id),character(1)),gates$city_id[gates$status=="EXTRACTED"]))
cells<-sort(unique(unlist(lapply(city_data,function(x)x$cell))))
threshold<-read.csv(cfg$paired_thresholds,stringsAsFactors=FALSE);threshold<-threshold[threshold$species%in%roster$species,]
columns<-c("species","group","projection_path","scale_divisor");models<-unique(threshold[,columns])
if(nrow(models)!=522L||anyDuplicated(paste(models$species,models$group))||!setequal(models$group,c("FULL","ENV")))stop("Exact261 paired projection keys required")
paired<-read.csv(cfg$paired_manifest,stringsAsFactors=FALSE)
for(i in seq_len(nrow(models))){m<-models[i,];p<-paired[paired$species==m$species,];g<-tolower(m$group)
 if(nrow(p)!=1L||p[[paste0(g,"_projection")]]!=m$projection_path||p[[paste0(g,"_scale_divisor")]]!=m$scale_divisor)stop("Projection differs from accepted paired manifest")}
models<-models[order(match(models$species,roster$species),models$group),]
dir.create(out,recursive=TRUE);dir.create(file.path(out,"tmp"));terraOptions(tempdir=file.path(out,"tmp"),memfrac=.2,progress=0)
write.csv(roster,file.path(out,"core_species_roster.csv"),row.names=FALSE)
source_files<-c(cfg$core_roster,cfg$paired_thresholds,cfg$paired_manifest,cfg$grid_template,file.path(cfg$baseline_root,c("verification.json","city_geometry_status.csv","input_checksums.csv")),files)
hashes<-data.frame(path=source_files,sha256=vapply(source_files,function(p)digest(file=p,algo="sha256"),character(1)))
write.csv(hashes,file.path(out,"input_checksums.csv"),row.names=FALSE)
state<-list(FULL=numeric(length(cells)),ENV=numeric(length(cells)),count=integer(length(cells)))
template<-rast(models$projection_path[1]);projection_hashes<-list()
if(!compareGeom(template,rast(cfg$grid_template),stopOnError=FALSE)||any(cells<1|cells>ncell(template)|cells!=round(cells)))stop("Baseline native cell geometry differs")
for(i in seq_len(261L)){
 values<-list()
 for(group in c("FULL","ENV")){
  m<-models[models$species==roster$species[i]&models$group==group,];r<-rast(m$projection_path)
  if(nlyr(r)!=1L||!compareGeom(template,r,stopOnError=FALSE))stop("Native projection geometry differs")
  values[[group]]<-fixed_score(extract(r,cells)[[1]],m$scale_divisor,length(cells))
  projection_hashes[[length(projection_hashes)+1L]]<-data.frame(species=m$species,group=group,path=m$projection_path,scale_divisor=m$scale_divisor,sha256=digest(file=m$projection_path,algo="sha256"))
 }
 state<-fixed_add(state,values$FULL,values$ENV)
 saveRDS(list(completed_species=i,roster_sha256=cfg$core_roster_sha256,cells=cells,state=state),file.path(out,"partial_sums.rds"))
 cat(i,roster$species[i],"complete_cells",sum(state$count==i),"\n");flush.console()
}
complete<-state$count==261L
value<-data.frame(cell=cells,joint_finite_species=state$count,FULL_sum=state$FULL,ENV_sum=state$ENV,FULL=ifelse(complete,state$FULL/261,NA_real_),ENV=ifelse(complete,state$ENV/261,NA_real_))
write.csv(value,file.path(out,"core_cell_values.csv"),row.names=FALSE)
write.csv(do.call(rbind,projection_hashes),file.path(out,"projection_checksums.csv"),row.names=FALSE)
rows<-list()
for(e in city_data){v<-value[match(e$cell,value$cell),];common<-v$joint_finite_species==261L&is.finite(e$area_km2)&e$area_km2>0
 for(zone in c("OUTSIDE_RING","NON_RECLAIMED_CITY","RECLAIMED_CITY")){
 take<-common&e$zone==zone;w<-e$area_km2[take];den<-sum(w)
 for(group in c("FULL","ENV"))rows[[length(rows)+1L]]<-data.frame(city_id=unique(e$city_id),zone=zone,support_policy="FIXED_261",group=group,
 mean_suitability=if(den>0)sum(w*v[[group]][take])/den else NA_real_,finite_native_cells=length(unique(e$cell[take])),native_cell_equivalents=sum(e$fraction[take]),common_area_km2=den,
 minimum_contributing_species=if(den>0)261 else NA_real_,maximum_contributing_species=if(den>0)261 else NA_real_,area_weighted_species_count=if(den>0)261 else NA_real_)
 }}
write.csv(do.call(rbind,rows),file.path(out,"zone_source_means.csv"),row.names=FALSE);write.csv(gates,file.path(out,"city_geometry_status.csv"),row.names=FALSE)
write_json(list(status="FIXED_261_COMPUTED",frozen_species_n=261,core_roster_sha256=cfg$core_roster_sha256,union_cells=length(cells),complete_cells=sum(complete),cell_values_sha256=digest(file=file.path(out,"core_cell_values.csv"),algo="sha256"),
 projection_manifest_sha256=digest(file=file.path(out,"projection_checksums.csv"),algo="sha256"),input_manifest_sha256=digest(file=file.path(out,"input_checksums.csv"),algo="sha256"),
 interpretation="Fixed261 continuous community, same jointly finite cells and area weights for FULL/ENV; finite support may exclude cities or zones; no causal attribution"),file.path(out,"extraction_receipt.json"),auto_unbox=TRUE,pretty=TRUE)
