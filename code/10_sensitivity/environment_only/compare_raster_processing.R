# Compare saved multispecies surfaces under controlled aggregation changes.
suppressPackageStartupMessages({library(terra);library(sf);library(jsonlite);library(digest)})
a<-commandArgs(TRUE);stopifnot(length(a)==2L);cfg<-fromJSON(a[1]);phase<-a[2];out<-cfg$output_root
script_arg<-grep('^--file=',commandArgs(FALSE),value=TRUE)
source(file.path(dirname(sub('^--file=','',script_arg)),'..','nested_spatial_contrasts','common_land_baseline_geometry.R'))
sf_use_s2(FALSE);key<-function(x)tolower(trimws(x))
write_table<-function(x,name)write.csv(x,file.path(out,name),row.names=FALSE)
area<-common_polygon_area
native<-function(x,cr){st_set_crs(st_transform(st_geometry(x),cr),NA)}
read_cells<-function(path,xy){r<-rast(path);stopifnot(nlyr(r)==1L);d<-extract(r,xy);as.numeric(d[[ncol(d)]])}
if(phase=='prepare'){
 stopifnot(!file.exists(file.path(out,'prepare_receipt.json')))
 dir.create(out,recursive=TRUE,showWarnings=FALSE);dir.create(file.path(out,'tmp'),showWarnings=FALSE)
 dir.create(file.path(out,'geometry_checkpoints'),showWarnings=FALSE)
 config_hash<-digest(file=a[1],algo='sha256');hash_path<-file.path(out,'preparation_config_sha256.txt')
 if(file.exists(hash_path))stopifnot(identical(readLines(hash_path),config_hash))else writeLines(config_hash,hash_path)
 terraOptions(tempdir=file.path(out,'tmp'),memfrac=.2,progress=0)
 cities<-st_read(cfg$cities,quiet=TRUE);cities$city_id<-as.character(cities[[cfg$city_id]])
 stopifnot(!anyDuplicated(key(cities$city_id)))
 template<-rast(cfg$surfaces[[1]]);cr<-crs(template)
 city_geom<-st_make_valid(st_transform(cities,cr));city_native<-st_set_crs(st_geometry(city_geom),NA)
 stripe<-rast(nrows=nrow(template),ncols=1,xmin=xmin(template),xmax=xmin(template)+res(template)[1],ymin=ymin(template),ymax=ymax(template),crs=cr)
 rowarea<-as.numeric(values(cellSize(stripe,unit='km',mask=FALSE)))
 sources<-list();geometry_rows<-list();weight_rows<-list();k<-0L
 for(label in names(cfg$reclamation_sources)){
  path<-cfg$reclamation_sources[[label]];shape_cache<-file.path(out,'geometry_checkpoints',paste0(label,'_source.rds'))
  if(file.exists(shape_cache)){saved<-readRDS(shape_cache);pieces<-saved$pieces;hits<-saved$hits;sources[[label]]<-saved$source}else{
   raw<-st_read(path,quiet=TRUE);raw<-st_transform(raw,cr);pieces<-suppressWarnings(st_cast(st_geometry(raw),'POLYGON'))
   hits<-st_intersects(st_geometry(city_geom),pieces)
   sources[[label]]<-data.frame(role=label,path=path,sha256=digest(file=path,algo='sha256'),features=nrow(raw),components=length(pieces))
   saveRDS(list(pieces=pieces,hits=hits,source=sources[[label]]),shape_cache,compress=FALSE)
  }
  for(i in seq_len(nrow(cities))){city<-cities$city_id[i]
   checkpoint<-file.path(out,'geometry_checkpoints',sprintf('%s_city_%03d.rds',label,i))
   if(file.exists(checkpoint)){saved<-readRDS(checkpoint);stopifnot(saved$city_id==city)
    geometry_rows<-c(geometry_rows,saved$rows);weight_rows<-c(weight_rows,saved$weights);k<-length(weight_rows);next
   }
   if(key(city)%in%key(cfg$exclude_city_keys)){
    geometry_rows[[length(geometry_rows)+1L]]<-data.frame(footprint=label,city_id=city,zone='BOTH',status='AUTHOR_EXCLUSION',geometry_area_km2=NA_real_,reason='Previously requested city exclusion');next
   }
   row_start<-length(geometry_rows);weight_start<-length(weight_rows)
   result<-tryCatch({
    cgeom<-city_native[i];ix<-hits[[i]]
    if(length(ix)){
     rr<-st_make_valid(st_set_crs(pieces[ix],NA));rr<-st_union(rr)
     rec<-suppressWarnings(st_intersection(cgeom,rr));rec<-rec[!st_is_empty(rec)&st_dimension(rec)==2L]
    }else rec<-st_sfc()
    non<-if(length(rec))suppressWarnings(st_difference(cgeom,st_union(rec)))else cgeom
    both<-list(RECLAIMED=rec,OTHER_URBAN=non)
    stopifnot(abs(area(rec)+area(non)-area(cgeom))<=max(1e-12,area(cgeom)*1e-7))
    for(zone in names(both)){
     z<-both[[zone]];z<-z[!st_is_empty(z)&st_dimension(z)==2L]
     if(!length(z)){
      geometry_rows[[length(geometry_rows)+1L]]<-data.frame(footprint=label,city_id=city,zone=zone,status='ZERO_GEOMETRY',geometry_area_km2=0,reason='No positive polygon area');next
     }
     z<-suppressWarnings(st_collection_extract(z,'POLYGON'));z<-st_union(z);zcr<-st_set_crs(z,cr)
     e<-extract(template,vect(zcr),exact=TRUE,cells=TRUE)
     e<-e[is.finite(e$cell)&is.finite(e$fraction)&e$fraction>0,,drop=FALSE]
     bad<-which(e$fraction>1)
     if(length(bad))for(j in bad){xy<-xyFromCell(template,e$cell[j]);dx<-res(template)[1]/2;dy<-res(template)[2]/2
      box<-st_sfc(st_polygon(list(rbind(c(xy[1]-dx,xy[2]-dy),c(xy[1]+dx,xy[2]-dy),c(xy[1]+dx,xy[2]+dy),c(xy[1]-dx,xy[2]+dy),c(xy[1]-dx,xy[2]-dy)))))
      e$fraction[j]<-area(suppressWarnings(st_intersection(z,box)))/area(box)
     }
     stopifnot(all(e$fraction>=0&e$fraction<=1+1e-8));e$fraction<-pmin(1,e$fraction)
     k<-k+1L;weight_rows[[k]]<-data.frame(footprint=label,city_id=city,zone=zone,cell=e$cell,fraction=e$fraction,weight_km2=e$fraction*rowarea[rowFromCell(template,e$cell)])
     geometry_rows[[length(geometry_rows)+1L]]<-data.frame(footprint=label,city_id=city,zone=zone,status=if(nrow(e))'PREPARED'else'NO_GRID_OVERLAP',geometry_area_km2=area(st_transform(zcr,6933))/1e6,reason='')
    };TRUE
   },error=function(err){geometry_rows[[length(geometry_rows)+1L]]<<-data.frame(footprint=label,city_id=city,zone='BOTH',status='GEOMETRY_ERROR',geometry_area_km2=NA_real_,reason=conditionMessage(err));FALSE})
   new_rows<-if(length(geometry_rows)>row_start)geometry_rows[seq.int(row_start+1L,length(geometry_rows))]else list()
   new_weights<-if(length(weight_rows)>weight_start)weight_rows[seq.int(weight_start+1L,length(weight_rows))]else list()
   if(result)saveRDS(list(city_id=city,rows=new_rows,weights=new_weights),checkpoint)
   write_table(do.call(rbind,geometry_rows),'city_geometry_status.csv')
   cat(label,city,result,'\n');flush.console()
  }
 }
 w<-do.call(rbind,weight_rows)
 totals<-aggregate(fraction~footprint+city_id+cell,w,sum);bad<-totals[totals$fraction>1+1e-7,];corrections<-list()
 if(nrow(bad))for(label in unique(bad$footprint)){
  saved_source<-readRDS(file.path(out,'geometry_checkpoints',paste0(label,'_source.rds')))
  for(city in unique(bad$city_id[bad$footprint==label])){
   i<-match(city,cities$city_id);cgeom<-city_native[i];ix<-saved_source$hits[[i]]
   rr<-st_union(st_make_valid(st_set_crs(saved_source$pieces[ix],NA)))
   rec<-suppressWarnings(st_intersection(cgeom,rr));non<-suppressWarnings(st_difference(cgeom,st_union(rec)))
   polygons<-list(RECLAIMED=rec,OTHER_URBAN=non)
   bad_cells<-bad$cell[bad$footprint==label&bad$city_id==city]
   affected<-which(w$footprint==label&w$city_id==city&w$cell%in%bad_cells)
   for(j in affected){xy<-xyFromCell(template,w$cell[j]);dx<-res(template)[1]/2;dy<-res(template)[2]/2
    box<-st_sfc(st_polygon(list(rbind(c(xy[1]-dx,xy[2]-dy),c(xy[1]+dx,xy[2]-dy),c(xy[1]+dx,xy[2]+dy),c(xy[1]-dx,xy[2]+dy),c(xy[1]-dx,xy[2]-dy)))))
    actual<-area(suppressWarnings(st_intersection(polygons[[w$zone[j]]],box)))/area(box)
    stopifnot(is.finite(actual),actual>=0,actual<=1+1e-8)
    corrections[[length(corrections)+1L]]<-data.frame(footprint=label,city_id=city,zone=w$zone[j],cell=w$cell[j],original_fraction=w$fraction[j],geos_fraction=actual)
    w$fraction[j]<-min(1,actual);w$weight_km2[j]<-w$fraction[j]*rowarea[rowFromCell(template,w$cell[j])]
   }
  }
 }
 if(length(corrections))write_table(do.call(rbind,corrections),'fraction_corrections.csv')
 w<-w[w$fraction>0&w$weight_km2>0,];cells<-sort(unique(w$cell));w$cell_index<-match(w$cell,cells)
 totals<-aggregate(fraction~footprint+city_id+cell,w,sum);stopifnot(all(totals$fraction<=1+1e-7))
 saveRDS(list(weights=w,cells=cells,xy=xyFromCell(template,cells),grid_path=cfg$surfaces[[1]]),file.path(out,'support.rds'))
 write_table(do.call(rbind,geometry_rows),'city_geometry_status.csv');write_table(do.call(rbind,sources),'geometry_sources.csv')
 write_json(list(status='PREPARED',cities_in_boundary=nrow(cities),cities_with_geometry=length(unique(w$city_id)),cells=length(cells),configuration_sha256=digest(file=a[1],algo='sha256'),rule='All supplied city boundaries; two urban zones only; no minimum-cell, outside-ring, cohort-overlap or common-response coverage gate'),file.path(out,'prepare_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
 quit()
}
support<-readRDS(file.path(out,'support.rds'));xy<-support$xy;cells<-support$cells
if(phase=='extract'){
 stopifnot(!file.exists(file.path(out,'cell_values.rds')))
 paired<-read.csv(cfg$paired_manifest);original<-read.csv(cfg$original_manifest)
 inventory<-read.delim(cfg$full_inventory,check.names=FALSE)
 continuous<-function(path){if(is.na(path)||!nzchar(path))return(NA_character_);if(!grepl('TSSbin|TSSfilt',basename(path)))return(path)
  stem<-sub('[.](grd|tif|tiff)$','',path);stem<-gsub('_TSSbin|_TSSfilt','',stem)
  q<-paste0(stem,c('.grd','.tif','.tiff'));exists<-q[file.exists(q)];if(length(exists))exists[1]else NA_character_
 }
 sets<-list()
 sets$HISTORICAL_SELECTED<-data.frame(species=original$accepted_taxon,path=original$original_projection_path)
 sets$HISTORICAL_CONTINUOUS<-data.frame(species=original$accepted_taxon,path=vapply(original$original_projection_path,continuous,character(1)))
 sets$ALL_FULL_SELECTED<-data.frame(species=inventory$accepted_taxon,path=inventory$baseline_global_projection_path)
 sets$ALL_FULL_CONTINUOUS<-data.frame(species=inventory$accepted_taxon,path=vapply(inventory$baseline_global_projection_path,continuous,character(1)))
 sets$PAIRED_FULL<-data.frame(species=paired$species,path=paired$full_projection)
 sets$PAIRED_ENV<-data.frame(species=paired$species,path=paired$env_projection)
 inputs<-do.call(rbind,lapply(names(sets),function(s)data.frame(set=s,sets[[s]])))
 inputs$exists<-!is.na(inputs$path)&file.exists(inputs$path)
 inputs$binary_filename<-grepl('TSSbin',inputs$path)
 write_table(inputs,'species_inputs.csv')
 paths<-unique(inputs$path[inputs$exists]);values<-vector('list',length(paths));stats<-vector('list',length(paths));names(values)<-paths
 reference<-rast(support$grid_path)
 dir.create(file.path(out,'input_cells'),showWarnings=FALSE)
 process_input<-function(i){
  path<-paths[i]
  cache<-file.path(out,'input_cells',paste0(digest(path,algo='sha256'),'.rds'))
  if(file.exists(cache)){
   saved<-readRDS(cache);stopifnot(length(saved$value)==length(cells),saved$metadata$path==path,saved$metadata$file_bytes==file.info(path)$size)
   return(saved)
  }
  r<-rast(path);stopifnot(nlyr(r)==1L,compareGeom(reference,r,stopOnError=FALSE))
  v<-read_cells(path,xy);mm<-minmax(r)
  if(any(!is.finite(mm)))mm<-as.numeric(global(r,c('min','max'),na.rm=TRUE)[1,])
  lo<-as.numeric(mm[1]);hi<-as.numeric(mm[2]);finite_range<-is.finite(lo)&&is.finite(hi)
  if(!finite_range){stopifnot(!any(is.finite(v)));lo<-hi<-NA_real_}
  ans<-list(value=v,metadata=data.frame(path=path,minimum=lo,maximum=hi,datatype=datatype(r),sample_finite=sum(is.finite(v)),sample_zero=sum(v==0,na.rm=TRUE),binary_filename=grepl('TSSbin',path),file_bytes=file.info(path)$size,modified_utc=format(file.info(path)$mtime,tz='UTC',usetz=TRUE),sample_sha256=digest(v,algo='sha256')))
  saveRDS(ans,cache)
  if(i%%25==0L){cat('Input',i,'of',length(paths),'\n');flush.console();gc()}
  ans
 }
 inspect_input<-function(i)tryCatch(process_input(i),error=function(e){cat('INPUT_ERROR',paths[i],conditionMessage(e),'\n');flush.console();list(input_error=conditionMessage(e),path=paths[i])})
 workers<-if(is.null(cfg$workers))1L else as.integer(cfg$workers)
 inspected<-parallel::mclapply(seq_along(paths),inspect_input,mc.cores=workers,mc.preschedule=TRUE)
 errors<-vapply(inspected,function(z)inherits(z,'try-error')||!is.null(z$input_error),logical(1))
 if(any(errors)){
  err<-lapply(which(errors),function(i)data.frame(path=paths[i],error=if(inherits(inspected[[i]],'try-error'))as.character(inspected[[i]])else inspected[[i]]$input_error))
  write_table(do.call(rbind,err),'input_errors.csv');stop('Input errors recorded; valid input-cell caches retained')
 }
 values<-lapply(inspected,`[[`,'value');names(values)<-paths;stats<-lapply(inspected,`[[`,'metadata');rm(inspected)
 stats<-do.call(rbind,stats);write_table(stats,'raster_input_metadata.csv')
 variants<-list();rules<-list();denominators<-list()
 for(s in names(sets)){
  q<-inputs[inputs$set==s&inputs$exists,];if(!nrow(q))next
  for(normalization in c('SCALE','MINMAX'))for(denominator in c('AVAILABLE','ZERO_FIXED')){
   total<-numeric(length(cells));count<-integer(length(cells))
   for(j in seq_len(nrow(q))){v<-values[[q$path[j]]];ok<-is.finite(v);meta<-stats[match(q$path[j],stats$path),]
    if(normalization=='SCALE'){if(grepl('TSSbin',q$path[j]))v<-v else v<-v/1000
    }else v<-if(!is.finite(meta$maximum)||!is.finite(meta$minimum)||meta$maximum==meta$minimum)v*0 else(v-meta$minimum)/(meta$maximum-meta$minimum)
    total[ok]<-total[ok]+v[ok];count[ok]<-count[ok]+1L
   }
   value<-if(denominator=='AVAILABLE')ifelse(count>0,total/count,NA_real_)else total/nrow(q)
   label<-paste(s,normalization,denominator,sep='__');variants[[label]]<-value;denominators[[label]]<-count
   rules[[label]]<-data.frame(variant=label,species_set=s,normalization=normalization,denominator=denominator,requested_species=sum(inputs$set==s),available_species=nrow(q),complete_input_set=all(inputs$exists[inputs$set==s]),native_range_mask='No additional native-range mask introduced')
  }
 }
 # Reconstruct the exact shared-membership rule of the saved paired products.
 for(g in c('FULL','ENV')){
  total<-numeric(length(cells));count<-integer(length(cells))
  for(i in seq_len(nrow(paired))){f<-values[[paired$full_projection[i]]];e<-values[[paired$env_projection[i]]];ok<-is.finite(f)&is.finite(e);v<-if(g=='FULL')f else e;total[ok]<-total[ok]+v[ok]/1000;count[ok]<-count[ok]+1L}
  label<-paste0('PAIRED_',g,'__SCALE_JOINT_AVAILABLE');variants[[label]]<-ifelse(count>0,total/count,NA_real_);denominators[[label]]<-count
  rules[[label]]<-data.frame(variant=label,species_set=paste0('PAIRED_',g),normalization='SCALE',denominator='JOINT_AVAILABLE',requested_species=nrow(paired),available_species=nrow(paired),complete_input_set=TRUE,native_range_mask='No additional native-range mask introduced')
 }
 for(label in names(cfg$surfaces)){v<-read_cells(cfg$surfaces[[label]],xy);variants[[label]]<-v;denominators[[label]]<-rep(NA_real_,length(v))
  rules[[label]]<-data.frame(variant=label,species_set='STORED_SURFACE',normalization='AS_STORED',denominator='AS_STORED',requested_species=NA,available_species=NA,complete_input_set=NA,native_range_mask='Stored values unchanged')}
 saveRDS(list(cells=cells,values=variants,counts=denominators),file.path(out,'cell_values.rds'))
 write_table(do.call(rbind,rules),'processing_rules.csv')
 # Manifest hashes, file metadata and extracted-value hashes bind saved inputs.
 fingerprint_paths<-unique(c(cfg$cities,cfg$paired_manifest,cfg$original_manifest,cfg$full_inventory,unlist(cfg$surfaces)))
 write_table(data.frame(path=fingerprint_paths,sha256=vapply(fingerprint_paths,function(p)digest(file=p,algo='sha256'),character(1))),'input_checksums.csv')
 quit()
}
if(phase=='summarise'){
 d<-readRDS(file.path(out,'cell_values.rds'));w<-support$weights;rows<-list();j<-0L
 groups<-split(seq_len(nrow(w)),interaction(w$footprint,w$city_id,w$zone,drop=TRUE))
 for(label in names(d$values))for(ix in groups){z<-w[ix,];v<-d$values[[label]][z$cell_index];n<-d$counts[[label]][z$cell_index];ok<-is.finite(v)&z$weight_km2>0;j<-j+1L
  rows[[j]]<-data.frame(variant=label,footprint=z$footprint[1],city_id=z$city_id[1],zone=z$zone[1],mean=if(any(ok))weighted.mean(v[ok],z$weight_km2[ok])else NA_real_,finite_area_km2=sum(z$weight_km2[ok]),total_grid_area_km2=sum(z$weight_km2),zero_area_km2=sum(z$weight_km2[ok&v==0]),finite_cells=sum(ok),mean_species_count=if(any(ok&is.finite(n)))weighted.mean(n[ok&is.finite(n)],z$weight_km2[ok&is.finite(n)])else NA_real_)
 }
 means<-do.call(rbind,rows);write_table(means,'city_zone_means.csv');results<-list();j<-0L;set.seed(cfg$seed)
 for(label in unique(means$variant))for(foot in unique(means$footprint)){
  z<-means[means$variant==label&means$footprint==foot,];r<-z[z$zone=='RECLAIMED',];n<-z[z$zone=='OTHER_URBAN',]
  q<-merge(r,n,by=c('variant','footprint','city_id'),suffixes=c('_rec','_other'));q<-q[is.finite(q$mean_rec)&is.finite(q$mean_other)&q$finite_area_km2_rec>0&q$finite_area_km2_other>0,]
  if(!nrow(q))next
  for(mode in c('EQUAL_CITY','POOLED_AREA')){
   summarize<-function(ix){t<-q[ix,];if(mode=='EQUAL_CITY')c(mean(t$mean_rec),mean(t$mean_other))else c(weighted.mean(t$mean_rec,t$finite_area_km2_rec),weighted.mean(t$mean_other,t$finite_area_km2_other))}
   point<-summarize(seq_len(nrow(q)));boot<-replicate(cfg$bootstrap_replicates,{p<-summarize(sample.int(nrow(q),nrow(q),TRUE));c(p[1]-p[2],100*(p[1]/p[2]-1))});j<-j+1L
   interval<-function(v,p){v<-v[is.finite(v)];if(length(v))unname(quantile(v,p))else NA_real_}
   results[[j]]<-data.frame(variant=label,footprint=foot,weighting=mode,cities=nrow(q),reclaimed=point[1],other_urban=point[2],difference=point[1]-point[2],difference_lower=interval(boot[1,],.025),difference_upper=interval(boot[1,],.975),percent=if(point[2]>0)100*(point[1]/point[2]-1)else NA_real_,percent_lower=interval(boot[2,],.025),percent_upper=interval(boot[2,],.975),undefined_percent_bootstraps=sum(!is.finite(boot[2,])))
  }
 }
 write_table(do.call(rbind,results),'contrasts.csv')
 # Compare each reconstruction directly with the historical surface at identical cells.
 original<-d$values[[cfg$historical_surface_label]];concordance<-lapply(names(d$values),function(label){v<-d$values[[label]];ok<-is.finite(v)&is.finite(original);delta<-v[ok]-original[ok];data.frame(variant=label,cells=sum(ok),mae=mean(abs(delta)),maximum_absolute_error=max(abs(delta)),correlation=cor(v[ok],original[ok]),historical_missing_rebuilt_finite=sum(!is.finite(original)&is.finite(v)),historical_zero_rebuilt_positive=sum(original==0&v>0,na.rm=TRUE))})
 write_table(do.call(rbind,concordance),'historical_surface_concordance.csv')
 if('STORED_GLOBAL_814'%in%names(d$values)){
  rebuilt<-d$values$ALL_FULL_SELECTED__MINMAX__ZERO_FIXED;stored<-d$values$STORED_GLOBAL_814;ok<-is.finite(rebuilt)&is.finite(stored)
  write_table(data.frame(stored='STORED_GLOBAL_814',reconstruction='ALL_FULL_SELECTED__MINMAX__ZERO_FIXED',cells=sum(ok),finite_coverage_identical=identical(is.finite(rebuilt),is.finite(stored)),mae=mean(abs(rebuilt[ok]-stored[ok])),maximum_absolute_error=max(abs(rebuilt[ok]-stored[ok])),correlation=cor(rebuilt[ok],stored[ok])),'stored_surface_reconstruction.csv')
 }
 write_json(list(status='COMPUTED',no_sdm_fitting=TRUE,variants=length(d$values),native_cells=length(cells),support_sha256=digest(file=file.path(out,'support.rds'),algo='sha256'),values_sha256=digest(file=file.path(out,'cell_values.rds'),algo='sha256')),file.path(out,'comparison_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
 quit()
}
stop('Unknown phase')
