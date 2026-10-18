# Compare saved continuous predictions with native domains and missing coverage separated.
suppressPackageStartupMessages({library(terra);library(sf);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
cfg <- fromJSON(args[1]); out <- cfg$output_root; base <- cfg$cached_comparison
dir.create(out,recursive=TRUE,showWarnings=FALSE)
sf_use_s2(FALSE)
support <- readRDS(file.path(base,'support.rds'))
inputs <- read.csv(file.path(base,'species_inputs.csv'))
full <- inputs[inputs$set=='ALL_FULL_CONTINUOUS',c('species','path')]
paired <- read.csv(cfg$paired_manifest)
stopifnot(nrow(full)==814L,!anyDuplicated(full$species),all(paired$species%in%full$species))
template <- rast(support$grid_path); n <- length(support$cells); ns <- nrow(full)
key <- function(x)tolower(gsub('[._]+',' ',trimws(x)))
stopifnot(!anyDuplicated(key(full$species)))
read_values <- function(path){
  z <- readRDS(file.path(base,'input_cells',paste0(digest(path,algo='sha256'),'.rds')))
  stopifnot(z$metadata$path==path,z$metadata$file_bytes==file.info(path)$size,length(z$value)==n)
  mm <- z$metadata
  stopifnot(!grepl('TSSbin|TSSfilt',basename(path)))
  if(is.finite(mm$maximum))stopifnot(mm$minimum>=0,mm$maximum<=1000,mm$maximum>1)
  v <- z$value/1000
  stopifnot(all(v[is.finite(v)]>=0 & v[is.finite(v)]<=1))
  list(value=v,metadata=mm)
}
mask_cache <- file.path(out,'native_cell_membership.rds')
mask_hashes <- vapply(cfg$native_vectors,function(p)digest(file=p,algo='sha256'),character(1))
if(file.exists(mask_cache)){
  masks <- readRDS(mask_cache)
  stopifnot(identical(masks$cells,support$cells),identical(masks$species,full$species),identical(masks$vector_sha256,mask_hashes))
}else{
  polys <- lapply(cfg$native_vectors,function(p){v<-st_read(p,quiet=TRUE);v[,c('accepted_taxon','mapping_source','geom')]})
  polys <- do.call(rbind,polys)
  polys <- st_wrap_dateline(polys,options=c('WRAPDATELINE=YES'))
  dx <- res(template)[1]/2; dy <- res(template)[2]/2
  boxes <- st_sfc(lapply(seq_len(n),function(i){x<-support$xy[i,1];y<-support$xy[i,2];st_polygon(list(rbind(c(x-dx,y-dy),c(x+dx,y-dy),c(x+dx,y+dy),c(x-dx,y+dy),c(x-dx,y-dy))))}),crs=4326)
  cat('Matching saved native polygons to',n,'grid cells\n');flush.console()
  hits <- st_intersects(st_set_crs(polys,NA),st_set_crs(boxes,NA))
  native <- matrix(FALSE,n,ns); ix <- match(key(polys$accepted_taxon),key(full$species))
  for(i in which(!is.na(ix)))if(length(hits[[i]]))native[hits[[i]],ix[i]] <- TRUE
  mapping <- data.frame(species=full$species,native_vector_present=key(full$species)%in%key(polys$accepted_taxon),native_cells=colSums(native))
  masks <- list(cells=support$cells,species=full$species,native=native,mapping=mapping,vector_sha256=mask_hashes)
  saveRDS(masks,mask_cache,compress=FALSE)
  rm(polys,hits,boxes);gc()
}
write.csv(masks$mapping,file.path(out,'native_mapping_status.csv'),row.names=FALSE)
values <- matrix(NA_real_,n,ns); meta <- list()
for(i in seq_len(ns)){
  z<-read_values(full$path[i]);values[,i]<-z$value
  meta[[i]]<-data.frame(group='FULL_814',species=full$species[i],z$metadata,input_scale='0-1000',divisor=1000,output_scale='0-1')
}
full_pair <- env_pair <- matrix(NA_real_,n,nrow(paired))
for(i in seq_len(nrow(paired))){
  f<-read_values(paired$full_projection[i]);e<-read_values(paired$env_projection[i])
  full_pair[,i]<-f$value;env_pair[,i]<-e$value
  meta[[length(meta)+1L]]<-data.frame(group='PAIRED_FULL',species=paired$species[i],f$metadata,input_scale='0-1000',divisor=1000,output_scale='0-1')
  meta[[length(meta)+1L]]<-data.frame(group='PAIRED_ENV',species=paired$species[i],e$metadata,input_scale='0-1000',divisor=1000,output_scale='0-1')
}
write.csv(do.call(rbind,meta),file.path(out,'continuous_input_scales.csv'),row.names=FALSE)
pair_mask <- masks$native[,match(paired$species,full$species),drop=FALSE]
products <- list(); totals <- list()
build <- function(label,v,native,shared=NULL){
  native_n<-rowSums(native); eligible<-ncol(v)-native_n
  finite<-is.finite(v)&!native
  if(!is.null(shared))finite<-finite&shared
  observed<-rowSums(finite);missing<-eligible-observed
  z<-v;z[!finite]<-0;total<-rowSums(z);known<-native_n+observed
  # A missing prediction remains unknown. Bounds retain its possible 0-1 contribution.
  ans<-list(coverage_adjusted=ifelse(known>0,total/known,NA_real_),alien_mean=ifelse(observed>0,total/observed,NA_real_),fixed_lower=total/ncol(v),fixed_upper=(total+missing)/ncol(v),strict_fixed=ifelse(missing==0,total/ncol(v),NA_real_),unmasked_available=rowMeans(v,na.rm=TRUE))
  ans$unmasked_available[!is.finite(ans$unmasked_available)]<-NA_real_
  list(values=ans,counts=data.frame(native=native_n,eligible=eligible,observed=observed,missing=missing,known=known,pool=ncol(v)),sum=total)
}
products$FULL_814 <- build('FULL_814',values,masks$native)
joint <- is.finite(full_pair)&is.finite(env_pair)
products$PAIRED_FULL <- build('PAIRED_FULL',full_pair,pair_mask,joint)
products$PAIRED_ENV <- build('PAIRED_ENV',env_pair,pair_mask,joint)
stopifnot(identical(products$PAIRED_FULL$counts,products$PAIRED_ENV$counts))
saveRDS(list(cells=support$cells,products=products),file.path(out,'non_native_cell_values.rds'),compress=FALSE)
saveRDS(list(full=values,paired_full=full_pair,paired_env=env_pair,paired_species=paired$species),file.path(out,'scaled_species_values.rds'),compress=FALSE)
groups <- split(seq_len(nrow(support$weights)),interaction(support$weights$footprint,support$weights$city_id,support$weights$zone,drop=TRUE))
rows<-list();k<-0L
for(g in names(products))for(rule in names(products[[g]]$values))for(ix in groups){
  w<-support$weights[ix,];v<-products[[g]]$values[[rule]][w$cell_index];c<-products[[g]]$counts[w$cell_index,];ok<-is.finite(v)
  k<-k+1L;rows[[k]]<-data.frame(group=g,rule=rule,footprint=w$footprint[1],city_id=w$city_id[1],zone=w$zone[1],mean=if(any(ok))weighted.mean(v[ok],w$weight_km2[ok])else NA_real_,finite_area_km2=sum(w$weight_km2[ok]),total_area_km2=sum(w$weight_km2),native_species=weighted.mean(c$native,w$weight_km2),predicted_alien_species=weighted.mean(c$observed,w$weight_km2),missing_alien_species=weighted.mean(c$missing,w$weight_km2),unknown_fraction=weighted.mean(c$missing/c$pool,w$weight_km2),no_alien_prediction_area_km2=sum(w$weight_km2[c$observed==0]))
}
means<-do.call(rbind,rows);write.csv(means,file.path(out,'non_native_city_zone_means.csv'),row.names=FALSE)
results<-list();k<-0L;set.seed(cfg$seed)
for(g in names(products))for(rule in names(products[[g]]$values))for(foot in unique(means$footprint)){
  q<-means[means$group==g&means$rule==rule&means$footprint==foot,]
  r<-q[q$zone=='RECLAIMED',];o<-q[q$zone=='OTHER_URBAN',];t<-merge(r,o,by='city_id',suffixes=c('_rec','_other'))
  t<-t[is.finite(t$mean_rec)&is.finite(t$mean_other)&t$finite_area_km2_rec>0&t$finite_area_km2_other>0,]
  if(!nrow(t))next
  for(mode in c('EQUAL_CITY','POOLED_AREA')){
    calc<-function(ix){a<-t[ix,];if(mode=='EQUAL_CITY')c(mean(a$mean_rec),mean(a$mean_other))else c(weighted.mean(a$mean_rec,a$finite_area_km2_rec),weighted.mean(a$mean_other,a$finite_area_km2_other))}
    p<-calc(seq_len(nrow(t)));boot<-replicate(cfg$bootstrap_replicates,{v<-calc(sample.int(nrow(t),nrow(t),TRUE));c(v[1]-v[2],100*(v[1]/v[2]-1))})
    quant<-function(x,p){x<-x[is.finite(x)];if(length(x))unname(quantile(x,p))else NA_real_}
    k<-k+1L;results[[k]]<-data.frame(group=g,rule=rule,footprint=foot,weighting=mode,cities=nrow(t),reclaimed=p[1],other_urban=p[2],difference=p[1]-p[2],percent=100*(p[1]/p[2]-1),difference_lower=quant(boot[1,],.025),difference_upper=quant(boot[1,],.975),percent_lower=quant(boot[2,],.025),percent_upper=quant(boot[2,],.975))
  }
}
write.csv(do.call(rbind,results),file.path(out,'non_native_contrasts.csv'),row.names=FALSE)
bound_rows<-list();k<-0L
for(g in names(products))for(foot in unique(means$footprint)){
  x<-means[means$group==g&means$footprint==foot&means$rule%in%c('fixed_lower','fixed_upper'),]
  take<-function(rule,zone){q<-x[x$rule==rule&x$zone==zone,c('city_id','mean')];names(q)[2]<-paste(rule,zone,sep='_');q}
  a<-Reduce(function(x,y)merge(x,y,by='city_id'),list(take('fixed_lower','RECLAIMED'),take('fixed_upper','RECLAIMED'),take('fixed_lower','OTHER_URBAN'),take('fixed_upper','OTHER_URBAN')))
  k<-k+1L;bound_rows[[k]]<-data.frame(group=g,footprint=foot,cities=nrow(a),minimum_possible_difference=mean(a$fixed_lower_RECLAIMED-a$fixed_upper_OTHER_URBAN),maximum_possible_difference=mean(a$fixed_upper_RECLAIMED-a$fixed_lower_OTHER_URBAN))
}
write.csv(do.call(rbind,bound_rows),file.path(out,'missing_prediction_difference_bounds.csv'),row.names=FALSE)
legacy_rows<-list();legacy_concordance<-list();k<-0L
for(label in names(cfg$stored_surfaces)){
  r<-rast(cfg$stored_surfaces[[label]]);stopifnot(compareGeom(template,r,stopOnError=FALSE))
  v<-extract(r,support$xy);v<-as.numeric(v[[ncol(v)]])
  saved<-list();j<-0L
  for(ix in groups){w<-support$weights[ix,];x<-v[w$cell_index];ok<-is.finite(x);j<-j+1L;saved[[j]]<-data.frame(product=label,footprint=w$footprint[1],city_id=w$city_id[1],zone=w$zone[1],mean=if(any(ok))weighted.mean(x[ok],w$weight_km2[ok])else NA_real_,finite_area_km2=sum(w$weight_km2[ok]))}
  saved<-do.call(rbind,saved)
  for(foot in unique(saved$footprint)){
    q<-saved[saved$footprint==foot,];a<-merge(q[q$zone=='RECLAIMED',],q[q$zone=='OTHER_URBAN',],by='city_id',suffixes=c('_rec','_other'));a<-a[is.finite(a$mean_rec)&is.finite(a$mean_other),]
    if(nrow(a)){k<-k+1L;legacy_rows[[k]]<-data.frame(product=label,footprint=foot,cities=nrow(a),reclaimed=mean(a$mean_rec),other_urban=mean(a$mean_other),percent=100*(mean(a$mean_rec)/mean(a$mean_other)-1))}
  }
  ref<-products$FULL_814$values$alien_mean;ok<-is.finite(v)&is.finite(ref)
  legacy_concordance[[label]]<-data.frame(product=label,compared_cells=sum(ok),maximum_error_to_corrected_alien_mean=if(any(ok))max(abs(v[ok]-ref[ok]))else NA_real_,stored_missing_recomputed_finite=sum(!is.finite(v)&is.finite(ref)),stored_finite_recomputed_missing=sum(is.finite(v)&!is.finite(ref)))
}
write.csv(do.call(rbind,legacy_rows),file.path(out,'stored_surface_contrasts.csv'),row.names=FALSE)
write.csv(do.call(rbind,legacy_concordance),file.path(out,'stored_surface_concordance.csv'),row.names=FALSE)
sources<-unique(c(cfg$native_vectors,cfg$paired_manifest,file.path(base,c('support.rds','species_inputs.csv','raster_input_metadata.csv'))))
write.csv(data.frame(path=sources,sha256=vapply(sources,function(p)digest(file=p,algo='sha256'),character(1))),file.path(out,'input_checksums.csv'),row.names=FALSE)
write_json(list(status='COMPUTED',no_sdm_fitting=TRUE,cells=n,species_full=ns,species_paired=nrow(paired),native_mapping_unavailable=masks$mapping$species[!masks$mapping$native_vector_present],native_rule='Saved country polygons, touched grid cells excluded from alien contribution; taxa without mapped native geometry retain the documented no-exclusion treatment',coverage_adjusted_rule='Sum of known alien suitability divided by native plus predicted-alien species; unknown predictions excluded from denominator',bounds_rule='Fixed-pool contribution bounds, not absence imputations',support_scope='Existing all-city geometry, no new city or minimum-cell filter'),file.path(out,'non_native_receipt.json'),auto_unbox=TRUE,pretty=TRUE)
cat('COMPUTED non-native comparisons\n')
