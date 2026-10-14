#!/usr/bin/env Rscript
# Screen reported coastal change categories at native suitability resolution.
suppressPackageStartupMessages({library(sf); library(terra); library(jsonlite); library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L) stop("Supply a private JSON configuration")
cfg <- fromJSON(args[1], simplifyVector=FALSE)
out <- cfg$screen_output
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,"screen_complete.json"))) stop("Existing screen preserved")
sf_use_s2(FALSE)
cities <- st_read(cfg$cities,quiet=TRUE)
stopifnot(nrow(cities)==108L, cfg$city_id %in% names(cities))
cities <- st_make_valid(cities)
cities <- st_transform(cities,6933)
cities$city_id <- as.character(cities[[cfg$city_id]])
city_area <- as.numeric(st_area(cities))/1e6
rows <- list(); coverage <- list(); geometry <- list(); receipts <- list()
for(id in c("coastal_change","coastal_landuse")) {
  cat("Reading",id,"\n"); flush.console()
  x <- st_read(cfg$layers[[id]],quiet=TRUE)
  field <- if(id=="coastal_change") "reclass1" else "class"
  stopifnot(field %in% names(x))
  x$reported_category <- as.character(x[[field]])
  input_features <- nrow(x)
  cat("Selecting city windows",id,"\n"); flush.console()
  x <- suppressWarnings(st_cast(x,"POLYGON"))
  windows <- st_transform(cities,st_crs(x))
  bounds <- t(vapply(st_geometry(x),function(g)as.numeric(st_bbox(g)),numeric(4)))
  selected <- rep(FALSE,nrow(x))
  for(i in seq_len(nrow(windows))) {
    b <- st_bbox(windows[i,])
    selected <- selected | (bounds[,1]<=b[3] & bounds[,3]>=b[1] &
      bounds[,2]<=b[4] & bounds[,4]>=b[2])
  }
  x <- x[selected,]
  cat("Candidate polygon parts",nrow(x),"\n"); flush.console()
  original_valid <- st_is_valid(x)
  x <- st_transform(st_make_valid(x),6933)
  cat("Intersecting",id,"with cities\n"); flush.console()
  bounds <- t(vapply(st_geometry(x),function(g)as.numeric(st_bbox(g)),numeric(4)))
  checkpoint_dir <- file.path(out,"geometry_checkpoints",id)
  dir.create(checkpoint_dir,recursive=TRUE,showWarnings=FALSE)
  contract <- digest(list(city_ids=cities$city_id,city_geometry=st_geometry(cities),
    layer=digest(file=cfg$layers[[id]],algo="sha256"),
    attributes=digest(file=sub("[.]shp$",".dbf",cfg$layers[[id]]),algo="sha256")),algo="sha256")
  contract_file <- file.path(checkpoint_dir,"input_contract.txt")
  if(file.exists(contract_file) && readLines(contract_file,warn=FALSE)[1]!=contract)
    stop("Existing geometry checkpoints use different inputs")
  if(!file.exists(contract_file))writeLines(contract,contract_file)
  workers <- max(1L,as.integer(Sys.getenv("SLURM_CPUS_PER_TASK","1")))
  city_parts <- parallel::mclapply(seq_len(nrow(cities)),function(i) {
    cache <- file.path(checkpoint_dir,sprintf("city_%03d.rds",i))
    if(file.exists(cache)) return(readRDS(cache))
    b <- st_bbox(cities[i,])
    hit <- which(bounds[,1]<=b[3] & bounds[,3]>=b[1] & bounds[,2]<=b[4] & bounds[,4]>=b[2])
    if(!length(hit))return(NULL)
    cat("City",i,id,"parts",length(hit),"\n"); flush.console()
    q <- suppressWarnings(st_intersection(cities[i,"city_id",drop=FALSE],x[hit,"reported_category",drop=FALSE]))
    q <- suppressWarnings(st_collection_extract(q,"POLYGON"))
    if(!nrow(q))return(NULL)
    groups <- split(seq_len(nrow(q)),q$reported_category)
    attrs <- st_drop_geometry(q)[vapply(groups,function(ix)ix[1],integer(1)),]
    merged <- st_sfc(lapply(groups,function(ix)st_union(st_geometry(q)[ix])[[1]]),crs=6933)
    ans <- st_sf(attrs,geometry=merged)
    attr(ans,"unique_mapped_km2") <- as.numeric(st_area(st_union(st_geometry(ans))))/1e6
    saveRDS(ans,paste0(cache,".part"))
    if(!file.rename(paste0(cache,".part"),cache))stop("Checkpoint rename failed")
    ans
  },mc.cores=workers,mc.preschedule=FALSE)
  if(any(vapply(city_parts,inherits,logical(1),"try-error")))stop("A city geometry worker failed")
  city_parts <- Filter(Negate(is.null),city_parts)
  if(!length(city_parts))stop("No mapped city intersection")
  z <- do.call(rbind,city_parts)
  unique_areas <- setNames(vapply(city_parts,function(q)attr(q,"unique_mapped_km2"),numeric(1)),
    vapply(city_parts,function(q)q$city_id[1],character(1)))
  z$area_km2 <- as.numeric(st_area(z))/1e6
  z <- z[z$area_km2>0,]
  write.csv(st_drop_geometry(z),file.path(out,paste0(id,"_city_categories.csv")),row.names=FALSE)
  coverage[[id]] <- do.call(rbind,lapply(seq_len(nrow(cities)),function(i) {
    q <- z[z$city_id==cities$city_id[i],]
    unique_area <- if(nrow(q)) unique_areas[[cities$city_id[i]]] else 0
    summed_area <- sum(q$area_km2)
    data.frame(layer=id,city_id=cities$city_id[i],city_area_km2=city_area[i],
      category_count=length(unique(q$reported_category)),mapped_area_km2=unique_area,
      summed_category_area_km2=summed_area,overlap_excess_km2=summed_area-unique_area,
      mapped_fraction=unique_area/city_area[i])
  }))
  geometry[[id]] <- data.frame(layer=id,input_features=input_features,candidate_parts=nrow(x),invalid_input=sum(!original_valid),
    city_intersections=nrow(z),cities_covered=length(unique(z$city_id)))
  if(!nrow(z)) next
  for(response in names(cfg$responses)) {
    cat("Extracting",id,response,"\n"); flush.console()
    r <- rast(cfg$responses[[response]])
    stopifnot(nlyr(r)==1L,abs(res(r)[1]-1/24)<1e-8,abs(res(r)[2]-1/24)<1e-8)
    e <- terra::extract(r,vect(st_transform(z,crs(r))),exact=TRUE,cells=TRUE)
    value_field <- names(r)[1]
    weight_field <- intersect(c("fraction","weight"),names(e))
    if(length(weight_field)!=1L) stop("Exact extraction fraction unavailable")
    vals <- lapply(seq_len(nrow(z)),function(i) {
      d <- e[e$ID==i & is.finite(e[[value_field]]) & is.finite(e[[weight_field]]),]
      w <- d[[weight_field]]; y <- d[[value_field]]
      data.frame(layer=id,response=response,city_id=z$city_id[i],reported_category=z$reported_category[i],
        mapped_area_km2=z$area_km2[i],native_cells=length(unique(d$cell)),
        covered_cell_equivalents=sum(w),effective_cells=if(sum(w^2)>0)sum(w)^2/sum(w^2) else 0,
        mean_suitability=if(sum(w)>0)sum(w*y)/sum(w) else NA_real_)
    })
    rows[[paste(id,response)]] <- do.call(rbind,vals)
  }
}
write.csv(do.call(rbind,rows),file.path(out,"native_cell_category_suitability.csv"),row.names=FALSE)
write.csv(do.call(rbind,coverage),file.path(out,"city_coverage.csv"),row.names=FALSE)
write.csv(do.call(rbind,geometry),file.path(out,"geometry_checks.csv"),row.names=FALSE)
paths <- c(cfg$cities,unlist(cfg$layers),unlist(cfg$responses))
write.csv(data.frame(path=paths,sha256=vapply(paths,function(p)digest(file=p,algo="sha256"),character(1))),
  file.path(out,"input_checksums.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
write_json(list(status="SPATIAL_SCREEN_ONLY",year_field="reclass1",calendar_semantics_verified=FALSE,
  no_age_or_lag_model_fitted=TRUE,native_grid_resampling=FALSE,
  reason="Source calendar dictionary and mapped land definition must be verified before temporal interpretation"),
  file.path(out,"screen_complete.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Spatial screen complete; calendar interpretation remains conditional.\n")
