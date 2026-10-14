# Check cached-cell extraction with unequal missingness between paired maps.
suppressPackageStartupMessages({library(terra);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=1L)stop("Supply the optional exploratory module directory")
module_directory <- args[1]
td <- tempfile("binary_city_fixture_");dir.create(td)
full <- rast(nrows=2,ncols=3,xmin=0,xmax=3,ymin=0,ymax=2,crs="EPSG:4326")
values(full) <- c(NA,0,200,400,600,800)
env <- full;values(env) <- c(NA,10,NA,600,300,800)
fpath <- file.path(td,"full.tif");epath <- file.path(td,"env.tif")
writeRaster(full,fpath);writeRaster(env,epath)
pairs <- data.frame(species=sprintf("Species_%03d",1:684),full_projection=fpath,
  env_projection=epath,full_scale_divisor=1000,env_scale_divisor=1000)
pp <- file.path(td,"pairs.csv");write.csv(pairs,pp,row.names=FALSE)
thresholds <- data.frame(species="Species_001",group=c("FULL","ENV"),
  projection_path=c(fpath,epath),t10_raw=400,max_tss_raw=600)
tp <- file.path(td,"thresholds.csv");write.csv(thresholds,tp,row.names=FALSE)
sd <- file.path(td,"support");dir.create(sd)
support <- list(regions=data.frame(city_id=sprintf("CITY_%03d",1:108),region="CITY"),
  gates=data.frame(city_id=sprintf("CITY_%03d",1:108),status="SYNTHETIC"),
  weights=data.frame(ID=rep(1:108,each=6),cell=rep(1:6,108),fraction=1,
    area_km2=1,cell_index=rep(1:6,108)),cells=1:6,
  grid=list(nrows=2,ncols=3,xmin=0,xmax=3,ymin=0,ymax=2,crs=crs(full)),
  paired_manifest_sha256=digest(file=pp,algo="sha256"))
sp <- file.path(sd,"support.rds");saveRDS(support,sp)
write_json(list(support_sha256=digest(file=sp,algo="sha256")),
  file.path(sd,"support_receipt.json"),auto_unbox=TRUE)
cfg <- list(paired_manifest=pp,thresholds=tp,city_support_directory=sd,
  output_directory=file.path(td,"output"),first_species=1L,count=1L)
config_file <- file.path(td,"config.json");write_json(cfg,config_file,auto_unbox=TRUE)
code <- readLines(file.path(module_directory,"binary_city_extract.R"))
code[code=="args <- commandArgs(TRUE)"] <- "args <- c(config_file)"
eval(parse(text=code),envir=environment())
ans <- read.csv(file.path(cfg$output_directory,"city_species_threshold_area.csv"))
stopifnot(nrow(ans)==432L,all(ans$common_finite_cells==4),all(ans$common_finite_area_km2==4))
stopifnot(all(ans$suitable_fraction[ans$group=="FULL" & ans$method=="t10"]==.75),
  all(ans$suitable_fraction[ans$group=="ENV" & ans$method=="t10"]==.5))
cat("Cached binary city extraction passed:432 rows, paired common support4\n")
