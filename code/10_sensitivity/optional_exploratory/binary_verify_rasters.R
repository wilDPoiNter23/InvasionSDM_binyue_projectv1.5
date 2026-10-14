# Independent geometry, coverage, inclusive-boundary and checksum verification.
suppressPackageStartupMessages({library(terra);library(digest)})
args <- commandArgs(TRUE)
if(length(args)!=2L)stop("Supply conversion manifest and new verification table")
if(file.exists(args[2]))stop("Verification output exists")
d <- read.csv(args[1],stringsAsFactors=FALSE)
required <- c("projection_path","output_path","cutoff_raw")
if(!all(required %in% names(d)))stop("Missing conversion manifest fields")
terraOptions(memfrac=.25,progress=0)
result <- lapply(seq_len(nrow(d)),function(i) {
  r <- rast(d$projection_path[i]); b <- rast(d$output_path[i])
  geometry <- nlyr(r)==1L && nlyr(b)==1L && compareGeom(r,b,stopOnError=FALSE)
  cells <- unique(as.integer(round(seq(1,ncell(r),length.out=min(50001,ncell(r))))))
  rv <- extract(r,cells)[[1]]; bv <- extract(b,cells)[[1]]
  sampled_na <- identical(is.na(rv),is.na(bv))
  sampled_values <- all(bv[!is.na(rv)]==as.integer(rv[!is.na(rv)]>=d$cutoff_raw[i]))
  full_na_difference <- global(is.na(r)!=is.na(b),"sum",na.rm=TRUE)[[1]]
  range <- global(b,c("min","max"),na.rm=TRUE)
  binary_range <- range$min>=0 && range$max<=1
  finite_input <- global(!is.na(r),"sum",na.rm=TRUE)[[1]]
  finite_output <- global(!is.na(b),"sum",na.rm=TRUE)[[1]]
  occupied <- global(b,"sum",na.rm=TRUE)[[1]]
  # A full block-wise numerical comparison supplements deterministic samples.
  wrong <- global(ifel(is.na(r),NA,b!=(r>=d$cutoff_raw[i])),"sum",na.rm=TRUE)[[1]]
  pass <- geometry && sampled_na && sampled_values && binary_range && full_na_difference==0 && finite_input==finite_output && wrong==0
  backing <- if(grepl("[.]grd$",d$projection_path[i],ignore.case=TRUE))sub("[.]grd$",".gri",d$projection_path[i],ignore.case=TRUE) else d$projection_path[i]
  if(!file.exists(backing))stop("Continuous raster backing file missing")
  data.frame(projection_path=d$projection_path[i],output_path=d$output_path[i],cutoff_raw=d$cutoff_raw[i],
    geometry_pass=geometry,sample_n=length(cells),sample_na_pass=sampled_na,sample_values_pass=sampled_values,
    finite_input=finite_input,finite_output=finite_output,occupied_cells=occupied,
    full_na_difference=full_na_difference,full_value_difference=wrong,status=if(pass)"PASS"else"FAIL",
    projection_sha256=digest(file=d$projection_path[i],algo="sha256"),
    projection_backing_path=backing,projection_backing_sha256=digest(file=backing,algo="sha256"),
    binary_sha256=digest(file=d$output_path[i],algo="sha256"))
})
result <- do.call(rbind,result);write.csv(result,args[2],row.names=FALSE)
if(any(result$status!="PASS"))stop("Binary verification failed")
