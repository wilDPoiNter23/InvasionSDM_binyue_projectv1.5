# Independent geometry, coverage, inclusive-boundary and checksum verification.
# Input and binary values are decoded once in bounded sequential row blocks.
Sys.setenv(GDAL_CACHEMAX="64")
suppressPackageStartupMessages({library(terra);library(digest)})

binary_verify_file <- function(input_path,output_path,cutoff,block_rows=64L) {
  if(!is.finite(cutoff))stop("Finite threshold required")
  if(length(block_rows)!=1L || is.na(block_rows) || block_rows<1L)stop("Positive block rows required")
  r <- rast(input_path);b <- rast(output_path)
  geometry <- nlyr(r)==1L && nlyr(b)==1L && compareGeom(r,b,stopOnError=FALSE)
  if(nlyr(r)!=1L || nlyr(b)!=1L || nrow(r)!=nrow(b) || ncol(r)!=ncol(b))stop("Matching single-layer dimensions required")
  cells <- unique(as.integer(round(seq(1,ncell(r),length.out=min(50001,ncell(r))))))
  sampled_input <- rep(NA_real_,length(cells));sampled_output <- sampled_input
  cursor <- 1L;finite_input <- 0;finite_output <- 0;occupied <- 0
  full_na_difference <- 0;wrong <- 0;binary_min <- Inf;binary_max <- -Inf
  open_r <- FALSE;open_b <- FALSE
  on.exit({if(open_b)readStop(b);if(open_r)readStop(r)},add=TRUE)
  readStart(r);open_r <- TRUE;readStart(b);open_b <- TRUE
  for(first_row in seq.int(1L,nrow(r),by=as.integer(block_rows))) {
    count <- min(as.integer(block_rows),nrow(r)-first_row+1L)
    rv <- readValues(r,row=first_row,nrows=count,mat=FALSE)
    bv <- readValues(b,row=first_row,nrows=count,mat=FALSE)
    if(length(rv)!=length(bv))stop("Decoded block dimensions differ")
    input_na <- is.na(rv);output_na <- is.na(bv)
    finite_input <- finite_input+sum(!input_na);finite_output <- finite_output+sum(!output_na)
    full_na_difference <- full_na_difference+sum(input_na!=output_na)
    both <- !input_na & !output_na
    wrong <- wrong+sum(bv[both]!=(rv[both]>=cutoff))
    decoded <- bv[!output_na]
    if(length(decoded)) {
      occupied <- occupied+sum(decoded)
      binary_min <- min(binary_min,min(decoded));binary_max <- max(binary_max,max(decoded))
    }
    first_cell <- (first_row-1L)*ncol(r)+1
    last_cell <- first_cell+length(rv)-1
    final_sample <- findInterval(last_cell,cells)
    if(cursor<=final_sample) {
      sample_indices <- seq.int(cursor,final_sample)
      local_cells <- cells[sample_indices]-first_cell+1
      sampled_input[sample_indices] <- rv[local_cells]
      sampled_output[sample_indices] <- bv[local_cells]
      cursor <- final_sample+1L
    }
  }
  readStop(b);open_b <- FALSE;readStop(r);open_r <- FALSE
  if(cursor!=length(cells)+1L)stop("Deterministic samples were not all collected")
  sampled_na <- identical(is.na(sampled_input),is.na(sampled_output))
  present <- !is.na(sampled_input)
  sampled_values <- all(!is.na(sampled_output[present]) & sampled_output[present]==as.integer(sampled_input[present]>=cutoff))
  binary_range <- finite_output>0 && binary_min>=0 && binary_max<=1
  pass <- geometry && sampled_na && sampled_values && binary_range && full_na_difference==0 && finite_input==finite_output && wrong==0
  backing <- if(grepl("[.]grd$",input_path,ignore.case=TRUE))sub("[.]grd$",".gri",input_path,ignore.case=TRUE) else input_path
  if(!file.exists(backing))stop("Continuous raster backing file missing")
  projection_hash <- digest(file=input_path,algo="sha256")
  backing_hash <- if(identical(backing,input_path))projection_hash else digest(file=backing,algo="sha256")
  data.frame(projection_path=input_path,output_path=output_path,cutoff_raw=cutoff,
    geometry_pass=geometry,sample_n=length(cells),sample_na_pass=sampled_na,sample_values_pass=sampled_values,
    finite_input=finite_input,finite_output=finite_output,occupied_cells=occupied,
    full_na_difference=full_na_difference,full_value_difference=wrong,status=if(pass)"PASS"else"FAIL",
    projection_sha256=projection_hash,projection_backing_path=backing,
    projection_backing_sha256=backing_hash,binary_sha256=digest(file=output_path,algo="sha256"))
}

if(sys.nframe()==0L) {
  args <- commandArgs(TRUE)
  if(length(args)!=2L)stop("Supply conversion manifest and new verification table")
  if(file.exists(args[2]))stop("Verification output exists")
  d <- read.csv(args[1],stringsAsFactors=FALSE)
  required <- c("projection_path","output_path","cutoff_raw")
  if(!all(required %in% names(d)))stop("Missing conversion manifest fields")
  terraOptions(memfrac=.1,progress=0)
  result <- lapply(seq_len(nrow(d)),function(i)binary_verify_file(d$projection_path[i],d$output_path[i],d$cutoff_raw[i]))
  result <- do.call(rbind,result);write.csv(result,args[2],row.names=FALSE)
  if(any(result$status!="PASS"))stop("Binary verification failed")
}
