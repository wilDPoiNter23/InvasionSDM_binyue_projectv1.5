# Block-wise binary conversion of existing continuous products.
# Input: CSV projection_path, cutoff_raw, output_path; original maps are retained.
# Output: binary GeoTIFFs and per-product area summary.
suppressPackageStartupMessages(library(terra))

binary_convert_one <- function(input_path, cutoff, output_path, scale_divisor=1000) {
  if (!is.finite(cutoff) || cutoff<0 || cutoff>scale_divisor) stop("Invalid cutoff units")
  if (file.exists(output_path)) stop("Output exists")
  r <- rast(input_path)
  if(nlyr(r)!=1L) stop("One continuous product required")
  if(grepl("TSSbin|TSSfilt",input_path)) stop("Binary input forbidden")
  bounds <- global(r,c("min","max"),na.rm=TRUE)
  if(any(!is.finite(as.matrix(bounds))) || bounds$min<0 || bounds$max>scale_divisor) stop("Invalid continuous score range")
  dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
  # Terra evaluates this expression in disk-backed blocks under its memory limit.
  b <- ifel(r>=cutoff,1,0,filename=output_path,overwrite=FALSE,
            wopt=list(datatype="INT1U",NAflag=255,gdal=c("COMPRESS=DEFLATE")))
  if(!compareGeom(r,b,stopOnError=FALSE)) stop("Output geometry differs")
  totals <- global(b,c("sum","notNA"),na.rm=TRUE)
  area <- expanse(b,unit="km",byValue=TRUE,transform=TRUE)
  occupied <- if(any(area$value==1)) sum(area$area[area$value==1]) else 0
  data.frame(projection_path=input_path,output_path=output_path,cutoff_raw=cutoff,
    scale_divisor=scale_divisor,occupied_cells=totals$sum,
    finite_cells=totals$notNA,occupied_area_km2=occupied)
}

if(sys.nframe()==0L) {
  args <- commandArgs(TRUE)
  if(!length(args) %in% c(2L,4L)) stop("Usage: binary_convert.R manifest.csv area_summary.csv [first_row count]")
  if(file.exists(args[2])) stop("Summary exists")
  d <- read.csv(args[1],stringsAsFactors=FALSE)
  if(!all(c("projection_path","cutoff_raw","output_path") %in% names(d))) stop("Missing manifest fields")
  if(anyDuplicated(d$output_path)) stop("Duplicate output paths")
  if(length(args)==4L) {
    first <- as.integer(args[3]);count <- as.integer(args[4])
    if(is.na(first)||is.na(count)||first<1L||count<1L||first>nrow(d))stop("Invalid row range")
    d <- d[seq.int(first,min(nrow(d),first+count-1L)),,drop=FALSE]
  }
  terraOptions(memfrac=.25,progress=0)
  z <- lapply(seq_len(nrow(d)),function(i) binary_convert_one(d$projection_path[i],d$cutoff_raw[i],d$output_path[i],if("scale_divisor" %in% names(d))d$scale_divisor[i] else 1000))
  write.csv(do.call(rbind,z),args[2],row.names=FALSE)
}
