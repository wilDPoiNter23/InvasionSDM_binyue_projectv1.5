# Check inclusive boundaries, NA masks and a single changed pixel in small blocks.
args <- commandArgs(TRUE)
if(length(args)!=1L)stop("Supply the optional exploratory module directory")
source(file.path(args[1],"binary_verify_rasters.R"))
td <- tempfile("binary_verify_blocks_");dir.create(td)
r <- rast(nrows=3,ncols=4,xmin=0,xmax=4,ymin=0,ymax=3,crs="EPSG:4326")
values(r) <- c(NA,0,499,500,501,1000,NA,500,0,1000,499,NA)
input <- file.path(td,"continuous.tif");writeRaster(r,input)
b <- r;values(b) <- ifelse(is.na(values(r)),NA,as.integer(values(r)>=500))
output <- file.path(td,"binary.tif");writeRaster(b,output,datatype="INT1U",NAflag=255)
ok <- binary_verify_file(input,output,500,block_rows=1L)
stopifnot(ok$status=="PASS",ok$finite_input==9,ok$finite_output==9,
  ok$occupied_cells==5,ok$full_na_difference==0,ok$full_value_difference==0,
  ok$sample_n==12,ok$sample_na_pass,ok$sample_values_pass)
bad <- b;v <- values(bad);v[4] <- 0;values(bad) <- v
altered <- file.path(td,"one_changed_pixel.tif");writeRaster(bad,altered,datatype="INT1U",NAflag=255)
fail <- binary_verify_file(input,altered,500,block_rows=2L)
stopifnot(fail$status=="FAIL",fail$full_value_difference==1,fail$full_na_difference==0)
bad_na <- b;v <- values(bad_na);v[1] <- 0;values(bad_na) <- v
na_changed <- file.path(td,"changed_na_mask.tif");writeRaster(bad_na,na_changed,datatype="INT1U",NAflag=255)
fail_na <- binary_verify_file(input,na_changed,500,block_rows=1L)
stopifnot(fail_na$status=="FAIL",fail_na$full_na_difference==1,fail_na$finite_output==10)
zero <- r;values(zero) <- ifelse(is.na(values(r)),NA,1L)
zero_path <- file.path(td,"cutoff_zero.tif");writeRaster(zero,zero_path,datatype="INT1U",NAflag=255)
ok_zero <- binary_verify_file(input,zero_path,0,block_rows=2L)
stopifnot(ok_zero$status=="PASS",ok_zero$occupied_cells==9)
cat("Block verifier passed: boundary/NA/zero fixtures; changed pixel and NA mask rejected\n")
