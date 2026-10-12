# Original source: test_extract814.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/test_extract814.R
# Original source SHA-256: 4f220de583c7fe0908423bf0d34b6f7ad59ac47b75ab62010a533c9e4ee219d1
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c('<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu',.libPaths()))
suppressPackageStartupMessages(library(terra))
r=rast(nrows=2,ncols=2,xmin=0,xmax=2,ymin=0,ymax=2);values(r)=c(1,2,3,4)
xy=xyFromCell(r,c(1,4));z=terra::extract(r,as.matrix(xy));stopifnot(ncol(z)==1,identical(as.numeric(z[,1]),c(1,4)))
cat('COORDINATE_MATRIX_EXTRACTION_PASS\n')
