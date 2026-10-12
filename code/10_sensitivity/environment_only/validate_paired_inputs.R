#!/usr/bin/env Rscript
# Check stored completion records and continuous input geometry before aggregation.
suppressPackageStartupMessages(library(terra))
if(!requireNamespace('digest',quietly=TRUE)) stop('Install digest for stored-record fingerprints')
args <- commandArgs(TRUE)
if (length(args) != 2L) stop('Usage: Rscript validate_paired_inputs.R MANIFEST OUTPUT_CSV')
if (file.exists(args[2])) stop('The validation output already exists')
m <- read.csv(args[1], stringsAsFactors=FALSE, check.names=FALSE)
stopifnot(nrow(m)>0L, !anyDuplicated(m$species))
reference <- rast(m$full_projection[1])
check_record <- function(q) {
  required <- c('taxon','variant','scale','PA_CV_identical','max_independent_delta','nonNA')
  stopifnot(all(required %in% names(q)),all(vapply(q[required],length,integer(1))==1L),
            identical(q$scale,'BIOMOD_0_1000'),isTRUE(q$PA_CV_identical),
            is.finite(q$max_independent_delta),q$max_independent_delta>=0,q$max_independent_delta<=1.5,
            is.finite(q$nonNA),q$nonNA>1000)
  selected <- if(length(q$selected_models)) q$selected_models else q$selected_model
  stopifnot(length(selected)>0,all(!is.na(selected)),all(nzchar(selected)))
  selected
}
check_fingerprint <- function(path,expected) {
  stopifnot(length(expected)==1L,nchar(expected)==64L,
            identical(digest::digest(file=path,algo='sha256'),expected))
}
checks <- vector('list', nrow(m))
for (i in seq_len(nrow(m))) {
  row <- m[i, ]
  check_fingerprint(row$env_receipt,row$env_receipt_sha256)
  qe <- readRDS(row$env_receipt)
  selected <- check_record(qe)
  identity_pass <- identical(qe$taxon,row$species) && identical(qe$variant,row$env_model_type)
  if(!identity_pass) {
    # Older records split a two-word taxon across the identity fields.
    design <- readRDS(file.path(dirname(row$env_receipt),'design_before.rds'))
    stopifnot(identical(paste(qe$taxon,qe$variant),row$species),
              identical(design$taxon,row$species), identical(design$variant,row$env_model_type),
              identical(design$predictors,qe$predictors))
    prefix <- paste0(gsub(' ','.',row$species,fixed=TRUE),'_')
    stopifnot(all(startsWith(selected,prefix)))
    identity_pass <- TRUE
  }
  stopifnot(identity_pass,
            qe$scale=='BIOMOD_0_1000', isTRUE(qe$PA_CV_identical),
            is.finite(qe$max_independent_delta), qe$max_independent_delta<=1.5,
            qe$nonNA>1000, length(selected)>0)
  if (nzchar(row$full_receipt)) {
    check_fingerprint(row$full_receipt,row$full_receipt_sha256)
    check_fingerprint(row$pair_receipt,row$pair_receipt_sha256)
    qf <- readRDS(row$full_receipt)
    check_record(qf)
    qp <- readRDS(row$pair_receipt)
    stopifnot(identical(qf$taxon,row$species), qf$variant=='CLEAN_FULL',
              qf$scale=='BIOMOD_0_1000', isTRUE(qf$PA_CV_identical),
              is.finite(qf$max_independent_delta), qf$max_independent_delta<=1.5,
              identical(qp$species,row$species), isTRUE(qp$PA_CV_identical), isTRUE(qp$ID_absent), isTRUE(qp$both_routes_validated))
  }
  full <- rast(row$full_projection); env <- rast(row$env_projection)
  stopifnot(nlyr(full)==1, nlyr(env)==1,
            compareGeom(reference,full,env,stopOnError=FALSE),
            !grepl('TSSbin|TSSfilt|binary',basename(row$full_projection),ignore.case=TRUE))
  checks[[i]] <- data.frame(species=row$species, stored_ENV_check_pass=TRUE,
                            stored_pair_check_pass=if(nzchar(row$pair_receipt)) TRUE else NA, geometry_pass=TRUE,
                            baseline_FULL_source=if(nzchar(row$full_receipt)) 'paired clean fit' else 'accepted baseline inventory')
}
write.csv(do.call(rbind,checks),args[2],row.names=FALSE)
cat('Checked',nrow(m),'paired inputs.\n')
