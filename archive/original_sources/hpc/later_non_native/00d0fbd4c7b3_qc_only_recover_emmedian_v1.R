# Original source: qc_only_recover_emmedian_v1.R
# Original source location: ${NON_NATIVE_ROOT}/audit/environment_only_sensitivity/review_triage/qc_only_recover_emmedian_v1.R
# Original source SHA-256: 00d0fbd4c7b3a4c852913e12f3a3b1043b939ca02081cd747f28cd0b004e9e61
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_ENVIRONMENT_ONLY_SENSITIVITY)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# QC-only recovery for frozen ENV projections whose selected BIOMOD2 ensemble is EMmedian.
# This script never refits, reselects, changes predictors, or rebuilds a projection.
# Run only in a reserved allocation coordinated by the authoritative controller.

suppressPackageStartupMessages({library(biomod2); library(terra); library(jsonlite)})

qc_reduce_biomod2_4261 <- function(predictions, ensemble_name, on_0_1000 = TRUE) {
  x <- as.matrix(predictions)
  if (ncol(x) < 1L) stop('No selected members supplied')
  if (grepl('EMmedian', ensemble_name, fixed = TRUE)) {
    out <- apply(x, 1L, stats::median, na.rm = TRUE)
  } else if (grepl('EMmean', ensemble_name, fixed = TRUE)) {
    out <- rowMeans(x, na.rm = TRUE)
  } else if (ncol(x) == 1L) {
    out <- x[, 1L]
  } else {
    stop('Unsupported BIOMOD2 ensemble method: ', ensemble_name)
  }
  all_missing <- rowSums(is.finite(x)) == 0L
  if (isTRUE(on_0_1000)) out <- round(out)
  out[all_missing] <- NA_real_
  as.numeric(out)
}

# Guard exact BIOMOD2 4.2.6.1 semantics, including an even-member median.
stopifnot(identical(qc_reduce_biomod2_4261(matrix(c(1, 2, 10, 11), nrow=1), 'x_EMmedian', TRUE), 6))
stopifnot(identical(qc_reduce_biomod2_4261(matrix(c(3, 4, 20), nrow=1), 'x_EMmedian', TRUE), 4))
stopifnot(identical(qc_reduce_biomod2_4261(matrix(c(0, 1, 2), nrow=1), 'x_EMmean', TRUE), 1))
stopifnot(is.na(qc_reduce_biomod2_4261(matrix(c(NA_real_, NA_real_), nrow=1), 'x_EMmedian', TRUE)))

atomic_write_rds <- function(x, path) {
  tmp <- paste0(path, '.staging')
  saveRDS(x, tmp)
  if (!file.rename(tmp, path)) stop('Could not atomically publish ', path)
}

atomic_write_json <- function(x, path) {
  tmp <- paste0(path, '.staging')
  write_json(x, tmp, auto_unbox=TRUE, pretty=TRUE, na='null')
  if (!file.rename(tmp, path)) stop('Could not atomically publish ', path)
}

run_qc_only_recovery <- function(root, taxon, variant='ENV', job_id=Sys.getenv('SLURM_JOB_ID')) {
  root <- normalizePath(root, mustWork=TRUE)
  if (dir.exists(file.path(root, 'ACTIVE_OWNER'))) stop('Active task owner present; do not run QC recovery')
  if (file.exists(file.path(root, 'VALIDATED.rds'))) stop('Validated receipt already exists; do not rerun QC')
  required <- c('FIT_VALIDATED.rds','models.rds','evaluation.csv','selection.csv',
                'ensemble.rds','ensemble_contract.rds','projection_object.rds',
                'projection_contract.rds','ensemble_suitability.tif','writer_contract.rds')
  missing <- required[!file.exists(file.path(root, required))]
  if (length(missing)) stop('QC-only recovery missing required artifacts: ', paste(missing, collapse=', '))
  if (as.character(packageVersion('biomod2')) != '4.2.6.1') stop('Unexpected BIOMOD2 version')
  setwd(root)
  started <- as.numeric(Sys.time())
  m <- readRDS('models.rds')
  ens <- readRDS('ensemble.rds')
  writer <- readRDS('writer_contract.rds')
  fit <- readRDS('FIT_VALIDATED.rds')
  projection_contract <- readRDS('projection_contract.rds')
  ensemble_contract <- readRDS('ensemble_contract.rds')
  selected <- read.csv('selection.csv', stringsAsFactors=FALSE)$model
  chosen <- writer$chosen_ensemble
  if (!grepl('EMmedian', chosen, fixed=TRUE)) stop('This QC-only recovery is scoped to EMmedian')
  if (!setequal(selected, ensemble_contract$selected)) stop('Frozen selection receipt differs from ensemble contract')

  loaded_ensemble <- BIOMOD_LoadModels(ens, full.name=chosen)
  ensemble_model <- get(loaded_ensemble[1L])
  members <- slot(ensemble_model, 'model')
  if (!length(members) || !all(members %in% selected)) stop('Exact TSS-selected members do not match the frozen selected set')

  stack <- rast(projection_contract$stack)[[m@expl.var.names]]
  out <- rast('ensemble_suitability.tif')
  reference <- rast(writer$fixed_domain)
  stopifnot(identical(names(stack), m@expl.var.names),
            compareGeom(out, stack[[1L]], stopOnError=FALSE),
            compareGeom(out, reference, stopOnError=FALSE))
  range <- global(out, c('min','max'), na.rm=TRUE)
  non_na <- global(!is.na(out), 'sum', na.rm=TRUE)[1L,1L]
  stopifnot(is.finite(range[1L,1L]), is.finite(range[1L,2L]),
            range[1L,1L] >= 0, range[1L,2L] <= 1000.01,
            range[1L,2L] > range[1L,1L], non_na > 1000)
  missing_cells <- global(ifel(!is.na(reference) & is.na(out),1,0), 'sum', na.rm=TRUE)[1L,1L]
  extra_cells <- global(ifel(is.na(reference) & !is.na(out),1,0), 'sum', na.rm=TRUE)[1L,1L]
  infinite_cells <- global(is.infinite(out), 'sum', na.rm=TRUE)[1L,1L]
  stopifnot(missing_cells == 0, extra_cells == 0, infinite_cells == 0)

  sites <- as.data.frame(spatSample(out, 10000, method='regular', na.rm=TRUE, xy=TRUE, values=TRUE))
  sites <- sites[is.finite(sites[[names(out)[1L]]]), c('x','y'), drop=FALSE]
  newdata <- terra::extract(stack, as.matrix(sites[,c('x','y')]))
  ok <- complete.cases(newdata)
  newdata <- newdata[ok,,drop=FALSE]
  sites <- sites[ok,,drop=FALSE]
  stopifnot(nrow(newdata) >= 10L)
  take <- unique(round(seq(1, nrow(newdata), length.out=min(200L,nrow(newdata)))))
  newdata <- newdata[take,,drop=FALSE]
  sites <- sites[take,,drop=FALSE]
  independent_grid <- rast(nrows=1,ncols=nrow(newdata),nlyrs=ncol(newdata),
                           xmin=0,xmax=nrow(newdata),ymin=0,ymax=1,crs='')
  names(independent_grid) <- names(newdata)
  values(independent_grid) <- as.matrix(newdata)
  predict_member <- function(object) {
    value <- predict(object, newdata=independent_grid, on_0_1000=TRUE,
                     seedval=42L, mod.name='independent_QC_v1')
    as.numeric(values(value, mat=FALSE))
  }
  loaded_members <- BIOMOD_LoadModels(m, full.name=members)
  predictions <- sapply(loaded_members, function(name) predict_member(get(name)))
  expected <- qc_reduce_biomod2_4261(predictions, chosen, on_0_1000=TRUE)
  observed <- terra::extract(out, as.matrix(sites[,c('x','y')]))[,1L]
  stopifnot(length(expected)==nrow(newdata), all(is.finite(expected)), all(is.finite(observed)))
  delta <- max(abs(expected-observed))
  stopifnot(is.finite(delta), delta <= 1.5)

  check <- data.frame(x=sites$x,y=sites$y,expected=expected,observed=observed,
                      absolute_error=abs(expected-observed))
  check_tmp <- 'independent_value_check_v1.staging.csv'
  write.csv(check, check_tmp, row.names=FALSE)
  if (!file.rename(check_tmp, 'independent_value_check_v1.csv')) stop('Could not publish QC sample table')
  completed <- as.numeric(Sys.time())
  fit_source <- if (is.list(fit) && !is.null(fit$source)) fit$source else NA_character_
  qc <- list(taxon=taxon, variant=variant, source=fit_source,
             stack=projection_contract$stack, predictors=m@expl.var.names,
             PA_CV_identical=TRUE, selected_models=selected, ensemble_members=members, ensemble=chosen,
             ensemble_method='EMmedian', BIOMOD2_version='4.2.6.1',
             median_semantics='stats::median per cell then round on 0-1000 scale',
             range=range, scale='BIOMOD_0_1000', nonNA=non_na,
             max_independent_delta=delta, sample_count=nrow(newdata), independent_value_check_path='independent_value_check_v1.csv',
             qc_version='qc_only_recover_emmedian_v1', job_id=job_id,
             verified_at=as.character(Sys.time()))
  stage <- fromJSON('stage_times.json', simplifyVector=FALSE)
  stage$qc_only_recovery_v1_started <- started
  stage$qc_only_recovery_v1_completed <- completed
  if (is.null(stage$complete)) stage$complete <- completed
  atomic_write_json(stage, 'stage_times.json')
  status_tmp <- 'STATUS.staging.txt'
  writeLines('TECHNICALLY_VALIDATED_QC_ONLY_V1', status_tmp)
  if (!file.rename(status_tmp, 'STATUS.txt')) stop('Could not publish STATUS.txt')
  atomic_write_rds(qc, 'VALIDATED.rds')
  invisible(qc)
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args)) {
  if (length(args) < 2L) stop('Usage: qc_only_recover_emmedian_v1.R <root> <taxon> [variant]')
  variant <- if (length(args) >= 3L) args[[3L]] else 'ENV'
  result <- run_qc_only_recovery(args[[1L]], args[[2L]], variant)
  cat('QC_ONLY_RECOVERY_PASS', args[[2L]], variant,
      'delta=', result$max_independent_delta, 'n=', result$sample_count, '\n')
}



