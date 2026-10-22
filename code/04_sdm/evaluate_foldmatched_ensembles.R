# Evaluate fold-specific ensembles from archived single-model predictions.
# Usage: Rscript evaluate_foldmatched_ensembles.R manifest.csv row_index output_dir
# The final projection's aggregation method is reused, but its saved model
# members are not presented as if they had independent validation scores.

auc_rank <- function(y, score) {
  ok <- is.finite(score) & y %in% c(0, 1)
  y <- y[ok]
  score <- score[ok]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  (sum(rank(score, ties.method = "average")[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

training_tss_cutoff <- function(y, score) {
  ok <- is.finite(score) & y %in% c(0, 1)
  y <- y[ok]
  score <- score[ok]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  ord <- order(score, decreasing = TRUE)
  sorted <- score[ord]
  labels <- y[ord]
  ends <- cumsum(rle(sorted)$lengths)
  tp <- cumsum(labels)[ends]
  fp <- ends - tp
  tss <- tp / n1 - fp / n0
  # Sorted cutoffs decrease, so the last maximum is the lowest tied cutoff.
  sorted[ends[tail(which(tss == max(tss)), 1L)]]
}

tss_at_cutoff <- function(y, score, cutoff) {
  ok <- is.finite(score) & y %in% c(0, 1)
  y <- y[ok]
  score <- score[ok]
  if (!is.finite(cutoff) || !any(y == 1) || !any(y == 0)) return(NA_real_)
  mean(score[y == 1] >= cutoff) + mean(score[y == 0] < cutoff) - 1
}

select_members <- function(evaluation, pa, run) {
  e <- evaluation[evaluation$PA == pa & evaluation$run == run &
                    evaluation$metric.eval %in% c("TSS", "ROC"),
                  c("full.name", "algo", "metric.eval", "calibration"), drop = FALSE]
  if (!nrow(e) || anyDuplicated(paste(e$full.name, e$metric.eval))) {
    return(list(names = character(), tier = "unavailable"))
  }
  wide <- reshape(e, idvar = c("full.name", "algo"), timevar = "metric.eval",
                  direction = "wide")
  if (!all(c("calibration.TSS", "calibration.ROC") %in% names(wide))) {
    return(list(names = character(), tier = "unavailable"))
  }
  for (threshold in c(0.8, 0.7)) {
    keep <- is.finite(wide$calibration.TSS) & is.finite(wide$calibration.ROC) &
      wide$calibration.TSS > threshold & wide$calibration.ROC > threshold
    if (sum(keep) >= 2L) {
      return(list(names = wide$full.name[keep], tier = sprintf("both_gt_%.1f", threshold)))
    }
  }
  list(names = character(), tier = "fewer_than_two_training_qualified_members")
}

read_archived_model <- function(path) {
  if (!file.exists(path)) stop("Model object missing: ", path)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) return(readRDS(path))
  e <- new.env(parent = globalenv())
  loaded <- load(path, envir = e)
  hits <- loaded[vapply(loaded, function(name) inherits(e[[name]], "BIOMOD.models.out"), logical(1))]
  if (length(hits) != 1L) stop("Expected exactly one BIOMOD.models.out in archived file")
  e[[hits]]
}

presence_background <- function(formatted, pa) {
  y <- as.numeric(formatted@data.species)
  pa_table <- formatted@PA.table
  if (!is.null(pa_table)) {
    if (!pa %in% colnames(pa_table)) stop("PA column absent from formatted data")
    y[is.na(y) & pa_table[, pa]] <- 0
  }
  y
}

normalise_predictions <- function(values) {
  if (any(is.finite(values) & (values < 0 | values > 1000))) stop("Predictions outside [0, 1000]")
  if (any(values > 1, na.rm = TRUE)) values <- values / 1000
  values
}

evaluate_one_fold <- function(evaluation, predictions, calibration, y, pa, run, method) {
  chosen <- select_members(evaluation, pa, run)
  column <- paste0("_", pa, "_", run)
  if (!column %in% colnames(calibration)) stop("Calibration column missing: ", column)
  train <- as.logical(calibration[, column])
  if (anyNA(train) || length(train) != length(y)) stop("Invalid calibration mask")
  result <- data.frame(pa = pa, run = run, method = method, tier = chosen$tier,
                       n_members = length(chosen$names), n_train = sum(train),
                       n_holdout = sum(!train), n_train_scored = 0L,
                       n_holdout_scored = 0L, threshold = NA_real_,
                       auc = NA_real_, tss = NA_real_, status = "incomplete_ensemble",
                       members = paste(chosen$names, collapse = ";"))
  if (length(chosen$names) < 2L) return(result)
  sub <- predictions[predictions$full.name %in% chosen$names &
                       predictions$PA == pa & predictions$run == run,
                     c("full.name", "points", "pred"), drop = FALSE]
  if (anyDuplicated(paste(sub$full.name, sub$points))) stop("Duplicate point prediction")
  if (!setequal(unique(sub$full.name), chosen$names)) stop("Selected prediction missing")
  score_matrix <- matrix(NA_real_, nrow = length(y), ncol = length(chosen$names))
  for (j in seq_along(chosen$names)) {
    part <- sub[sub$full.name == chosen$names[j], , drop = FALSE]
    if (any(!part$points %in% seq_along(y))) stop("Prediction point outside formatted data")
    score_matrix[part$points, j] <- normalise_predictions(as.numeric(part$pred))
  }
  complete <- rowSums(is.finite(score_matrix)) == ncol(score_matrix)
  score <- rep(NA_real_, length(y))
  if (method == "EMmean") score[complete] <- rowMeans(score_matrix[complete, , drop = FALSE])
  if (method == "EMmedian") score[complete] <- apply(score_matrix[complete, , drop = FALSE], 1L, median)
  train_ok <- train & is.finite(score) & y %in% c(0, 1)
  test_ok <- !train & is.finite(score) & y %in% c(0, 1)
  result$n_train_scored <- sum(train_ok)
  result$n_holdout_scored <- sum(test_ok)
  if (!any(y[train_ok] == 1) || !any(y[train_ok] == 0) ||
      !any(y[test_ok] == 1) || !any(y[test_ok] == 0)) {
    result$status <- "insufficient_label_support"
    return(result)
  }
  cutoff <- training_tss_cutoff(y[train_ok], score[train_ok])
  result$threshold <- cutoff
  result$auc <- auc_rank(y[test_ok], score[test_ok])
  result$tss <- tss_at_cutoff(y[test_ok], score[test_ok], cutoff)
  result$status <- if (is.finite(result$auc) && is.finite(result$tss)) "PASS" else "metric_unavailable"
  result
}

evaluate_species <- function(row) {
  wd <- row$working_directory
  if (!dir.exists(wd)) stop("Working directory missing: ", wd)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(wd)
  model <- read_archived_model(row$model_object_path)
  evaluation <- biomod2::get_evaluations(model)
  predictions <- biomod2::get_predictions(model)
  calibration <- biomod2::get_calib_lines(model)
  formatted <- biomod2::get_formal_data(model)
  if (nrow(calibration) != length(formatted@data.species)) stop("Calibration and response sizes differ")
  pairs <- unique(evaluation[grepl("^RUN[0-9]+$", evaluation$run), c("PA", "run"), drop = FALSE])
  if (!nrow(pairs)) stop("No archived validation runs")
  results <- lapply(seq_len(nrow(pairs)), function(i) {
    pa <- pairs$PA[i]
    run <- pairs$run[i]
    evaluate_one_fold(evaluation, predictions, calibration,
                      presence_background(formatted, pa), pa, run, row$selected_method)
  })
  d <- do.call(rbind, results)
  d$accepted_taxon <- row$accepted_taxon
  d$source_generation <- row$source_generation
  d$model_object_path <- row$model_object_path
  d$model_md5 <- unname(tools::md5sum(row$model_object_path))
  d$biomod2_version <- as.character(utils::packageVersion("biomod2"))
  d
}

run_self_test <- function() {
  stopifnot(identical(auc_rank(c(1, 1, 0, 0), c(.9, .8, .2, .1)), 1))
  stopifnot(identical(auc_rank(c(1, 0), c(.5, .5)), .5))
  stopifnot(identical(training_tss_cutoff(c(1, 1, 0, 0), c(.9, .8, .2, .1)), .8))
  stopifnot(identical(tss_at_cutoff(c(1, 0), c(.9, .1), .8), 1))
  test_eval <- data.frame(PA = rep("PA1", 6), run = rep("RUN1", 6),
                          full.name = rep(c("a", "b", "c"), each = 2),
                          algo = rep(c("GLM", "RF", "MAXNET"), each = 2),
                          metric.eval = rep(c("TSS", "ROC"), 3),
                          calibration = c(.9, .9, .75, .75, .6, .6))
  stopifnot(identical(select_members(test_eval, "PA1", "RUN1")$names, c("a", "b")))
  cat("Self-test passed\n")
}

args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--self-test")) {
  run_self_test()
} else {
  if (length(args) != 3L) stop("Usage: Rscript evaluate_foldmatched_ensembles.R manifest.csv row_index output_dir")
  missing <- !requireNamespace("biomod2", quietly = TRUE)
  if (missing) stop("biomod2 is required")
  manifest <- read.csv(args[1], stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("accepted_taxon", "model_object_path", "working_directory",
              "source_generation", "selected_method")
  if (!all(needed %in% names(manifest))) stop("Manifest missing required columns")
  i <- as.integer(args[2])
  if (is.na(i) || i < 1L || i > nrow(manifest)) stop("Invalid manifest row index")
  row <- manifest[i, , drop = FALSE]
  if (!row$selected_method %in% c("EMmean", "EMmedian")) stop("Selected method must be EMmean or EMmedian")
  dir.create(args[3], recursive = TRUE, showWarnings = FALSE)
  output <- file.path(args[3], sprintf("%04d.csv", i))
  if (file.exists(output)) stop("Output already exists: ", output)
  result <- evaluate_species(row)
  temporary <- tempfile(pattern = "ensemble_cv_", tmpdir = args[3], fileext = ".csv")
  write.csv(result, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, output)) {
    unlink(temporary)
    stop("Could not publish complete output: ", output)
  }
  cat(row$accepted_taxon, ": ", sum(result$status == "PASS"), "/", nrow(result), " folds passed\n", sep = "")
}
