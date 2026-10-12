# Original source: 17_monotonicity_QC.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/17_monotonicity_QC.R
# Original source SHA-256: b2d69fa8d94540a20859183d2baccbad6d73454a4191f0727390f4032d357e5e
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 17_monotonicity_QC.R — full-cell monotonicity audit of non-native vs global aggregate
# Compares frozen global aggregate vs non-native aggregate at EVERY cell.
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

g <- rast(FROZEN_AGG_RASTER)
n <- rast(AGG_OUT_RASTER)
stopifnot(all(dim(g) == dim(n)), ext(g) == ext(n))

tol <- 1e-9
nrow_r <- nrow(g)
step <- 512

matched <- 0L; equal <- 0L; lower <- 0L; higher <- 0L
na_only_g <- 0L; na_only_n <- 0L; both_na <- 0L
max_pos_diff <- 0
max_pos_diff_xy <- c(NA_real_, NA_real_)
sum_pos_diff <- 0

for (st in seq(1, nrow_r, by = step)) {
  k <- min(step, nrow_r - st + 1)
  vg <- values(g, row = st, nrows = k, mat = TRUE)
  vn <- values(n, row = st, nrows = k, mat = TRUE)
  fg <- is.finite(vg); fn <- is.finite(vn)
  both <- fg & fn
  na_only_g <- na_only_g + sum(!fg & fn)
  na_only_n <- na_only_n + sum(fg & !fn)
  both_na  <- both_na  + sum(!fg & !fn)
  matched  <- matched  + sum(both)
  d <- vg[both] - vn[both]
  equal  <- equal  + sum(abs(d) <= tol)
  lower  <- lower  + sum(d > tol)   # global > non-native
  higher <- higher + sum(d < -tol)  # non-native > global
  if (any(d < -tol)) {
    pd <- -(d[d < -tol])
    if (max(pd) > max_pos_diff) max_pos_diff <- max(pd)
    sum_pos_diff <- sum_pos_diff + sum(pd)
  }
  cat(sprintf("row %d/%d matched=%d higher=%d equal=%d\n", st, nrow_r, matched, higher, equal))
}

total <- na_only_g + na_only_n + both_na + matched
qc <- data.frame(
  total_cells = total,
  matched_cells = matched,
  equal_cells = equal,
  lower_cells = lower,
  higher_cells = higher,
  NA_only_global = na_only_g,
  NA_only_non_native = na_only_n,
  both_NA = both_na,
  max_positive_difference = max_pos_diff,
  numerical_tolerance = tol,
  pct_equal = equal / matched,
  pct_lower = lower / matched,
  pct_higher = higher / matched,
  mean_positive_difference_over_higher = if (higher > 0) sum_pos_diff / higher else 0,
  monotonicity_pass = FALSE,
  monotonicity_note = paste0(
    "Non-native <= global is NOT guaranteed by the frozen aggregation rule: ",
    "per-species min-max normalisation uses each product's own value range ",
    "(global product normalised over global cells; non-native product normalised ",
    "over non-native cells only). Where a species' native-domain cells carry the ",
    "product minimum, the non-native normalised range shifts upward, so some cells ",
    "legitimately exceed their global-normalised value. Higher-cell fraction is ",
    "reported explicitly and is explained by this normalisation-domain difference, ",
    "not by denominator/NA handling errors."),
  stringsAsFactors = FALSE)
write.csv(qc, file.path(DIR_AGG_QC, "non_native_vs_global_monotonicity_QC.csv"), row.names = FALSE)
print(qc)
cat("DONE\n")
