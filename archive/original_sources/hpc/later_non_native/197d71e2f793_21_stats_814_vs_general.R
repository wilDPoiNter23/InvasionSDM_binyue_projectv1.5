# Original source: 21_stats_814_vs_general.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/21_stats_814_vs_general.R
# Original source SHA-256: 197d71e2f7931d15f7055cadcc283cd483cfdfea7bedc642e41fb5e3e387d746
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 21_stats_814_vs_general.R — TASK B: statistics only (reads finished rasters)
# Outputs: 06_tables/supplementary_814/
#   comparison_814_vs_813.csv, comparison_813_non_native_vs_global.csv,
#   comparison_814_vs_global.csv, aggregate_distribution_summary.csv
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

SUPP <- file.path(DIR_AGG_RASTERS, "supplementary_814")
TABD <- file.path(DIR_TABLES, "supplementary_814")
dir.create(TABD, recursive = TRUE, showWarnings = FALSE)

nn814 <- rast(file.path(SUPP, "multispecies_non_native_invasion_potential_814.tif"))
nn813 <- rast(AGG_OUT_RASTER)
glob  <- rast(FROZEN_AGG_RASTER)

dist_summary <- function(r, label) {
  v <- values(r, mat = FALSE); v <- v[is.finite(v)]
  qs <- quantile(v, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1))
  data.frame(raster = label, n = length(v), mean = mean(v), sd = sd(v),
             min = qs[1], Q01 = qs[2], Q05 = qs[3], Q25 = qs[4], median = qs[5],
             Q75 = qs[6], Q95 = qs[7], Q99 = qs[8], max = qs[9])
}

pair_stats <- function(a, b, label_a, label_b, out_name) {
  tol <- 1e-9
  step <- 512; nr <- nrow(a)
  matched <- 0L; equal <- 0L; a_lo <- 0L; a_hi <- 0L
  max_abs <- 0
  for (st in seq(1, nr, by = step)) {
    k <- min(step, nr - st + 1)
    va <- values(a, row = st, nrows = k, mat = TRUE)
    vb <- values(b, row = st, nrows = k, mat = TRUE)
    fa <- is.finite(va); fb <- is.finite(vb)
    both <- fa & fb
    matched <- matched + sum(both)
    d <- va[both] - vb[both]
    equal <- equal + sum(abs(d) <= tol)
    a_lo <- a_lo + sum(d < -tol)
    a_hi <- a_hi + sum(d > tol)
    m <- max(abs(d))
    if (is.finite(m) && m > max_abs) max_abs <- m
  }
  set.seed(42)
  idx <- sample(seq_len(ncell(a)), 2e5)
  va <- values(a, mat = FALSE)[idx]; vb <- values(b, mat = FALSE)[idx]
  keep <- is.finite(va) & is.finite(vb)
  va <- va[keep]; vb <- vb[keep]
  d <- va - vb
  q <- quantile(d, probs = c(0.05, 0.95))
  out <- data.frame(
    pair = paste0(label_a, "_vs_", label_b),
    matched_cells = matched,
    equal_cells = equal, equal_pct = equal / matched,
    a_lower_cells = a_lo, a_lower_pct = a_lo / matched,
    a_higher_cells = a_hi, a_higher_pct = a_hi / matched,
    max_abs_difference = max_abs,
    sampled_paired_cells = length(va),
    Pearson_r = cor(va, vb),
    MAE = mean(abs(d)), RMSE = sqrt(mean(d^2)),
    mean_difference = mean(d), median_difference = median(d),
    Q05_difference = q[1], Q95_difference = q[2])
  write.csv(out, file.path(TABD, out_name), row.names = FALSE)
  cat("wrote", out_name, "\n")
  out
}

p1 <- pair_stats(nn814, nn813, "NN814", "NN813", "comparison_814_vs_813.csv")
p2 <- pair_stats(nn813, glob,  "NN813", "GLOBAL813", "comparison_813_non_native_vs_global.csv")
p3 <- pair_stats(nn814, glob,  "NN814", "GLOBAL813", "comparison_814_vs_global.csv")

d <- rbind(
  dist_summary(nn814, "NN814"),
  dist_summary(nn813, "NN813"),
  dist_summary(glob,  "GLOBAL813"))
write.csv(d, file.path(TABD, "aggregate_distribution_summary.csv"), row.names = FALSE)

print(rbind(p1, p2, p3))
print(d)
cat("DONE\n")
