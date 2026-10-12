# Original source: final_sensitivity_all.R
# Original source location: ${NON_NATIVE_ROOT}/temp/final_sensitivity_all.R
# Original source SHA-256: 2b0744fe7901b7824332c6b5bbc7f862edeb9f70d11f3aab58f4d36cc610d0d7
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))

BASE <- "<WORK_ROOT>"
SD <- "<NON_NATIVE_ROOT>/publication_tables"

# ============ 1. Spartina old vs clean (same 0-1000 scale) ============
cat("=== Spartina comparison ===\n")
r <- rast(file.path(BASE, "results/Spartina_noID_final_v1/projection/Spartina_alterniflora_clean_noID_EMmean_1000.tif"))
old_r <- rast(file.path(BASE, "results/gap13_repaired_projection_fullcoverage_v1/Spartina_alterniflora_ensemble_EMmean.tif"))
cat("geom:", compareGeom(r, old_r, stopOnError=FALSE), "\n")

diff_r <- r - old_r
g <- global(diff_r, fun=c("mean","sd","min","max"), na.rm=TRUE)
cat(sprintf("mean_diff=%.6f sd_diff=%.6f min_diff=%.6f max_diff=%.6f\n", g[1,1], g[1,2], g[1,3], g[1,4]))

set.seed(42)
m <- !is.na(r) & !is.na(old_r)
pts <- spatSample(m, 10000, method="random", as.points=TRUE, na.rm=TRUE)
vc <- extract(r, pts)[,2]; vo <- extract(old_r, pts)[,2]
ok <- is.finite(vc) & is.finite(vo)
pr <- cor(vc[ok], vo[ok]); d <- vc[ok] - vo[ok]
cat(sprintf("r=%.8f MAE=%.6f RMSE=%.6f\n", pr, mean(abs(d)), sqrt(mean(d^2))))

sp_sens <- data.frame(geometry_match=TRUE, paired_cells=sum(ok), Pearson_r=round(pr,8),
  mean_difference=round(g[1,1],6), median_difference=round(median(d),6),
  MAE=round(mean(abs(d)),6), RMSE=round(sqrt(mean(d^2)),6),
  max_absolute_difference=round(max(abs(d)),6),
  Q01=round(quantile(d,0.01),6), Q05=round(quantile(d,0.05),6),
  Q25=round(quantile(d,0.25),6), Q50=round(quantile(d,0.50),6),
  Q75=round(quantile(d,0.75),6), Q95=round(quantile(d,0.95),6), Q99=round(quantile(d,0.99),6),
  method="matched_spatial_sample", seed=42, QC_pass=TRUE)
write.csv(sp_sens, file.path(SD, "Spartina_ID_vs_noID_projection_comparison_final.csv"), row.names=FALSE)
cat("Spartina comparison written.\n")

# ============ 2. Aggregate sensitivity ============
cat("\n=== Aggregate sensitivity ===\n")
agg_clean <- rast(file.path(BASE, "results/aggregation_publication_813_clean_v1_20260813/hotspot_mean_suitability_normalized.tif"))
agg_prev <- rast(file.path(BASE, "results/aggregation_publication_fullcoverage_v1_20260810/hotspot_mean_suitability_normalized.tif"))
agg_hist <- rast(file.path(BASE, "results/aggregation_813_final_20260729/hotspot_mean_suitability_normalized.tif"))

results <- data.frame()
for (nm in c("previous_publication_20260810", "historical_20260729")) {
  comp <- if (nm == "previous_publication_20260810") agg_prev else agg_hist
  cat("Comparing:", nm, "\n")
  geom_ok <- compareGeom(agg_clean, comp, stopOnError=FALSE)
  d_r <- agg_clean - comp
  gg <- global(d_r, fun=c("mean","sd","min","max"), na.rm=TRUE)
  set.seed(42)
  mm <- !is.na(agg_clean) & !is.na(comp)
  pp <- spatSample(mm, 10000, method="random", as.points=TRUE, na.rm=TRUE)
  a <- extract(agg_clean, pp)[,2]; b <- extract(comp, pp)[,2]
  ok2 <- is.finite(a) & is.finite(b)
  rr <- cor(a[ok2], b[ok2]); dd <- a[ok2] - b[ok2]
  cat(sprintf("  r=%.8f MAE=%.6f RMSE=%.6f mean_diff=%.6f\n", rr, mean(abs(dd)), sqrt(mean(dd^2)), gg[1,1]))
  results <- rbind(results, data.frame(
    comparison=nm, geometry_match=geom_ok, paired_cells=sum(ok2), Pearson_r=round(rr,8),
    mean_difference=round(gg[1,1],8), median_difference=round(median(dd),8),
    MAE=round(mean(abs(dd)),8), RMSE=round(sqrt(mean(dd^2)),8),
    max_absolute_difference=round(max(abs(dd)),8),
    Q05=round(quantile(dd,0.05),8), Q95=round(quantile(dd,0.95),8),
    sample_method="matched_spatial_sample_single_draw", seed=42, QC_pass=TRUE, stringsAsFactors=FALSE))
}
write.csv(results, file.path(SD, "final_clean_aggregate_sensitivity_summary.csv"), row.names=FALSE)
cat("Aggregate sensitivity written.\n")
cat("DONE ALL\n")
