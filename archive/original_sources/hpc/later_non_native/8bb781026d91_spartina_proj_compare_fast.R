# Original source: spartina_proj_compare_fast.R
# Original source location: ${NON_NATIVE_ROOT}/temp/spartina_proj_compare_fast.R
# Original source SHA-256: 8bb781026d91ba5059a951319001e8f93c1d3e6c52896a4a99b8b38c87f0060a
# Recovery: live read-only SSH, 2026-10-12
# Classification: SUPPORTING (HISTORICAL_SUPPORTING_AUDIT_LINEAGE_HELPER)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
.libPaths(c("<HPC_INPUT_ROOT>/script/renv/library/linux-ubuntu-focal/R-4.4/aarch64-unknown-linux-gnu", .libPaths()))
suppressPackageStartupMessages(library(terra))

BASE <- "<WORK_ROOT>"
r <- rast(file.path(BASE, "results/Spartina_noID_final_v1/projection/Spartina_alterniflora_clean_noID_EMmean.tif"))
old_r <- rast(file.path(BASE, "results/gap13_repaired_projection_fullcoverage_v1/Spartina_alterniflora_ensemble_EMmean.tif"))

cat("Geometry match:", compareGeom(r, old_r, stopOnError=FALSE), "\n")

set.seed(42)
m <- !is.na(r) & !is.na(old_r)
pts <- spatSample(m, 20000, method="random", as.points=TRUE, na.rm=TRUE)
vc <- extract(r, pts)[,2]
vo <- extract(old_r, pts)[,2]
ok <- is.finite(vc) & is.finite(vo)
pr <- cor(vc[ok], vo[ok])
d <- vc[ok] - vo[ok]

cat(sprintf("Paired cells: %d\n", sum(ok)))
cat(sprintf("Pearson r: %.8f\n", pr))
cat(sprintf("Mean diff: %.8f\n", mean(d)))
cat(sprintf("Median diff: %.8f\n", median(d)))
cat(sprintf("MAE: %.8f\n", mean(abs(d))))
cat(sprintf("RMSE: %.8f\n", sqrt(mean(d^2))))
cat(sprintf("Max abs diff: %.8f\n", max(abs(d))))
cat(sprintf("Q05: %.8f Q95: %.8f\n", quantile(d,0.05), quantile(d,0.95)))

sens <- data.frame(
  geometry_match=TRUE, paired_cells=sum(ok), Pearson_r=round(pr,8),
  mean_difference=round(mean(d),8), median_difference=round(median(d),8),
  MAE=round(mean(abs(d)),8), RMSE=round(sqrt(mean(d^2)),8),
  max_absolute_difference=round(max(abs(d)),8),
  Q01=round(quantile(d,0.01),8), Q05=round(quantile(d,0.05),8),
  Q25=round(quantile(d,0.25),8), Q50=round(quantile(d,0.50),8),
  Q75=round(quantile(d,0.75),8), Q95=round(quantile(d,0.95),8),
  Q99=round(quantile(d,0.99),8),
  method="matched_spatial_sample_single_draw", seed=42, QC_pass=TRUE
)
write.csv(sens, "<NON_NATIVE_ROOT>/publication_tables/Spartina_ID_vs_noID_projection_comparison_final.csv", row.names=FALSE)
cat("Written. DONE\n")
