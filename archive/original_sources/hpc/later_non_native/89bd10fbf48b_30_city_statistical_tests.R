# Original source: 30_city_statistical_tests.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/30_city_statistical_tests.R
# Original source SHA-256: 89bd10fbf48be1f1b9d3867913991cde081a6fdd22b4dffc274411c0891aeca4
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 30_city_statistical_tests.R — statistical tests: can the GIS city values be
# considered the same data as each product version?
# Series (per-city means): GIS, 813G, 813NN, 814G, 814NN
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

OUTDIR <- file.path(DIR_TABLES, "city_analysis_108")
v <- vect("<HPC_INPUT_ROOT>/climate/cities/108finalCities_recheck.shp")
gis <- read.csv("<NON_NATIVE_ROOT>/01_data_package/city_risk_summary_fromGIS.csv",
                stringsAsFactors = FALSE)
gis$city_l <- tolower(trimws(gis$city))

city_mean <- function(r) {
  e <- extract(r, v, ID = TRUE, na.rm = TRUE)
  m <- tapply(e[, 2], e$ID, mean)
  data.frame(city_l = tolower(trimws(v$shapeName)), val = as.numeric(m[as.character(seq_len(nrow(v)))]))
}

g813  <- city_mean(rast(FROZEN_AGG_RASTER))
n813  <- city_mean(rast(AGG_OUT_RASTER))
n814  <- city_mean(rast(file.path(DIR_AGG_RASTERS, "supplementary_814", "multispecies_non_native_invasion_potential_814.tif")))
g814  <- city_mean(rast(file.path(DIR_AGG_RASTERS, "supplementary_814", "multispecies_global_invasion_potential_814.tif")))

# one row per unique GIS city (duplicates share identical values)
gis_u <- gis[!duplicated(gis$city_l), ]
dat <- data.frame(city = gis_u$city, city_l = gis_u$city_l,
                  GIS = as.numeric(gis_u$mean_risk),
                  G813 = g813$val[match(gis_u$city_l, g813$city_l)],
                  N813 = n813$val[match(gis_u$city_l, n813$city_l)],
                  G814 = g814$val[match(gis_u$city_l, g814$city_l)],
                  N814 = n814$val[match(gis_u$city_l, n814$city_l)])
dat <- dat[complete.cases(dat), ]
cat("paired cities for tests:", nrow(dat), "\n")

ccc <- function(x, y) {
  r <- cor(x, y); m1 <- mean(x); m2 <- mean(y); s1 <- sd(x); s2 <- sd(y)
  2 * r * s1 * s2 / (s1^2 + s2^2 + (m1 - m2)^2)
}

series <- c("G813", "N813", "G814", "N814")
res <- lapply(series, function(s) {
  x <- dat$GIS; y <- dat[[s]]; d <- x - y
  wt <- suppressWarnings(wilcox.test(x, y, paired = TRUE, exact = FALSE))
  tt <- t.test(x, y, paired = TRUE)
  ci90 <- tt$conf.int  # 95% CI by default; TOST uses 90% -> recompute
  tt90 <- t.test(x, y, paired = TRUE, conf.level = 0.90)
  ci <- tt90$conf.int
  tost <- (ci[1] > -0.05) && (ci[2] < 0.05)
  data.frame(comparison = paste0("GIS_vs_", s),
    n = length(d),
    mean_GIS = mean(x), mean_product = mean(y),
    mean_diff = mean(d), sd_diff = sd(d),
    Pearson_r = cor(x, y), CCC = ccc(x, y),
    paired_t_p = tt$p.value,
    wilcoxon_p = wt$p.value,
    wilcoxon_V = as.numeric(wt$statistic),
    CI90_lower = ci[1], CI90_upper = ci[2],
    TOST_equiv_bounds_0_05 = tost)
})
res <- do.call(rbind, res)
res$holm_adjusted_p <- p.adjust(res$wilcoxon_p, method = "holm")

# Friedman test across all 5 series
M <- as.matrix(dat[, c("GIS", series)])
fried <- friedman.test(M)
cat("Friedman chi2:", fried$statistic, " p:", fried$p.value, "\n")

write.csv(dat, file.path(OUTDIR, "city_series_five_versions.csv"), row.names = FALSE)
write.csv(res, file.path(OUTDIR, "city_statistical_tests_products.csv"), row.names = FALSE)
print(res)
cat("DONE\n")
