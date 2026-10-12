# Original source: 31_audit_projection_scales.R
# Original source location: ${NON_NATIVE_ROOT}/02_derived_non_native_potential/07_scripts/31_audit_projection_scales.R
# Original source SHA-256: e1708f7ab10485121be98506e731195704e5c75f37177e85a141f0b0137776fc
# Recovery: live read-only SSH, 2026-10-12
# Classification: HISTORICAL (HISTORICAL_SUPPORTING_LATER_NON_NATIVE)
# Publication modifications: private root/account paths replaced by public placeholders; UTF-8 decoding
# Scientific modification: NONE
# Inspection snapshot; not a canonical submitted-analysis entrypoint.
# 31_audit_projection_scales.R — audit native value scales of all 813 frozen
# global projections (ARE they 0-1 or 0-1000 or something else?)
ca <- commandArgs(trailingOnly = FALSE)
source(file.path(dirname(sub("^--file=", "", ca[grep("^--file=", ca)])), "00_config.R"))
suppressMessages(library(terra))

m <- read.csv(FROZEN_MANIFEST_V3, stringsAsFactors = FALSE)
paths <- m$final_projection_path
paths <- ifelse(grepl("^/", paths) & file.exists(paths), paths,
                file.path("<WORK_ROOT>", paths))

out <- data.frame()
for (i in seq_along(paths)) {
  mm <- tryCatch(minmax(rast(paths[i])), error = function(e) matrix(c(NA, NA), 1))
  out <- rbind(out, data.frame(accepted_taxon = m$accepted_taxon[i],
    min = mm[1], max = mm[2], product_type = m$product_type[i],
    source_generation = m$source_generation[i], stringsAsFactors = FALSE))
  if (i %% 100 == 0) cat(sprintf("[%d/813]\n", i))
}
classify <- function(mn, mx) {
  ifelse(is.na(mx), "NA",
    ifelse(mx <= 1.0001, "0-1 scale",
      ifelse(mx <= 1000.0001, "0-1000 scale (max<=1000)", "OTHER")))
}
out$scale_class <- classify(out$min, out$max)
write.csv(out, file.path(DIR_TABLES, "projection_scale_audit_813.csv"), row.names = FALSE)
cat("\nscale class counts:\n"); print(table(out$scale_class))
cat("\nmin>0 among 0-1000 products:\n")
z <- out[out$scale_class != "0-1 scale" & out$min > 0.001, ]
print(z[order(-z$min), ][1:min(10, nrow(z)), c("accepted_taxon", "min", "max")])
cat("\nsummary of mins by class:\n"); print(tapply(out$min, out$scale_class, summary))
cat("DONE\n")
