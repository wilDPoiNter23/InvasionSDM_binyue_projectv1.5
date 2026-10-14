#!/usr/bin/env Rscript
# Inspect temporal attributes and source documentation before age modelling.
suppressPackageStartupMessages({library(jsonlite); library(sf); library(digest)})
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Supply a private JSON configuration")
cfg <- fromJSON(args[1], simplifyVector = FALSE)
out <- cfg$output_root
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(out, "input_inventory.csv"))) stop("Existing inspection preserved")
inventory <- list(); frequencies <- list(); summaries <- list()
for (id in names(cfg$layers)) {
  path <- cfg$layers[[id]]
  x <- st_read(path, quiet = TRUE)
  d <- st_drop_geometry(x)
  bb <- st_bbox(st_transform(st_as_sfc(st_bbox(x)), 4326))
  inventory[[id]] <- data.frame(layer = id, features = nrow(x),
    fields = paste(names(d), collapse = ";"), epsg = st_crs(x)$epsg,
    xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4],
    source = path, sha256 = digest(file = path, algo = "sha256"))
  write.csv(head(d, 20), file.path(out, paste0(id, "attribute_examples.csv")), row.names = FALSE)
  for (field in names(d)) {
    u <- unique(d[[field]])
    summaries[[paste(id, field)]] <- data.frame(layer = id, field = field,
      class = class(d[[field]])[1], unique_values = length(u), missing = sum(is.na(d[[field]])),
      examples = paste(head(u, 12), collapse = ";"))
    if (length(u) <= 100L) {
      tab <- table(d[[field]], useNA = "ifany")
      frequencies[[paste(id, field)]] <- data.frame(layer = id, field = field,
        value = names(tab), count = as.integer(tab))
    }
  }
}
write.csv(do.call(rbind, inventory), file.path(out, "input_inventory.csv"), row.names = FALSE)
write.csv(do.call(rbind, summaries), file.path(out, "attribute_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, frequencies), file.path(out, "attribute_frequencies.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out, "sessionInfo.txt"))
writeLines(capture.output(str(cfg)), file.path(out, "configuration_receipt.txt"))
cat("Temporal input attributes inspected; no ages inferred.\n")
