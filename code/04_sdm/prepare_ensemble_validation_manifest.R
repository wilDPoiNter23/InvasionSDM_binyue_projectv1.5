# Bind accepted projections to archived fitted objects for fold-matched validation.
# Usage: Rscript prepare_ensemble_validation_manifest.R assets.csv accepted.csv manifest.csv excluded.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop("Usage: Rscript prepare_ensemble_validation_manifest.R assets.csv accepted.csv manifest.csv excluded.csv")
}
assets <- read.csv(args[1], stringsAsFactors = FALSE, check.names = FALSE)
accepted <- read.csv(args[2], stringsAsFactors = FALSE, check.names = FALSE)
required_assets <- c("species", "asset_role", "model_object_path", "selected_continuous_projection", "source_root")
required_accepted <- c("accepted_taxon", "source_generation", "product_type", "final_projection_path", "ready_for_aggregation")
if (!all(required_assets %in% names(assets)) || !all(required_accepted %in% names(accepted))) {
  stop("Input manifest lacks required columns")
}
assets <- assets[assets$asset_role == "MAIN_FULL_PRESERVATION_CANDIDATE", , drop = FALSE]
if (anyDuplicated(assets$species) || anyDuplicated(accepted$accepted_taxon)) stop("Duplicate species key")
if (!setequal(assets$species, accepted$accepted_taxon)) stop("The two manifests have different species sets")
matched <- match(accepted$accepted_taxon, assets$species)
bound <- cbind(accepted, assets[matched, c("model_object_path", "selected_continuous_projection", "source_root")])
if (any(as.character(bound$ready_for_aggregation) != "TRUE")) stop("Accepted projection not aggregation-ready")

method_from_projection <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  name <- basename(path)
  if (grepl("_EMmean", name)) return("EMmean")
  if (grepl("_EMmedian", name)) return("EMmedian")
  if (!grepl("\\.grd$", path, ignore.case = TRUE)) return(NA_character_)
  header <- readLines(path, warn = FALSE)
  layer <- header[grepl("^layername=", header)]
  if (length(layer) != 1L) return(NA_character_)
  if (grepl("_EMmean", layer)) return("EMmean")
  if (grepl("_EMmedian", layer)) return("EMmedian")
  NA_character_
}

bound$selected_method <- vapply(bound$selected_continuous_projection,
                                method_from_projection, character(1))
bound$working_directory <- ifelse(
  grepl("/models/[^/]+/[^/]+\\.models\\.out$", bound$model_object_path),
  file.path(bound$source_root, "models"), bound$source_root)
bound$input_status <- ifelse(
  !grepl("ensemble", bound$product_type, ignore.case = TRUE), "fallback_projection",
  ifelse(is.na(bound$selected_method), "no_identified_ensemble_method",
         ifelse(!file.exists(bound$model_object_path), "missing_model_object",
                ifelse(!dir.exists(bound$working_directory), "missing_working_directory", "ready"))))

manifest <- bound[bound$input_status == "ready",
                  c("accepted_taxon", "source_generation", "product_type",
                    "final_projection_path", "selected_continuous_projection",
                    "model_object_path", "working_directory", "selected_method")]
excluded <- bound[bound$input_status != "ready",
                  c("accepted_taxon", "source_generation", "product_type",
                    "selected_continuous_projection", "model_object_path", "input_status")]
if (anyDuplicated(manifest$accepted_taxon)) stop("Output manifest has duplicate species")
dir.create(dirname(args[3]), recursive = TRUE, showWarnings = FALSE)
if (file.exists(args[3]) || file.exists(args[4])) stop("Output exists")
write.csv(manifest, args[3], row.names = FALSE, na = "")
write.csv(excluded, args[4], row.names = FALSE, na = "")
cat("Ready:", nrow(manifest), "| Excluded:", nrow(excluded), "\n")
print(table(manifest$source_generation, manifest$selected_method))
print(table(excluded$input_status))
