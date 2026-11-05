# Compare stored GAM versions and prediction ranges without fitting models.
# Usage: Rscript compare_stored_gam_versions.R configuration.json
suppressPackageStartupMessages({library(mgcv); library(jsonlite); library(digest)})
cfg <- fromJSON(commandArgs(TRUE)[1])
out <- cfg$output_root
stopifnot(dir.exists(out))
targets <- c('GAM_version_city_responses.csv', 'GAM_version_range_comparison.csv',
  'GAM_fixed_model_range_check.csv', 'GAM_fixed_model_peak_check.csv',
  'GAM_stored_model_specifications.csv', 'GAM_version_comparison_receipt.json')
if (any(file.exists(file.path(out, targets))))
  stop('Existing stored-model comparison retained; use an empty output directory.')
sources <- list()
hash_file <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  digest(readBin(con, "raw", n = file.info(path, extra_cols = FALSE)$size),
         algo = "sha256", serialize = FALSE)
}
remember <- function(path) {stopifnot(file.exists(path)); sources[[path]] <<- hash_file(path); path}
read_table <- function(path) read.csv(remember(path), stringsAsFactors = FALSE, check.names = FALSE)
old_input <- read_table(cfg$original_input)
old83 <- read_table(file.path(cfg$previous83_root, "complete_city_input.csv"))
new83 <- read_table(file.path(cfg$current_root, "complete_city_input.csv"))
old_model <- readRDS(remember(cfg$original_model))
new_model <- readRDS(remember(file.path(cfg$current_root, "ARCHIVED_GAM_RISK_G0.rds")))
old83_summary <- read_table(file.path(cfg$previous83_root, "model_summary.csv"))
old83_profiles <- read_table(file.path(cfg$previous83_root, "endpoint_profiles.csv"))
new83_profiles <- read_table(file.path(cfg$current_root, "endpoint_profiles.csv"))
stopifnot(nrow(old_input) == 108, nrow(old83) == 83, nrow(new83) == 83,
          identical(old83$city, new83$city), identical(names(old83), names(new83)))
unchanged <- setdiff(names(old83), "risk")
stopifnot(identical(old83[, unchanged], new83[, unchanged]))
formula_text <- function(m) gsub("[[:space:]]+", " ", paste(deparse(formula(m)), collapse = " "))
stopifnot(identical(formula_text(old_model), formula_text(new_model)),
          identical(old_model$method, new_model$method),
          identical(old_model$family$family, new_model$family$family),
          identical(old_model$family$link, new_model$family$link))
stopifnot(all(old_model$smooth[[1]]$term == c("log_recl", "area_km2")),
          all(new_model$smooth[[1]]$term == c("log_recl", "area_km2")),
          all(vapply(old_model$smooth[[1]]$margin, function(x) x$bs.dim, numeric(1)) == 5),
          all(vapply(new_model$smooth[[1]]$margin, function(x) x$bs.dim, numeric(1)) == 5))
old_input$log_recl <- log1p(old_input$reclamation_area)
old_input$log_area <- log1p(old_input$area_km2)
old_input$true_fraction <- old_input$reclamation_area / (100 * old_input$area_km2)
required <- c("risk", "log_recl", "area_km2", "log_area", "true_fraction",
              "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1")
old84 <- old_input[complete.cases(old_input[, required]), ]
stopifnot(nrow(old84) == 84, nrow(old_model$model) == 84, nrow(new_model$model) == 83,
          max(abs(old_model$model$risk - old84$risk)) < 1e-12,
          max(abs(new_model$model$risk - new83$risk)) < 1e-12)
dropped <- setdiff(old84$city, old83$city)
stopifnot(length(dropped) == 1, dropped == "zhanzhou")
for (field in setdiff(names(new_model$model), "risk")) {
  stopifnot(max(abs(new_model$model[[field]] - new83[[field]])) < 1e-12)
}
city <- data.frame(city = old83$city, city_area_km2 = old83$area_km2,
                   original_response = old83$risk, updated_response = new83$risk,
                   change = new83$risk - old83$risk)
write.csv(city, file.path(out, "GAM_version_city_responses.csv"), row.names = FALSE)
old <- old83_profiles[old83_profiles$response == "ARCHIVED_GAM_RISK" & old83_profiles$model == "G0", ]
new <- new83_profiles[new83_profiles$response == "ARCHIVED_GAM_RISK" & new83_profiles$model == "G0", ]
stopifnot(identical(old[, c("mode", "area_quantile", "city_area_km2", "reclamation_low_ha", "reclamation_high_ha")],
                    new[, c("mode", "area_quantile", "city_area_km2", "reclamation_low_ha", "reclamation_high_ha")]))
p90 <- old[old$area_quantile == .9, ]
matrix <- rbind(
  transform(old[old$area_quantile == .9 & old$mode %in% c("WHOLE_INPUT_RANGE", "COMMON_ABSOLUTE_IQR"), ],
            response_version = "Original response, 83 cities"),
  transform(new[new$area_quantile == .9 & new$mode %in% c("WHOLE_INPUT_RANGE", "COMMON_ABSOLUTE_IQR"), ],
            response_version = "Updated FULL response, 83 cities"))
matrix$lower_reclamation_km2 <- matrix$reclamation_low_ha / 100
matrix$upper_reclamation_km2 <- matrix$reclamation_high_ha / 100
matrix$difference_points <- matrix$difference * 100
write.csv(matrix, file.path(out, "GAM_version_range_comparison.csv"), row.names = FALSE)
reference <- old_input[!old_input$city %in% dropped, ]
baseline <- data.frame(log_recl = median(reference$log_recl),
                       area_km2 = p90$city_area_km2[1],
                       anthro_pc1 = median(reference$anthro_pc1, na.rm = TRUE),
                       climate_pc1 = median(reference$climate_pc1, na.rm = TRUE),
                       soil_pc1 = median(reference$soil_pc1, na.rm = TRUE),
                       marine_pc1 = median(reference$marine_pc1, na.rm = TRUE))
models <- list(ORIGINAL84 = old_model, UPDATED83 = new_model)
range_rows <- list(); curves <- list()
for (version in names(models)) {
  m <- models[[version]]
  for (mode in c("WHOLE_INPUT_RANGE", "COMMON_ABSOLUTE_IQR")) {
    q <- p90[p90$mode == mode, ]
    amounts <- c(q$reclamation_low_ha, q$reclamation_high_ha)
    nd <- baseline[rep(1, 2), ]; nd$log_recl <- log1p(amounts)
    p <- predict(m, nd, type = "response")
    range_rows[[paste(version, mode)]] <- data.frame(model_version = version,
      city_area_km2 = baseline$area_km2, mode = mode,
      lower_reclamation_km2 = amounts[1] / 100, upper_reclamation_km2 = amounts[2] / 100,
      lower_suitability = p[1], upper_suitability = p[2], difference_points = 100 * diff(p))
  }
  amount <- expm1(seq(log1p(min(reference$reclamation_area)),
                       log1p(max(reference$reclamation_area)), length.out = 4001))
  nd <- baseline[rep(1, length(amount)), ]; nd$log_recl <- log1p(amount)
  p <- predict(m, nd, type = "response")
  k <- which.max(p)
  curves[[version]] <- data.frame(model_version = version,
    city_area_km2 = baseline$area_km2, peak_reclamation_km2 = amount[k] / 100,
    peak_suitability = p[k], peak_is_boundary = k %in% c(1, length(p)))
}
range_rows <- do.call(rbind, range_rows)
write.csv(range_rows, file.path(out, "GAM_fixed_model_range_check.csv"), row.names = FALSE)
write.csv(do.call(rbind, curves), file.path(out, "GAM_fixed_model_peak_check.csv"), row.names = FALSE)
summary_row <- function(m, version) {
  s <- summary(m)
  data.frame(version = version, n = nrow(m$model), adjusted_r2 = s$r.sq,
             tensor_edf = s$s.table[1, 1], tensor_p = s$s.table[1, 4], method = m$method,
             family = m$family$family, link = m$family$link, formula = formula_text(m))
}
summary <- rbind(summary_row(old_model, "Original response, 84 cities"),
                 summary_row(new_model, "Updated FULL response, 83 cities"))
write.csv(summary, file.path(out, "GAM_stored_model_specifications.csv"), row.names = FALSE)
for (path in names(sources)) stopifnot(identical(hash_file(path), sources[[path]]))
receipt <- list(status = "STORED_MODEL_COMPARISON_COMPLETED", no_model_refits = TRUE,
  original_model_cities = 84, previous_and_current_cities = 83, dropped_city_key = dropped,
  identical_83_city_lists = TRUE, identical_all_nonresponse_columns = TRUE,
  changed_input_columns = "risk", identical_formulas = TRUE, identical_smoothing_method = TRUE,
  identical_tensor_basis_dimensions = TRUE, original_reclamation_unit = "hectares before log1p",
  current_reclamation_unit = "hectares before log1p", response_mean_old = mean(old83$risk),
  response_mean_new = mean(new83$risk), response_change_range = range(city$change),
  response_pearson = cor(old83$risk, new83$risk), response_spearman = cor(old83$risk, new83$risk, method = "spearman"),
  fixed_model_range_checks = range_rows, peaks = do.call(rbind, curves),
  input_sha256 = sources, no_graphics_modified = TRUE)
write_json(receipt, file.path(out, "GAM_version_comparison_receipt.json"), auto_unbox = TRUE, pretty = TRUE)
print(summary[, setdiff(names(summary), "formula")])
print(matrix[, c("response_version", "mode", "city_area_km2", "difference_points")])
print(range_rows)
print(do.call(rbind, curves))
cat("No models were fitted; model and input hashes unchanged.\n")
