# Original source: 060506_gam_candidate_model_test.r
# Original source location: <LOCAL_CODE_ROOT>/060506_gam_candidate_model_test.r
# Original source SHA-256: 8f84be308d908434b15744baf167eafb7a2eb6f9d6507d460abe4a32c06813bf
# Public-safe original snapshot: archive/original_sources/local/060506_gam_candidate_model_test.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
# Snapshot for inspection; path placeholders are not executable locations.
.libPaths("<HISTORICAL_PATH_010>")

suppressPackageStartupMessages({
  library(mgcv)
  library(dplyr)
})

workdir <- Sys.getenv("GAM_WORKDIR", unset = "<LOCAL_ANALYSIS_ROOT>/figure/gam_draft")
input_csv <- Sys.getenv("GAM_INPUT_CSV", unset = file.path(workdir, "gamtest_withpca.csv"))
output_dir <- Sys.getenv("GAM_OUTPUT_DIR", unset = "<RECOVERED_CODE_ROOT>/gam_candidate_model_outputs")
setwd(workdir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Candidate GAM model test ===\n")

city_df <- read.csv(input_csv)
city_df$log_city_area <- log1p(city_df$area_km2)
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$recl_frac <- city_df$reclamation_area / city_df$area_km2
city_df$log_recl_frac <- log1p(city_df$recl_frac)
city_df$Regions <- as.factor(city_df$Regions)

recl_pc <- prcomp(
  scale(city_df[, c("log_recl", "log_recl_frac")]),
  center = FALSE,
  scale. = FALSE
)
city_df$recl_combo_pc1 <- recl_pc$x[, 1]
if (cor(city_df$recl_combo_pc1, city_df$log_recl, use = "pairwise.complete.obs") < 0) {
  city_df$recl_combo_pc1 <- -city_df$recl_combo_pc1
}

control_sets <- list(
  full = c(
    "s(anthro_pc1, k = 5)",
    "s(climate_pc1, k = 5)",
    "s(soil_pc1, k = 5)",
    "s(marine_pc1, k = 5)",
    "s(geom_pc1, k = 5)",
    "s(Regions, bs = 're')"
  ),
  no_region = c(
    "s(anthro_pc1, k = 5)",
    "s(climate_pc1, k = 5)",
    "s(soil_pc1, k = 5)",
    "s(marine_pc1, k = 5)",
    "s(geom_pc1, k = 5)"
  ),
  no_marine = c(
    "s(anthro_pc1, k = 5)",
    "s(climate_pc1, k = 5)",
    "s(soil_pc1, k = 5)",
    "s(geom_pc1, k = 5)",
    "s(Regions, bs = 're')"
  ),
  reduced = c(
    "s(anthro_pc1, k = 5)",
    "s(climate_pc1, k = 5)",
    "s(soil_pc1, k = 5)",
    "s(geom_pc1, k = 5)"
  )
)

model_specs <- list(
  dual = c(
    "s(log_recl, k = 5)",
    "s(log_recl_frac, k = 5)"
  ),
  single_logrecl = c(
    "s(log_recl, k = 5)",
    "s(log_city_area, k = 5)"
  ),
  combo = c(
    "s(recl_combo_pc1, k = 5)",
    "s(log_city_area, k = 5)"
  ),
  ti_interaction = c(
    "s(log_recl, k = 5)",
    "s(log_city_area, k = 5)",
    "ti(log_recl, log_city_area, k = c(5, 5))"
  ),
  te_interaction = c(
    "te(log_recl, log_city_area, k = c(5, 5))"
  )
)

fit_model <- function(base_terms, control_terms) {
  rhs <- paste(c(base_terms, control_terms), collapse = " + ")
  gam(as.formula(paste("risk ~", rhs)), data = city_df, method = "REML")
}

extract_p <- function(model, pattern) {
  st <- summary(model)$s.table
  idx <- grep(pattern, rownames(st))
  if (!length(idx)) {
    return(NA_real_)
  }
  unname(st[idx[1], "p-value"])
}

extract_concurvity <- function(model) {
  cc <- concurvity(model, full = TRUE)
  if (is.list(cc)) {
    cc <- cc$estimate
  }
  cc
}

summarize_model <- function(model_name, model_obj) {
  cc <- extract_concurvity(model_obj)
  observed_row <- if ("observed" %in% rownames(cc)) cc["observed", ] else rep(NA_real_, ncol(cc))
  estimate_row <- if ("estimate" %in% rownames(cc)) cc["estimate", ] else rep(NA_real_, ncol(cc))

  tibble(
    model = model_name,
    AIC = AIC(model_obj),
    adj_r2 = summary(model_obj)$r.sq,
    dev_exp = summary(model_obj)$dev.expl,
    p_log_recl = extract_p(model_obj, "s\\(log_recl\\)"),
    p_log_recl_frac = extract_p(model_obj, "s\\(log_recl_frac\\)"),
    p_log_city_area = extract_p(model_obj, "s\\(log_city_area\\)"),
    p_recl_combo_pc1 = extract_p(model_obj, "s\\(recl_combo_pc1\\)"),
    p_ti = extract_p(model_obj, "ti\\("),
    p_te = extract_p(model_obj, "te\\("),
    cc_obs_max = suppressWarnings(max(observed_row, na.rm = TRUE)),
    cc_est_max = suppressWarnings(max(estimate_row, na.rm = TRUE)),
    cc_obs_log_recl = if ("s(log_recl)" %in% names(observed_row)) observed_row[["s(log_recl)"]] else NA_real_,
    cc_obs_log_recl_frac = if ("s(log_recl_frac)" %in% names(observed_row)) observed_row[["s(log_recl_frac)"]] else NA_real_,
    cc_obs_log_city_area = if ("s(log_city_area)" %in% names(observed_row)) observed_row[["s(log_city_area)"]] else NA_real_,
    cc_obs_recl_combo = if ("s(recl_combo_pc1)" %in% names(observed_row)) observed_row[["s(recl_combo_pc1)"]] else NA_real_,
    cc_obs_ti = if ("ti(log_recl,log_city_area)" %in% names(observed_row)) observed_row[["ti(log_recl,log_city_area)"]] else NA_real_,
    cc_obs_te = if ("te(log_recl,log_city_area)" %in% names(observed_row)) observed_row[["te(log_recl,log_city_area)"]] else NA_real_
  )
}

models <- list()
for (control_name in names(control_sets)) {
  for (spec_name in names(model_specs)) {
    model_name <- paste(spec_name, control_name, sep = "__")
    cat("Fitting:", model_name, "\n")
    models[[model_name]] <- fit_model(model_specs[[spec_name]], control_sets[[control_name]])
  }
}

summary_table <- bind_rows(lapply(names(models), function(model_name) {
  summarize_model(model_name, models[[model_name]])
})) |>
  arrange(AIC)

write.csv(summary_table, file.path(output_dir, "candidate_model_summary.csv"), row.names = FALSE)

for (model_name in names(models)) {
  model_obj <- models[[model_name]]
  cc <- extract_concurvity(model_obj)
  write.csv(
    cc,
    file.path(output_dir, paste0(model_name, "_concurvity.csv"))
  )
  capture.output(
    list(
      formula = formula(model_obj),
      summary = summary(model_obj),
      concurvity = cc
    ),
    file = file.path(output_dir, paste0(model_name, "_summary.txt"))
  )
}

cat("\n=== Candidate model ranking ===\n")
print(summary_table, digits = 4)

cat("\nTop models by AIC:\n")
print(summary_table[, c(
  "model", "AIC", "adj_r2", "dev_exp",
  "p_log_recl", "p_log_recl_frac", "p_recl_combo_pc1", "p_ti", "p_te",
  "cc_obs_max", "cc_obs_log_recl", "cc_obs_log_recl_frac", "cc_obs_log_city_area"
)], digits = 4)
