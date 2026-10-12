# Original source: 060505_robustness_size_reclamation_choice.r
# Original source location: <LOCAL_CODE_ROOT>/060505_robustness_size_reclamation_choice.r
# Original source SHA-256: 4e16f95f5f73c2394af4dbfdc91063022d52742ce6d8bb176c5f4ae2c6388e02
# Public-safe original snapshot: archive/original_sources/local/060505_robustness_size_reclamation_choice.r
# Classification: EXPLORATORY
# Publication modifications:
# - machine-specific absolute path literals replaced by configured publication_path aliases
# - local encoding decoded to UTF-8; source bytes hashed above
# - comments and historical private machine paths sanitized
# - working-directory calls retained with configured paths; no analyses executed
# Scientific modification: NONE
# This is full historical source, not a certified unattended/reproducible run.
if (!exists("publication_path", mode="function")) stop("Load config/bootstrap.R first")
.libPaths(publication_path("HISTORICAL_PATH_010"))

suppressPackageStartupMessages({
  library(mgcv)
  library(dplyr)
})

workdir <- Sys.getenv("GAM_WORKDIR", unset = publication_path("LOCAL_ANALYSIS_ROOT", "figure/gam_draft"))
input_csv <- Sys.getenv("GAM_INPUT_CSV", unset = file.path(workdir, "gamtest_withpca.csv"))
output_dir <- Sys.getenv("GAM_OUTPUT_DIR", unset = publication_path("RECOVERED_CODE_ROOT", "robustness_size_outputs"))
setwd(workdir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Robustness Test: Size Covariate and Reclamation Representation ===\n")

city_df <- read.csv(input_csv)
city_df$log_city_area <- log1p(city_df$area_km2)
city_df$log_recl <- log1p(city_df$reclamation_area)
city_df$recl_frac <- city_df$reclamation_area / city_df$area_km2
city_df$log_recl_frac <- log1p(city_df$recl_frac)
city_df$Regions <- as.factor(city_df$Regions)

stopifnot(all(c(
  "risk", "log_recl", "log_recl_frac", "area_km2", "log_city_area",
  "anthro_pc1", "climate_pc1", "soil_pc1", "marine_pc1", "geom_pc1", "Regions"
) %in% names(city_df)))

# Bridge index to match the SEM idea of a combined reclamation construct.
recl_pc <- prcomp(
  scale(city_df[, c("log_recl", "log_recl_frac")]),
  center = FALSE,
  scale. = FALSE
)
city_df$recl_combo_pc1 <- recl_pc$x[, 1]
if (cor(city_df$recl_combo_pc1, city_df$log_recl, use = "pairwise.complete.obs") < 0) {
  city_df$recl_combo_pc1 <- -city_df$recl_combo_pc1
  recl_pc$rotation[, 1] <- -recl_pc$rotation[, 1]
}

controls <- paste(
  "s(anthro_pc1, k = 5)",
  "s(climate_pc1, k = 5)",
  "s(soil_pc1, k = 5)",
  "s(marine_pc1, k = 5)",
  "s(geom_pc1, k = 5)",
  "s(Regions, bs = 're')",
  sep = " + "
)

fit_gam <- function(rhs) {
  gam(as.formula(paste("risk ~", rhs)), data = city_df, method = "REML")
}

models <- list(
  size_area = fit_gam(paste(
    "s(log_recl, k = 5)",
    "s(area_km2, k = 5)",
    controls,
    sep = " + "
  )),
  size_logarea = fit_gam(paste(
    "s(log_recl, k = 5)",
    "s(log_city_area, k = 5)",
    controls,
    sep = " + "
  )),
  recl_dual = fit_gam(paste(
    "s(log_recl, k = 5)",
    "s(log_recl_frac, k = 5)",
    controls,
    sep = " + "
  )),
  recl_dual_plus_logarea = fit_gam(paste(
    "s(log_recl, k = 5)",
    "s(log_recl_frac, k = 5)",
    "s(log_city_area, k = 5)",
    controls,
    sep = " + "
  )),
  recl_combo = fit_gam(paste(
    "s(recl_combo_pc1, k = 5)",
    controls,
    sep = " + "
  )),
  recl_combo_plus_logarea = fit_gam(paste(
    "s(recl_combo_pc1, k = 5)",
    "s(log_city_area, k = 5)",
    controls,
    sep = " + "
  )),
  te_area = fit_gam(paste(
    "te(log_recl, area_km2, k = c(5, 5))",
    controls,
    sep = " + "
  )),
  te_logarea = fit_gam(paste(
    "te(log_recl, log_city_area, k = c(5, 5))",
    controls,
    sep = " + "
  ))
)

get_term_stat <- function(model, pattern, stat = "p-value") {
  st <- summary(model)$s.table
  idx <- grep(pattern, rownames(st))
  if (!length(idx)) {
    return(NA_real_)
  }
  unname(st[idx[1], stat])
}

model_summary <- bind_rows(lapply(names(models), function(model_name) {
  mod <- models[[model_name]]
  tibble(
    model = model_name,
    AIC = AIC(mod),
    adj_r2 = summary(mod)$r.sq,
    dev_exp = summary(mod)$dev.expl,
    p_log_recl = get_term_stat(mod, "s\\(log_recl\\)"),
    p_log_recl_frac = get_term_stat(mod, "s\\(log_recl_frac\\)"),
    p_area_km2 = get_term_stat(mod, "s\\(area_km2\\)"),
    p_log_city_area = get_term_stat(mod, "s\\(log_city_area\\)"),
    p_recl_combo_pc1 = get_term_stat(mod, "s\\(recl_combo_pc1\\)"),
    p_te = get_term_stat(mod, "te\\(")
  )
}))

term_table <- bind_rows(lapply(names(models), function(model_name) {
  st <- as.data.frame(summary(models[[model_name]])$s.table)
  st$term <- rownames(st)
  st$model <- model_name
  rownames(st) <- NULL
  st
}))

cor_table <- round(cor(
  city_df[, c("area_km2", "log_city_area", "log_recl", "log_recl_frac", "recl_combo_pc1")],
  use = "pairwise.complete.obs"
), 3)

concurv_dual_plus_logarea <- round(concurvity(models$recl_dual_plus_logarea, full = TRUE), 3)
combo_loadings <- data.frame(
  variable = rownames(recl_pc$rotation),
  loading_pc1 = unname(recl_pc$rotation[, 1]),
  stringsAsFactors = FALSE
)
combo_var_expl <- summary(recl_pc)$importance["Proportion of Variance", 1]

write.csv(model_summary, file.path(output_dir, "robustness_size_model_summary.csv"), row.names = FALSE)
write.csv(term_table, file.path(output_dir, "robustness_size_term_table.csv"), row.names = FALSE)
write.csv(cor_table, file.path(output_dir, "robustness_size_correlation_table.csv"))
write.csv(combo_loadings, file.path(output_dir, "robustness_size_combo_loadings.csv"), row.names = FALSE)
write.csv(concurv_dual_plus_logarea, file.path(output_dir, "robustness_size_dual_plus_logarea_concurvity.csv"))

model_summary <- model_summary |>
  arrange(AIC)

print(model_summary, digits = 4)

cat("\n=== Combined Reclamation Index ===\n")
print(combo_loadings, row.names = FALSE)
cat(sprintf("PC1 variance explained = %.3f\n", combo_var_expl))

cat("\n=== Correlation Table ===\n")
print(cor_table)

cat("\n=== Concurvity: dual + log_city_area ===\n")
print(concurv_dual_plus_logarea)

size_recommendation <- if (
  subset(model_summary, model == "size_logarea")$AIC <
  subset(model_summary, model == "size_area")$AIC
) {
  "Use log_city_area rather than raw area_km2 when a city-size covariate is needed."
} else {
  "Use area_km2 rather than log_city_area when a city-size covariate is needed."
}

main_recl_recommendation <- paste(
  "Keep log_recl + log_recl_frac as the main GAM reclamation specification.",
  "It preserves the absolute-versus-relative decomposition that the GAM can reveal."
)

sem_bridge_recommendation <- paste(
  "If you need a one-index model aligned with SEM, use recl_combo_pc1 + log_city_area as a bridge specification,",
  "not as the main GAM result."
)

interaction_recommendation <- if (
  abs(
    subset(model_summary, model == "te_logarea")$AIC -
      subset(model_summary, model == "te_area")$AIC
  ) < 2
) {
  paste(
    "For the tensor-product interaction, prefer te(log_recl, log_city_area).",
    "Its fit is essentially equivalent to te(log_recl, area_km2),",
    "but the log scale is more stable and easier to interpret."
  )
} else if (
  subset(model_summary, model == "te_logarea")$AIC <
  subset(model_summary, model == "te_area")$AIC
) {
  "For the tensor-product interaction, use te(log_recl, log_city_area)."
} else {
  "For the tensor-product interaction, use te(log_recl, area_km2)."
}

overcontrol_note <- paste(
  "Adding log_city_area on top of log_recl + log_recl_frac is not recommended as the main specification,",
  "because the extra size smooth adds little and introduces strong concurvity."
)

recommendation_lines <- c(
  "FINAL RECOMMENDATION",
  size_recommendation,
  main_recl_recommendation,
  sem_bridge_recommendation,
  interaction_recommendation,
  overcontrol_note
)

writeLines(
  recommendation_lines,
  con = file.path(output_dir, "robustness_size_recommendation.txt"),
  useBytes = TRUE
)

cat("\n=== Final Recommendation ===\n")
cat(paste(recommendation_lines, collapse = "\n"), "\n")
